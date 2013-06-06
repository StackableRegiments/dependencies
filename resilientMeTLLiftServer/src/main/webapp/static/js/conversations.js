var Conversations = (function(){
    var currentSearchTerm = "";
    var currentlyDisplayedConversations = [];
    var currentConversation = {};
		var currentServerConfigName = "external";
    var currentSlide = 0;
    var targetConversationJid = "";
    var currentTeacherSlide = 0;
    var isSyncedToTeacher = false;
    var shouldRefreshSlideDisplay = function(details){
        return (!("slides" in currentConversation) || "slides" in details && _.any(details,function(slide,slideIndex){
            var ccs = currentConversation.slides[slideIndex];
            if (ccs && "id" in ccs && "id" in slide && "index" in slide && "index" in ccs && ccs.id == slide.id && ccs.index == slide.index){
                return false;
            } else {
                return true;
            }
        }));
    }
		var paintThumbs = function(){
				console.log("firing paintThumbs");
				_.forEach(currentConversation.slides,function(slide){
					possiblyUpdateThumbnail(slide);
				})
    }
    var refreshSlideDisplay = function(){
        updateStatus("Refreshing slide display");
        var slideContainer = $("#slideContainer")
				slideContainer.html(unwrap(currentConversation.slides.sort(function(a,b){return a.index - b.index;}).map(constructSlide))).append(constructAddSlideButton());
				paintThumbs();
				var lazyRepaint = _.debounce(paintThumbs,200);
				slideContainer.on("scroll",lazyRepaint);
				Progress.call("onLayoutUpdated");
		}
    var changeConvToLectureFunction = function(jid){
        if (!jid){
            jid = currentConversation.jid.toString();
        }
        var newPermissions = {"studentCanOpenFriends":false,"studentCanPublish":false,"usersAreCompulsorilySynced":true};
        changePermissionsOfConversation(jid,newPermissions);
    };
    var changeConvToTutorialFunction = function(jid){
        if (!jid){
            jid = currentConversation.jid.toString();
        }
        var newPermissions = {"studentCanOpenFriends":false,"studentCanPublish":true,"usersAreCompulsorilySynced":false};
        changePermissionsOfConversation(jid,newPermissions);
    };
    var enableSyncMoveFunction = function(){
        isSyncedToTeacher = true;
        $("#enableSync").addClass("activePrivacy");
        $("#disableSync").removeClass("activePrivacy");
    };
    var disableSyncMoveFunction = function(){
        isSyncedToTeacher = false;
        $("#enableSync").removeClass("activePrivacy");
        $("#disableSync").addClass("activePrivacy");
    };
    var toggleSyncMoveFunction = function(){
        if (isSyncedToTeacher){
            disableSyncMoveFunction();
        } else {
            enableSyncMoveFunction();
        }
    };
    var getIsSyncedToTeacherDescriptorFunction = function(){
        if (isSyncedToTeacher){
            return "sync on";
        } else {
            return "sync off";
        }
    };
    var getConversationModeDescriptorFunction = function(){
        if (currentConversation && currentConversation.permissions && currentConversation.permissions.studentCanPublish){
            return "collaboration enabled";
        } else {
            return "collaboration disabled";
        }
    };
    var actOnConversationDetails = function(details){
        var oldConversationJid = "";
        if ("jid" in currentConversation){
            oldConversationJid = currentConversation.jid.toString().toLowerCase();
        };
        try{
            updateStatus(sprintf("Updating to conversation %s",details.jid));
            if (details.jid.toString().toLowerCase() == targetConversationJid.toLowerCase()){
                if (shouldDisplayConversationFunction(details)){
                    currentConversation = details;
										if ("configName" in details){
											currentServerConfigName = details.configName;
										}
										currentServerConfigName
                    if (currentConversation.jid.toString().toLowerCase() != oldConversationJid){
                        Progress.call("onConversationJoin");
                    }
                }
                else {
                    currentConversation = {};
                    targetConversationJid = "";
                }
            }
            updateCurrentConversation(details);
            if (!(_.any(currentlyDisplayedConversations,function(c){return c.jid == details.jid;})) && shouldModifyConversationFunction(details)){
                currentlyDisplayedConversations.push(details);
                refreshConversationSearchResults();
            }
        }
        catch(e){
            updateStatus(sprintf("FAILED: ReceiveConversationDetails exception: %s",e));
        }
        Progress.call("onLayoutUpdated");
    };
    var actOnConversations = function(listOfConversations){
        currentlyDisplayedConversations = listOfConversations;
        refreshConversationSearchResults();
    };
    //var onSyncMoveTimerElapsed = undefined;
    var actOnSyncMove = function(jid){
        if ((Conversations.getIsSyncedToTeacher() && !shouldModifyConversationFunction(currentConversation)) || (!UserSettings.getIsInteractive())){
            if ("slides" in currentConversation && currentConversation.slides.filter(function(slide){return slide.id.toString() == jid.toString();}).length > 0){
                WorkQueue.enqueue(function(){
                    performSyncMoveTo(jid);
                    return false;
                });
            }
        }
        /*
         if ((!shouldModifyConversationFunction(currentConversation)) || (!UserSettings.getIsInteractive())){
         if ("slides" in currentConversation && currentConversation.slides.filter(function(slide){return slide.id.toString() == jid.toString();}).length > 0){
         var syncMoveDelay = 3000;
         onSyncMoveTimerElapsed = function(){
         onSyncMoveTimerElapsed = undefined;
         console.log("syncMove moving to",jid,new Date());
         performSyncMoveTo(jid);
         };
         var whenAble = function(){
         console.log("syncMove polling for",jid,new Date());
         if (onSyncMoveTimerElapsed){
         if (WorkQueue.isAbleToWork){
         onSyncMoveTimerElapsed();
         } else {
         WorkQueue.enqueue(function(){
         console.log("syncMove re-establishing for",jid,new Date());
         setTimeout(whenAble,syncMoveDelay);
         });
         }
         }
         };
         WorkQueue.enqueue(function(){
         console.log("syncMove establishing for",jid,new Date());
         setTimeout(whenAble,syncMoveDelay);
         });
         }
         }
         */
    }
    var performSyncMoveTo = function(jid){
        if ((!shouldModifyConversationFunction(currentConversation)) || (!UserSettings.getIsInteractive())){
            if ("slides" in currentConversation && currentConversation.slides.filter(function(slide){return slide.id.toString() == jid.toString();}).length > 0){
                currentTeacherSlide = jid;
                if (Conversations.getIsSyncedToTeacher()){
                    if (currentSlide != jid){
                        console.log("syncMove moving to",jid);
                        currentSlide = jid;
                        doMoveToSlide(jid);
                    }
                }
            }
        }
    };
    var updateThumbnailFor = function(slideId) {
				//setting index to zero because this isn't necessary.
				possiblyUpdateThumbnail({id:slideId,index:0});
    }
    var goToNextSlideFunction = function(){
        if ("slides" in currentConversation && currentSlide > 0){
            var curr = _.find(currentConversation.slides,function(s){return s.id == currentSlide;});
            var next = _.find(currentConversation.slides,function(s){return s.index == (curr.index + 1);});
            if (next != undefined && "id" in next){
                doMoveToSlide(next.id.toString());
            }
        }
    };
    var goToPrevSlideFunction = function(){
        if ("slides" in currentConversation && currentSlide > 0){
            var curr = _.find(currentConversation.slides,function(s){return s.id == currentSlide;});
            var next = _.find(currentConversation.slides,function(s){return s.index == (curr.index - 1);});
            if (next != undefined && "id" in next){
                doMoveToSlide(next.id.toString());
            }
        }
    };
    var actOnNewConversationDetailsReceived = function(details){
        if (details.title.indexOf(currentSearchTerm) > -1 || details.author.indexOf(currentSearchTerm) > -1){
            currentlyDisplayedConversations = _.filter(currentlyDisplayedConversations,function(c){return c.jid != details.jid});
            currentlyDisplayedConversations.push(details);
            refreshConversationSearchResults();
        }
    };
    var actOnCurrentConversationJidReceived = function(jid){
        targetConversationJid = jid;
        updateLinks();
    };
    var actOnCurrentSlideJidReceived = function(jid){
        currentSlide = jid;
        indicateActiveSlide(jid);
        updateLinks();
    };
    var updateLinks = function(){
        var serviceUrlRoot = window.location.origin;
        var shareUrl = sprintf("/join?conversation=%s&slide=%s",targetConversationJid,currentSlide);
        var projectorUrl = sprintf("/projector/%s",targetConversationJid);
        $("#shareLink").html($("<a/>",{
            href:shareUrl,
            text:serviceUrlRoot + shareUrl
        }));
        $("#projectorLink").html($("<a/>",{
            href:projectorUrl,
            text:serviceUrlRoot + projectorUrl
        })).on("click",function(){
            var el = document.documentElement, rfs =
                    el.requestFullScreen
                    || el.webkitRequestFullScreen
                    || el.mozRequestFullScreen;
            rfs.call(el);

            DeviceConfiguration.setCurrentDevice("projector");
            return false;
        });
        //console.log(shareUrl,projectorUrl,$("#shareLink").text(),$("#projectorLink").text());
    };
    var updateCurrentConversation = function(details){
        if (details.jid == currentConversation.jid){
            updateConversationHeader();
            updateLinks();
            if (shouldRefreshSlideDisplay(details)){
                refreshSlideDisplay();
            }
        }
        updateCurrentlyDisplayedConversations(details);
    };
    var updateCurrentlyDisplayedConversations = function(details){
        currentlyDisplayedConversations = currentlyDisplayedConversations.map(
            function(conv){
                if (conv.jid == details.jid){
                    return details;
                } else {
                    return conv;
                }
            }
        )
        refreshConversationSearchResults();
    };

    var shouldModifyConversationFunction = function(conversation){
        if (!conversation){
            conversation = currentConversation;
        }
        if ("author" in conversation && conversation.author.toLowerCase() == UserSettings.getUsername().toLowerCase()){
            return true;
        } else {
            return false;
        }
    };
    var shouldDisplayConversationFunction = function(conversation){
        if (!conversation){
            conversation = currentConversation;
        }
        if ("subject" in conversation && conversation.subject.toLowerCase() != "deleted" && (("author" in conversation && conversation.author == UserSettings.getUsername()) || _.any(UserSettings.getUserGroups(), function(group){
            return group.value.toLowerCase() == conversation.subject.toLowerCase();
        }))) {
            return true;
        } else {
            return false;
        }
    };
    var shouldPublishInConversationFunction = function(conversation){
        if (!conversation){
            conversation = currentConversation;
        }
        if("permissions" in conversation && "studentCanPublish" in conversation.permissions && (shouldModifyConversationFunction(conversation) || conversation.permissions.studentCanPublish)){
            return true;
        } else {
            return false;
        }
    };
    var refreshConversationSearchResults = function(){
        try {
            var convs = _.sortBy(currentlyDisplayedConversations.filter(function(conv){
                return shouldDisplayConversationFunction(conv);
            }),function(conv){return new Date(conv.created);}).reverse().map(constructConversation);
            var searchResults = $("#searchResults");
            if (_.size(convs) > 0){
                searchResults.html(unwrap(convs));
            }   else {
                searchResults.html($("<div/>",{
                    text:"No search results found"
                }));
            }
        } catch(e){
            console.log("refreshConversationSearchResults",e);
        }
    };
    var constructAddSlideButton = function(){
        if (shouldModifyConversationFunction()){
            return $("<div/>",{
                id: "addSlideButton",
                class:"toolbar",
                name: "addSlideButton",
                type: "button"
            }).append($("<span>Add Slide</span>")).on("click",function(){
                var currentJid = currentConversation.jid;
                var currentSlideIndex = currentConversation.slides.filter(function(slide){return slide.id == currentSlide;})[0].index;
                var newIndex = currentSlideIndex + 1;
                addSlideToConversationAtIndex(currentConversation.jid,newIndex);
                Progress.conversationDetailsReceived["JoinAtIndexIfAvailable"] = function(incomingDetails){
                    if ("jid" in incomingDetails && incomingDetails.jid == currentJid){
                        if ("slides" in incomingDetails){
                            var newSlide = _.find(incomingDetails.slides,function(s){
                                return s.index == newIndex && s.id != currentSlide;
                            });
                            doMoveToSlide(newSlide.id.toString());
                        }
                    }
                };
            });
        } else {
            return $("<div/>");
        }
    }
    var doMoveToSlide = function(slideId){
        delete Progress.conversationDetailsReceived["JoinAtIndexIfAvailable"];
        WorkQueue.enqueue(function(){
	    console.log("doMoveToslide",slideId);
            indicateActiveSlide(slideId);
            loadSlide(slideId);
            return true;
        });
    };
    var indicateActiveSlide = function(slideId){
        $(".slideButtonContainer").removeClass("activeSlide");
        $(sprintf("#slideContainer_%s",slideId)).addClass("activeSlide");
    };
		var possiblyUpdateThumbnail = function(slide){
			var slidesContainer = $("#slideContainer");
			var slidesTop = slidesContainer.scrollTop() + slidesContainer.offset().top;
			var slidesBottom = slidesTop + slidesContainer.height();
			var slideContainer = $(sprintf("#slideButton_%s",slide.id));
			var slideTop = slideContainer.offset().top;
			var slideBottom = slideTop + slideContainer.height();
			if ((slideBottom >= slidesTop) && (slideTop <= slidesBottom)
				&& (slideBottom <= slidesBottom) && (slideTop >= slidesTop) 
				){
				console.log("rendering within view",slide.id,slide);
				var currentSrc = slideContainer.attr("src");
				if (currentSrc == undefined || currentSrc.indexOf("/thumbnail/") != 0){
					slideContainer.attr("src",sprintf("/thumbnail/%s/%s?nocache=%s",currentServerConfigName,slide.id,Date.now()));
				}
			}
		}
    var constructSlide = function(slide){
        var slideIndex = slide.index + 1;
        var newSlide = $("<div/>",{
            id: sprintf("slideContainer_%s",slide.id),
            class:"slideButtonContainer"
        }).css({
						height:"75px",
						width:"100px",
						margin:"10px"
				});
        $("<img/>",{
            id: sprintf("slideButton_%s",slide.id),
        //    src:sprintf("/thumbnail/madam/%s?nocache=%s",slide.id,Date.now()),
            class:"thumbnail",
            alt:sprintf("Slide %s",slideIndex),
            title:sprintf("Slide %s (%s)",slideIndex,slide.id)
        }).on("click",function(){
            disableSyncMoveFunction();
            doMoveToSlide(slide.id.toString());
        }).appendTo(newSlide);
        $("<span/>",{
            text: sprintf("%s/%s",slideIndex,currentConversation.slides.length),
            class: "slideThumbnailNumber"
        }).appendTo($("<div/>").appendTo(newSlide));
        return newSlide;
    }
    var constructConversation = function(conversation){
        var uniq = function(name){
            return sprintf("%s_%s",name,conversation.jid);
        };
        var jidString = conversation.jid.toString();
        var deleteSpan = $("<span>Delete</span>")
        var renameSpan = $("<span>Rename</span>")
        var sharingSpan = $("<span>Sharing</span>")
        var newConv = $("<div/>",{
            id: uniq("conversation"),
            class:"searchResult"
        }).on("click",function(e){
            var id1 = e.target.parentElement.id;
            var id2 = e.target.parentElement.parentElement.id;
            if(id1 ==uniq("extraConversationTools") || id2==uniq("extraConversationTools")) return;
            targetConversationJid = jidString;
            var firstSlide = conversation.slides.filter(function(slide){return slide.index == 0;})[0];
            hideBackstage();
            doMoveToSlide(firstSlide.id.toString());
        });
        var jidString = conversation.jid.toString();
        var row1 = $("<div/>");
        var row2 = $("<div/>",{
            class:"middleRow",
        });
        var row3 = $("<div/>",{
            id:uniq("extraConversationTools"),
            class: "extraConversationTools"
        });

        var convTitle = $("<span/>",{
            id: uniq("conversationTitle"),
            class: "conversationTitle",
            text: conversation.title
        });
        var convAuthor = $("<span/>",{
            class:"conversationAuthor",
            text: sprintf("by %s",conversation.author)
        });
        var convSubject = $("<span/>",{
            class:"conversationSubject",
            text:sprintf("Restricted to %s",conversation.subject)
        });
        var convCreated = $("<span/>",{
            class:"conversationCreated",
            text:sprintf("Created on %s",conversation.created)
        });
        var joinConvButton = $("<div/>", {
            id: uniq("conversationJoin"),
            class: "conversationJoinButton conversationSearchButton",
            name: uniq("conversationJoin"),
            type: "button"
        }).on("click",function(){
            targetConversationJid = jidString;
            var firstSlide = conversation.slides.filter(function(slide){return slide.index == 0;})[0];
            hideBackstage();
            doMoveToSlide(firstSlide.id.toString());
        }).append("<span>join</span>");
        var renameConvButton = $("<div/>", {
            id: uniq("conversationRenameSubmit"),
            class: "conversationSearchButton",
            name: uniq("conversationRenameSubmit"),
            type: "button"
        }).on("click",function(){requestRenameConversationDialogue(jidString);}).append(renameSpan);
        var changeSharingButton = $("<div/>", {
            id: uniq("conversationChangeSubjectSubmit"),
            name: uniq("conversationChangeSubjectSubmit"),
            class: "conversationSearchButton",
            type: "button"
        }).on("click",function(){requestChangeSubjectOfConversationDialogue(jidString);}).append(sharingSpan);
        var deleteConvButton = $("<div/>", {
            id: uniq("conversationDelete"),
            class: "conversationSearchButton",
            name: uniq("conversationDelete"),
            type: "button"
        }).on("click",function(){
            requestDeleteConversationDialogue(jidString);
        }).append(deleteSpan);
        newConv.append(row1.append(convTitle));
        newConv.append(row2.append(convAuthor).append(convSubject).append(convCreated));
        if (shouldModifyConversationFunction(conversation)){
            newConv.append(row3.append(renameConvButton).append(changeSharingButton).append(deleteConvButton));
        }
        if ("jid" in conversation && targetConversationJid.trim().toLowerCase() == conversation.jid.toString().trim().toLowerCase()){
            newConv.addClass("activeConversation");
        }
        return newConv;
    }
    Progress.newConversationDetailsReceived["Conversations"] = actOnNewConversationDetailsReceived;
    Progress.conversationsReceived["Conversations"] = actOnConversations;
    Progress.syncMoveReceived["Conversations"] = actOnSyncMove;
    Progress.conversationDetailsReceived["Conversations"] = actOnConversationDetails;
    //    Progress.onConversationJoin["Conversations"] = refreshSlideDisplay;
    Progress.currentSlideJidReceived["Conversations"] = actOnCurrentSlideJidReceived;
    Progress.currentConversationJidReceived["Conversations"] = actOnCurrentConversationJidReceived;
		//Progress.onLayoutUpdated["Conversations"] = paintThumbs;
    $(function(){
        $("#conversations").click(function(){
            showBackstage("conversations");
        });
        $("<div/>", {
            id:"createConversationButton",
            class: "conversationSearchButton",
            name:"createConversationButton",
            type:"button"
        }).append($("<span/>",{text:"Create Conversation"})).on("click",function(){
            createConversation(sprintf("%s created on %s",UserSettings.getUsername(),Date()));
        }).appendTo("#createConversationContainer");
        $("<div/>", {
            id:"myConversationsButton",
            class: "conversationSearchButton",
            type:"button",
        }).append($("<span/>",{text:"My Conversations"})).on("click",function(){
            getSearchResult(UserSettings.getUsername());
        }).appendTo("#createConversationContainer");
        $("<div/>", {
            id:"searchButton",
            class: "conversationSearchButton",
            name:"searchButton",
            type: "button"
        }).append($("<span/>", {text: "Search"})).on("click",function(){
            getSearchResult(currentSearchTerm);
        }).appendTo("#searchButtonContainer");
        var updateSearchTerm = function(e){
            currentSearchTerm = this.value;
            if (e.which == 13){
                e.stopPropagation();
                getSearchResult(currentSearchTerm);
            }
        };
        $("<input/>", {
            id:"searchForConversationBox",
            name:"searchForConversationBox",
            blur:updateSearchTerm,
            change:updateSearchTerm,
            focus:updateSearchTerm,
            keydown:updateSearchTerm,
            select:updateSearchTerm
        }).appendTo("#searchForConversationBoxContainer");
        $("<span />",{
            text:"share",
            id:"shareButton"
        }).on("click",function(){
            $("#shareContainer").toggle();
            updateLinks();
        }).appendTo($("#shareButton"));
        $("#closeSharingButton").on("click", function() {
            $("#shareContainer").toggle();
        });
    });
    return {
        inConversation:function(){
            return Conversations.getCurrentConversationJid().length > 0;
        },
        isAuthor:function(){
            if(!Conversations.inConversation()){
                return false;
            }
            return UserSettings.getUsername() == Conversations.getCurrentConversation().author;
        },
        getCurrentTeacherSlide : function(){return currentTeacherSlide;},
        getCurrentSlideJid : function(){return currentSlide;},
        getCurrentSlide : function(){return _.find(currentConversation.slides,function(i){return i.id.toString() == currentSlide.toString();})},
        getCurrentConversationJid : function(){
            if ("jid" in currentConversation){
                return currentConversation.jid.toString();
            } else return targetConversationJid;
        },
        getCurrentConversation : function(){return currentConversation;},
        getIsSyncedToTeacher : function(){return isSyncedToTeacher;},
        getIsSyncedToTeacherDescriptor : getIsSyncedToTeacherDescriptorFunction,
        getConversationModeDescriptor : getConversationModeDescriptorFunction,
        enableSyncMove : enableSyncMoveFunction,
        disableSyncMove : disableSyncMoveFunction,
        toggleSyncMove : toggleSyncMoveFunction,
        changeConversationToTutorial : changeConvToTutorialFunction,
        changeConversationToLecture : changeConvToLectureFunction,
        shouldDisplayConversation : shouldDisplayConversationFunction,
        shouldPublishInConversation : shouldPublishInConversationFunction,
        shouldModifyConversation : shouldModifyConversationFunction,
        goToNextSlide : goToNextSlideFunction,
        goToPrevSlide : goToPrevSlideFunction,
        updateThumbnail :updateThumbnailFor
    };
})();

function unwrap(jqs){
    return _.pluck(jqs,"0");
}
function receiveCurrentSlide(jid){
    Progress.call("currentSlideJidReceived",[jid]);
}
function receiveCurrentConversation(jid){
    Progress.call("currentConversationJidReceived",[jid]);
}
function receiveConversationDetails(details){
    Progress.call("conversationDetailsReceived",[details]);
}
function receiveSyncMove(jid){
    if(Conversations.getIsSyncedToTeacher()){
        Progress.call("syncMoveReceived",[jid]);
    }
}
function receiveNewConversationDetails(details){
    Progress.call("newConversationDetailsReceived",[details]);
}
function receiveConversations(listOfConversations){
    Progress.call("conversationsReceived",[listOfConversations]);
}
// these will be injected by lift
//function moveToSlide(jid)
//function joinConversation(jid)
//function getConversation()
//function leaveConversation(jid)
//function getSearchResult(query)
//function createConversation(title)
//function deleteConversation(jid)
//function renameConversation(jid,newTitle)
//function changePermissions(jid,newPermissions)
//function changeSubject(jid,newSubject)
//function addSlide(jid,indexOfNewSlide)
//function reorderSlides(jid,alteredSlides)
