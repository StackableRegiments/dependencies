function setupStatus(){
    pending = {};
    var display = $("#strokesPending");
    var latency = $("#latency");
    var recentLatency = 0;
    var progressFuncs = {};
    var cancelFuncs = {};
    window.updateStrokesPending = function(delta,identity){
        if(delta > 0){
            pending[identity] = Date.now();
        }
        else if(identity in pending){
            recentLatency = Date.now() - pending[identity];
            delete pending[identity];
        }
        display.text(Object.keys(pending).length);
        latency.text(recentLatency);
    }
    window.updateTracking = function(id,progressFunc,cancelFunc){
        if(progressFunc){
            progressFuncs[id] = progressFunc;
        }
        if(cancelFunc){
            cancelFuncs[id] = cancelFunc;
        }
        else if(id in progressFuncs){
            progressFuncs[id]();
        }
        else{
            console.log("No progress initializer function was issued for ",id);
        }
    }
    window.stopTracking = function(id){
        if(id in cancelFuncs){
            cancelFuncs[id]();
        }
        delete progressFuncs[id];
        delete cancelFuncs[id];
    }
}
function strokeCollected(spoints){
    if(spoints.length > 0){
        var points = spoints.split(" ").map(function(p){
            return parseFloat(p);
        });

        var currentSlide = Conversations.getCurrentSlideJid();
        var ink = {
            thickness : Modes.draw.drawingAttributes.width,
            color:[Modes.draw.drawingAttributes.color,255],
            type:"ink",
            author:UserSettings.getUsername(),
            timestamp:Date.now(),
            target:"presentationSpace",
            privacy:Privacy.getCurrentPrivacy(),
            slide:currentSlide.toString(),
            isHighlighter:Modes.draw.drawingAttributes.isHighlighter
        };
        var scaledPoints = [];
        var x;
        var y;
        var worldPos;
        for(var p = 0; p < points.length; p += 3){
            x = points[p];
            y = points[p+1];
            worldPos = screenToWorld(x,y);
            scaledPoints = scaledPoints.concat([worldPos.x,worldPos.y,points[p+2]]);
        }
        ink.points = scaledPoints;
        ink.checksum = ink.points.reduce(function(a,b){return a+b},0);
        ink.startingSum = ink.checksum;
        ink.identity = ink.checksum.toFixed(1);
        calculateInkBounds(ink);
        if(ink.isHighlighter){
            boardContent.highlighters[ink.identity] = ink;
        }
        else{
            boardContent.inks[ink.identity] = ink;
        }
        sendInk(ink);
    }
}
function batchTransform(){

    var currentSlide = Conversations.getCurrentSlideJid();
    return {
        type:"moveDelta",
        identity:Date.now().toString(),
        author:UserSettings.getUsername(),
        slide:currentSlide.toString(),
        target:"presentationSpace",
        privacy:Privacy.getCurrentPrivacy(),
        timestamp:Date.now(),
        inkIds:[],
        textIds:[],
        imageIds:[],
        xTranslate:0,
        yTranslate:0,
        xScale:1.0,
        yScale:1.0,
        isDeleted:false,
        newPrivacy:"not_set"
    }
}
function sendDirtyInk(ink){
    var currentSlide = Conversations.getCurrentSlideJid();
    sendStanza({
        type:"dirtyInk",
        identity:ink.identity,
        author:UserSettings.getUsername(),
        timestamp:Date.now(),
        slide:currentSlide.toString(),
        target:"presentationSpace",
        privacy:ink.privacy
    });
}
function sendInk(ink){
    updateStrokesPending(1,ink.identity);
    sendStanza(ink);
}
var stanzaHandlers = {
    ink:inkReceived,
    dirtyInk:dirtyInkReceived,
    move:moveReceived,
    moveDelta:transformReceived,
    image:imageReceived,
    text:textReceived,
    command:commandReceived,
    submission:submissionReceived
};
function submissionReceived(submission){
    Submissions.processSubmission(submission);
}
function commandReceived(c){
    if(c.command == "/TEACHER_VIEW_MOVED"){
        if(c.parameters[5] != Conversations.getCurrentSlideJid()){
            return;
        }
        var ps = c.parameters.map(parseFloat);
        if(_.any(ps,isNaN)){
            console.log("Can't follow teacher to",c);
            return;
        }
        if(ps[4] == DeviceConfiguration.getIdentity()){
            return;
        }
        if(Conversations.getIsSyncedToTeacher()){
            var f = function(){
                console.log("syncing to teacher view");
                zoomToPage();
                TweenController.zoomAndPanViewbox(ps[0],ps[1],ps[2],ps[3],function(){},false,true);
            };
            if(UserSettings.getIsInteractive()){
                WorkQueue.enqueue(f);
            }
            else{
                f();
            }
        }
        else{
        }
    }
}
function textReceived(t){
    try{
        console.log(sprintf("textReceived [%s]",t.text),t);
        if(isUsable(t)){
            boardContent.texts[t.identity] = t;
            prerenderText(t);
            incorporateBoardBounds(t.bounds);
            WorkQueue.enqueue(function(){
                if(isInClearSpace(t.bounds)){
                    drawText(t);
                    return false;
                }
                else{
                    return true;
                }
            });
        }
        else{
            if(t.identity in boardContent.texts){
                delete boardContent.texts[t.identity];
            }
        }
    }
    catch(e){
        console.log("textReceived exception:",e);
    }
}
function receiveMeTLStanza(stanza){
    Progress.call("stanzaReceived",[stanza]);
}
function actOnReceivedStanza(stanza){
    try{
        if(stanza.type in stanzaHandlers){
            stanzaHandlers[stanza.type](stanza);
            Progress.call("onBoardContentChanged");
        }
        else{
            console.log(sprintf("Unknown stanza: %s %s",stanza.type,stanza));
        }
    }
    catch(e){
        console.log("Exception in receiveMeTLStanza",e,stanza);
    }
}
function transformReceived(transform){
    var op = "";
    if(transform.newPrivacy != "not_set"){
        var p = transform.newPrivacy;
        op += "Became "+p;
        var setPrivacy = function(ink){
            if(ink){
                ink.privacy = p;
            }
        }
        $.each(transform.inkIds,function(i,id){
            setPrivacy(boardContent.inks[id]);
            setPrivacy(boardContent.highlighters[id]);
        });
        $.each(transform.imageIds,function(i,id){
            boardContent.images[id].privacy = p;
        });
        $.each(transform.textIds,function(i,id){
            boardContent.texts[id].privacy = p;
        });
    }
    if(transform.isDeleted){
        op += "deleted";
        var p = transform.privacy;
        $.each(transform.inkIds,function(i,id){
            deleteInk("highlighters",p,id);
            deleteInk("inks",p,id);
        });
        $.each(transform.imageIds,function(i,id){
            deleteImage(p,id);
        });
        $.each(transform.textIds,function(i,id){
            deleteText(p,id);
        });
    }
    if(transform.xTranslate || transform.yTranslate){
        var deltaX = transform.xTranslate;
        var deltaY = transform.yTranslate;
        op += sprintf("translate (%s,%s)",deltaX,deltaY);
        var translateInk = function(ink){
            if(ink){
                var ps = ink.points;
                for(var p = 0; p < ps.length; p += 3){
                    ps[p] += deltaX;
                    ps[p+1] += deltaY;
                }
                calculateInkBounds(ink);
            }
        }
        $.each(transform.inkIds,function(i,id){
            translateInk(boardContent.inks[id]);
            translateInk(boardContent.highlighters[id]);
        });
        $.each(transform.imageIds,function(i,id){
            var image = boardContent.images[id];
            image.x += transform.xTranslate;
            image.y += transform.yTranslate;
            calculateImageBounds(image);
        });
        $.each(transform.textIds,function(i,id){
            var text = boardContent.texts[id];
            text.x += transform.xTranslate;
            text.y += transform.yTranslate;
            calculateTextBounds(text);
        });
    }
    if(transform.xScale != 1 || transform.yScale != 1){
        op += sprintf("scale (%s,%s)",transform.xScale,transform.yScale);
        var transformInk = function(ink){
            if(ink){
                var ps = ink.points;
                var xPos = ink.bounds[0];
                var yPos = ink.bounds[1];
                var xp, yp;
                for(var p = 0; p < ps.length; p += 3){
                    xp = ps[p] - xPos;
                    yp = ps[p + 1] - yPos;
                    ps[p] = xPos + xp * transform.xScale;
                    ps[p+1] = yPos + yp * transform.yScale;
                }
                calculateInkBounds(ink);
            }
        }
        $.each(transform.inkIds,function(i,id){
            transformInk(boardContent.inks[id]);
            transformInk(boardContent.highlighters[id]);
        });
        $.each(transform.imageIds,function(i,id){
            var image = boardContent.images[id];
            image.width = image.width * transform.xScale;
            image.height = image.height * transform.yScale;
            calculateImageBounds(image);
        });
        $.each(transform.textIds,function(i,id){
            var text = boardContent.texts[id];
            text.width = text.width * transform.xScale;
            text.height = text.height * transform.yScale;
            text.size = text.size * transform.yScale;
            text.font = sprintf("%spx %s",text.size,text.family);
            if(isUsable(text)){
                prerenderText(text);
            }
            else{
                if(text.identity in boardContent.texts){
                    delete boardContent.texts[text.identity];
                }
            }
        });
    }
    updateStatus(sprintf("%s %s %s %s",
                         op,
                         transform.imageIds.length,
                         transform.textIds.length,
                         transform.inkIds.length));
    blit();
}
function moveReceived(move){
    console.log("moveReceived",move);
    updateStatus(sprintf("Moving %s, %s, %s",
                         Object.keys(move.images).length,
                         Object.keys(move.texts).length,
                         Object.keys(move.inks).length));
    $.each(move.inks,function(id,ink){
        boardContent.inks[id] = ink;
    });
    $.each(move.images,function(id,image){
        boardContent.images[id] = image;
    });
    $.each(move.texts,function(id,text){
        boardContent.texts[id] = text;
    });
    blit();
}
function deleteInk(inks,privacy,id){
    if(id in boardContent[inks]){
        var ink = boardContent[inks][id];
        if(ink.privacy.toUpperCase() == privacy.toUpperCase()){
            console.log("Deleting ink",inks,id);
            delete boardContent[inks][id];
        }
    }
}
function deleteImage(privacy,id){
    var image = boardContent.images[id];
    if(image.privacy.toUpperCase() == privacy.toUpperCase()){
        delete boardContent.images[id];
    }
}
function deleteText(privacy,id){
    var text = boardContent.texts[id];
    if(text.privacy.toUpperCase() == privacy.toUpperCase()){
        delete boardContent.texts[id];
    }
}
function dirtyInkReceived(dirtyInk){
    var id = dirtyInk.identity;
    var deletePrivacy = dirtyInk.privacy;
    deleteInk("highlighters",deletePrivacy,id);
    deleteInk("inks",deletePrivacy,id);
    updateStatus(sprintf("Deleted ink %s",id));
    blit();
}
function isInClearSpace(bounds){
    return !_.any(visibleBounds,function(onscreenElement){
        return intersectRect(onscreenElement,bounds);
    });
}
function screenBounds(worldBounds){
    var screenPos = worldToScreen(worldBounds[0],worldBounds[1]);
    var screenLimit = worldToScreen(worldBounds[2],worldBounds[3]);
    var screenWidth = screenLimit.x - screenPos.x;
    var screenHeight = screenLimit.y - screenPos.y;
    return {
        screenPos:screenPos,
        screenLimit:screenLimit,
        screenWidth:screenWidth,
        screenHeight:screenHeight
    };
}
function drawImage(image){
    try{
        var sBounds = screenBounds(image.bounds);
        visibleBounds.push(image.bounds);
        var borderW = sBounds.screenWidth * 0.10;
        var borderH = sBounds.screenHeight * 0.10;
        boardContext.drawImage(image.canvas, sBounds.screenPos.x - (borderW / 2), sBounds.screenPos.y - (borderH / 2), sBounds.screenWidth + borderW ,sBounds.screenHeight + borderH);
    }
    catch(e){
        console.log("drawImage exception",e);
    }
}
function drawText(text){
    try{
        var sBounds = screenBounds(text.bounds);
        visibleBounds.push(text.bounds);
        boardContext.drawImage(text.canvas,sBounds.screenPos.x,sBounds.screenPos.y,sBounds.screenWidth,sBounds.screenHeight);
    }
    catch(e){
        console.log("drawText exception",e);
    }
}
function drawInk(ink){
    var sBounds = screenBounds(ink.bounds);
    visibleBounds.push(ink.bounds);
    boardContext.drawImage(ink.canvas,
                           sBounds.screenPos.x,sBounds.screenPos.y,
                           sBounds.screenWidth,sBounds.screenHeight);
}
function imageReceived(image){
    var dataImage = new Image();
    image.imageData = dataImage;
    dataImage.onload = function(){
        if(image.width == 0){
            image.width = dataImage.naturalWidth;
        }
        if(image.height == 0){
            image.height = dataImage.naturalHeight;
        }
        image.bounds = [image.x,image.y,image.x+image.width,image.y+image.height];
        incorporateBoardBounds(image.bounds);
        console.log("imageReceived",image);
        boardContent.images[image.identity]  = image;
        updateTracking(image.identity);
        prerenderImage(image);
        WorkQueue.enqueue(function(){
            if(isInClearSpace(image.bounds)){
                console.log("Drawing image in clear space");
                try {
                    drawImage(image);
                } catch(e){
                    console.log("drawImage exception",e);
                }
                return false;
            }
            else{
                console.log("Rerendering image in contested space");
                return true;
            }
        });
    }
    dataImage.src = calculateImageSource(image);
}
function inkReceived(ink){
    calculateInkBounds(ink);
    updateStrokesPending(-1,ink.identity);
    if(prerenderInk(ink)){
        incorporateBoardBounds(ink.bounds);
        if(ink.isHighlighter){
            boardContent.highlighters[ink.identity] = ink;
        }
        else{
            boardContent.inks[ink.identity] = ink;
        }
        WorkQueue.enqueue(function(){
            if(isInClearSpace(ink.bounds)){
                drawInk(ink);
                return false;
            }
            else{
                return true;
            }
        });
    }
}
function zoomToFit(){
    Progress.onBoardContentChanged.autoZooming = zoomToFit;
    requestedViewboxWidth = boardContent.width;
    requestedViewboxHeight = boardContent.height;
    IncludeView.specific(boardContent.minX,boardContent.minY,boardContent.width,boardContent.height);
}
function zoomToOriginal(){
    delete Progress.onBoardContentChanged.autoZooming;
    var oldReqVBH = requestedViewboxHeight;
    var oldReqVBW = requestedViewboxWidth;
    requestedViewboxWidth = boardWidth;
    requestedViewboxHeight = boardHeight;
    IncludeView.specific(0,0,boardWidth,boardHeight);
}
function zoomToPage(){
    delete Progress.onBoardContentChanged.autoZooming;
    var oldReqVBH = requestedViewboxHeight;
    var oldReqVBW = requestedViewboxWidth;
    requestedViewboxWidth = boardWidth;
    requestedViewboxHeight = boardHeight;
    var xPos = viewboxX + ((oldReqVBW - requestedViewboxWidth) / 2);
    var yPos = viewboxY + ((oldReqVBH - requestedViewboxHeight) / 2);
    IncludeView.specific(xPos,yPos,boardWidth,boardHeight);
}
function receiveS2C(id,markup){
    try{
        var m = $(unescape(markup));
        m.addClass("s2cMessage").appendTo("body");
    }
    catch(e){
        console.log("receiveS2C exception:",e);
    }
}
