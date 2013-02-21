#! /bin/sh

#to connect to a single new metl server backend
cd /srv/MeTLAuxiliaryServer
ulimit -u 4096
mvn jetty:run -Djetty.port=8080 -Dmetl.backend=standalone -Drun.mode=production 1>log/output.log 2>&1 & 

#while debugging
#mvn jetty:run -Djetty.port=8080 -Dmetl.backend=standalone -Drun.mode=production -Dmetl.stopwatch.minimumLog=10 -Dmetl.stopwatch.enabled=true 1>log/output.log 2>&1 &

#to use the internal H2 system
#mvn jetty:run -Djetty.port=8080 -Dmetl.backend=external -Drun.mode=production 1>log/output.log 2>&1 &

#to use the legacy monash MeTL2011 system
#mvn jetty:run -Djetty.port=8080 1>log/output.log -Drun.mode=production 2>&1 &
