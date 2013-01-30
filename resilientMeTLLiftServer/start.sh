#! /bin/sh

#to connect to a single new metl server backend
mvn jetty:run -Djetty.port=8088 -Dmetl.backend=standalone -Drun.mode=production &

#to use the internal H2 system
#mvn jetty:run -Djetty.port=8088 -Dmetl.backend=external -Drun.mode=production &

#to use the legacy monash MeTL2011 system
#mvn jetty:run -Djetty.port=8088 -Drun.mode=production &
