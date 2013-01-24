#! /bin/sh

#to connect to a single new metl server backend
mvn jetty:run -Djetty.port=8088 -Dmetl.backend=standalone &

#to use the internal H2 system
#mvn jetty:run -Djetty.port=8088 -Dmetl.backend=external &

#to use the legacy monash MeTL2011 system
#mvn jetty:run -Djetty.port=8088 &
