#### test connect_to_db() ####
source("./R/util.R")

#re-install rJava--may not be needed
#set JAVA variables
# Sys.setenv(JAVA_HOME="/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre",
#            JAVA="/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/bin/java",
#            JAR="/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/../bin/jar",
#            JAVAC="/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/../bin/javac",
#            JAVAH="/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/../bin/javah",
#            JAVA_LIBS="-L/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/lib/amd64/server -L/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/lib/amd64 -L/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/../lib/amd64 -L -L/usr/java/packages/lib/amd64 -L/usr/lib64 -L/lib64 -L/lib -L/usr/lib -ljvm",
#            JAVA_CPPFLAGS="-I/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/../include")
# 
# install.packages("rJava",lib=.libPaths()[1])

#install other required libraries
require_libraries(c("DBI",
                    "magrittr",
                    "tidyr",
                    "dplyr",
                    "stringr"))

#oracle - JDBC
params<-list(  DBMS_type="Oracle",
               driver_type="JDBC",
               remote_CDM=FALSE)

config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db(params$DBMS_type,
                    params$driver_type,
                    config_file)
DBMS_type<-attr(conn,"DBMS_type")
driver_type<-attr(conn,"driver_type")

# test connection
dbGetQuery(conn,"select 'connected!' test_result from dual")


#oracle - OCI
params<-list(  DBMS_type="Oracle",
               driver_type="OCI",
               remote_CDM=FALSE)

config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db(params$DBMS_type,
                    params$driver_type,
                    config_file)
DBMS_type<-attr(conn,"DBMS_type")

# test connection
dbGetQuery(conn,"select 'connected!' test_result from dual")

