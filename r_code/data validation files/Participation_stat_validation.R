#
#This R-script make a series of cumulative temporal plots
#

#clear memory 
rm( list=ls() ) 

#set seed
set.seed(9)

#load necessary packages
library(tm)
library(foreign)
library(base)
library(lda)
library(topicmodels)
library(Matrix)
library(plyr)
library(doBy)
library(TTR)
library(stats)
library(RColorBrewer)

#set working directory
setwd(".../applications/") 


################################################################
#######################Read In Data#############################
################################################################

#read in cleaned data
cops.data<-read.csv("cops.cleaned.csv",header=TRUE, row.names=NULL)
precops.data<-read.csv("precops.cleaned.csv",header=TRUE, row.names=NULL)

#combine
complete.data<-rbind(cops.data,precops.data)

#create timecounter
complete.data$timecounter<-ifelse(grepl("COP",complete.data$Meeting),(as.numeric(gsub("COP ","",complete.data$Meeting))+11),NA)
complete.data$timecounter<-ifelse(grepl("INC",complete.data$Meeting),(as.numeric(gsub("INC ","",complete.data$Meeting))),complete.data$timecounter)

#############################################
###########Group Over Time Plot##############
#############################################

#retain relevant variables
complete.data<-complete.data[,c("timecounter","IGO","NGO","Observer","Party","IO")]
complete.data<-na.omit(complete.data)

#collapse variables to get topic freqencies at each point in time
collapse1 <- summaryBy(IGO+NGO+Observer+Party+IO~timecounter, FUN=c(sum), data=complete.data)

#assign column names
colnames(collapse1)<-c("timecounter","IGO","NGO","Observer","Party","IO")

#create summed measures
collapse1$TOPIC0SUM<-rep(0,length(collapse1$IGO))
collapse1$IGOSUM<-collapse1$IGO
collapse1$NGOSUM<-collapse1$IGO+collapse1$NGO
collapse1$ObserverSUM<-collapse1$IGO+collapse1$NGO+collapse1$Observer
collapse1$PartySUM<-collapse1$IGO+collapse1$NGO+collapse1$Observer+collapse1$Party
collapse1$AgenciesSUM<-collapse1$IGO+collapse1$NGO+collapse1$Observer+collapse1$Party+collapse1$IO

#de-mean
collapse1$TOPIC0SUMNew<-collapse1$TOPIC0SUM-(collapse1$IGOSUM+collapse1$NGOSUM+collapse1$ObserverSUM+collapse1$PartySUM+collapse1$AgenciesSUM)/5
collapse1$IGOSUMNew<-collapse1$IGOSUM-(collapse1$IGOSUM+collapse1$NGOSUM+collapse1$ObserverSUM+collapse1$PartySUM+collapse1$AgenciesSUM)/5
collapse1$NGOSUMNew<-collapse1$NGOSUM-(collapse1$IGOSUM+collapse1$NGOSUM+collapse1$ObserverSUM+collapse1$PartySUM+collapse1$AgenciesSUM)/5
collapse1$ObserverSUMNew<-collapse1$ObserverSUM-(collapse1$IGOSUM+collapse1$NGOSUM+collapse1$ObserverSUM+collapse1$PartySUM+collapse1$AgenciesSUM)/5
collapse1$PartySUMNew<-collapse1$PartySUM-(collapse1$IGOSUM+collapse1$NGOSUM+collapse1$ObserverSUM+collapse1$PartySUM+collapse1$AgenciesSUM)/5
collapse1$AgenciesSUMNew<-collapse1$AgenciesSUM-(collapse1$IGOSUM+collapse1$NGOSUM+collapse1$ObserverSUM+collapse1$PartySUM+collapse1$AgenciesSUM)/5

#set up the plotting space
par(mar=c(5.1,1.7,1.1,2.5))
#sets the bottom, left, top and right margins

#make plot
plot(collapse1$timecounter,collapse1$AgenciesSUMNew,type="l",col="white",ylim=c(-23000,17000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$AgenciesSUMNew, rev(collapse1$TOPIC0SUMNew)),
     col = "#8DD3C7", border = NA)
par(new=TRUE)
plot(collapse1$timecounter,collapse1$PartySUMNew,type="l",col="#FDC086",ylim=c(-23000,17000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$PartySUMNew, rev(collapse1$AgenciesSUMNew)),
     col = "#FDC086", border = NA)
par(new=TRUE)
plot(collapse1$timecounter,collapse1$ObserverSUMNew,type="l",col="#FB8072",ylim=c(-23000,17000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$ObserverSUMNew, rev(collapse1$PartySUMNew)),
     col = "#FB8072", border = NA)
par(new=TRUE)
plot(collapse1$timecounter,collapse1$NGOSUMNew,type="l",col="#80B1D3",ylim=c(-23000,17000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$NGOSUMNew, rev(collapse1$ObserverSUMNew)),
     col = "#80B1D3", border = NA)
par(new=TRUE)
plot(collapse1$timecounter,collapse1$IGOSUMNew,type="l",col="#BC80BD",ylim=c(-23000,17000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$IGOSUMNew, rev(collapse1$NGOSUMNew)),
     col = "#BC80BD", border = NA)
par(new=TRUE)
axis(1, at=c(1,7,13,19,25,31,37),lab=c("Pre-COP1","Pre-COP7","COP2","COP8","COP14","COP20","COP26"))
axis(4, at=c(-22236,-17236,-12236,-7236,-2236,2764,7764,12764,17764), lab=c("0","5K","10K","15K","20K","25K","30K","35K","40K"),las=01,cex.axis=.85)
text(1,4500, "IO Attendees",cex=.85,pos=4,col="#8DD3C7")
text(1,6000, "NGO Attendees",cex=.85,pos=4,col="#BC80BD")
text(1,7500, "Observer Attendees",cex=.85,pos=4,col="#80B1D3")
text(1,9000, "Party Attendees",cex=.85,pos=4,col="#FB8072")
text(1,10500, "IGO Attendees",cex=.85,pos=4,col="#FDC086")


#############################################
###########Gender Over Time Plot#############
#############################################

#read in cleaned data
cops.data<-read.csv("cops.cleaned.csv",header=TRUE, row.names=NULL)
precops.data<-read.csv("precops.cleaned.csv",header=TRUE, row.names=NULL)

#combine
complete.data<-rbind(cops.data,precops.data)

#create timecounter
complete.data$timecounter<-ifelse(grepl("COP",complete.data$Meeting),(as.numeric(gsub("COP ","",complete.data$Meeting))+11),NA)
complete.data$timecounter<-ifelse(grepl("INC",complete.data$Meeting),(as.numeric(gsub("INC ","",complete.data$Meeting))),complete.data$timecounter)

#keep vars
complete.data<-complete.data[,c("timecounter","Female")]
complete.data$male<-ifelse(complete.data$Female==1,0,1)
complete.data$male<-ifelse(is.na(complete.data$male),0,complete.data$male)
complete.data$other<-ifelse(is.na(complete.data$Female),1,0)
complete.data$Female<-ifelse(is.na(complete.data$Female),0,complete.data$Female)
complete.data<-na.omit(complete.data)

#collapse variables to get topic freqencies at each point in time
collapse1 <- summaryBy(Female+male+other~timecounter, FUN=c(sum), data=complete.data)

#add column names
colnames(collapse1)<-c("timecounter","female","male", "other")  

#create summed measures
collapse1$TOPIC0SUM<-rep(0,length(collapse1$female))
collapse1$femaleSUM<-collapse1$female
collapse1$maleSUM<-collapse1$female+collapse1$male
collapse1$otherSUM<-collapse1$female+collapse1$male+collapse1$other

#de-mean
collapse1$TOPIC0SUMNew<-collapse1$TOPIC0SUM-(collapse1$female+collapse1$male+collapse1$other)/3
collapse1$femaleSUMNew<-collapse1$femaleSUM-(collapse1$female+collapse1$male+collapse1$other)/3
collapse1$maleSUMNew<-collapse1$maleSUM-(collapse1$female+collapse1$male+collapse1$other)/3
collapse1$otherSUMNew<-collapse1$otherSUM-(collapse1$female+collapse1$male+collapse1$other)/3

#set up plotting space
par(mar=c(5.1,1.7,1.1,2.5))
#sets the bottom, left, top and right margins

#make plot
plot(collapse1$timecounter,collapse1$otherSUMNew,type="l",col="white",ylim=c(-15000,28000),xlim=c(0,40),axes=FALSE,ylab="",xlab="",main="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$otherSUMNew, rev(collapse1$TOPIC0SUMNew)),
     col = "#8DD3C7", border = NA)
par(new=TRUE)
plot(collapse1$timecounter,collapse1$maleSUMNew,type="l",col="#E78AC3",ylim=c(-15000,28000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$maleSUMNew, rev(collapse1$otherSUMNew)),
     col = "#E78AC3", border = NA)
par(new=TRUE)
plot(collapse1$timecounter,collapse1$femaleSUMNew,type="l",col="#FDC086",ylim=c(-15000,28000),xlim=c(0,40),axes=FALSE,ylab="",xlab="")
par(new=TRUE)
polygon(c(collapse1$timecounter, rev(collapse1$timecounter)), c(collapse1$femaleSUMNew, rev(collapse1$maleSUMNew)),
     col = "#FDC086", border = NA)
par(new=TRUE)
axis(1, at=c(1,7,13,19,25,31,37),lab=c("Pre-COP1","Pre-COP7","COP2","COP8","COP14","COP20","COP26"))
axis(4, at=c(-12949.3,-7949.3,-2949.3, 2050.7,7050.7, 12050.7, 17050.7,22050.7,27050.7), lab=c("0","5K","10K","15K","20K","25K","30K","35K","40K"),las=01,cex.axis=.85)
text(1,16000, "Female Attendees",cex=1,pos=4,col="#8DD3C7")
text(1,19000, "Male Attendees",cex=1,pos=4,col="#FDC086")
text(1,22000, "Other Attendees",cex=1,pos=4,col="#E78AC3")