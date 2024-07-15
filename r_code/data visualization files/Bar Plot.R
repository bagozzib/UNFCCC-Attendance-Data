#
#This R-script make a bar plot of honorifics
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
library(wordcloud)

#set working directory
setwd("") 

################################################################
#######################Read In Data#############################
################################################################

#read in cleaned data
cops.data<-read.csv("cops.cleaned.csv",header=TRUE, row.names=NULL,fileEncoding = "UTF-8")
precops.data<-read.csv("precops.cleaned.csv",header=TRUE, row.names=NULL,fileEncoding = "UTF-8")

#combine
complete.data<-rbind(cops.data,precops.data)

#Get frequencies for plotting
sort(table(complete.data$Honorific))

#Manually input values with frequency >1
newlist<-c(rep("Mr.",171275),
	rep("Ms.",99570),
	rep("M.",8663),
	rep("Sr.",7171),
	rep("H.E. Mr.",6991),
	rep("Mme",4765),
	rep("Sra.",4447),
	rep("H.E. Ms.",1624),
	rep("S.E. M",727),
	rep("S.E. Sr.",619),
    rep("Dr.",398),
	rep("S.E. Sra.",200),
	rep("S.E. Mme",157),
	rep("Prof.",105),
	rep("Mrs.",157),
    rep("H.E.",59),
    rep("Rev.",30),
    rep("Mx.",30),
    rep("H.M.",30),
    rep("H.E. Mr. Dr.",29),
    rep("Mr. Dr.",27),
    rep("H.E. Dr.",21),
    rep("Ind.",20),
    rep("H.E. Ms. Dr.",13),
    rep("H.R.H.",12),
    rep("Prof. Dr.",9),
    rep("His",9),
    rep("H.E. M.",9),
    rep("S.E.",8),
    rep("Ms. Dr.",6),
    rep("Miss",6),
    rep("H.H.",6),
    rep("H.E. Sr.",6),
    rep("H.E. Prof.",5),
    rep("Baron",5),
    rep("Lord",4),
    rep("Srta.",3),
    rep("Msgr.",3),
    rep("Hon.",3),
    rep("H.E. Archbishop",3),
    rep("Fr.",3),
    rep("S.E. Mr.",2),
    rep("H.H. Mr.",2),
    rep("H.E. Mrs.",2)
)

#make plot
png("barplot.png", units="in", width=6, height=7, res=500)
par(las=2) # make label text perpendicular to axis
par(mar=c(4,6.5,1,1.5)) # increase y-axis margin.
barplot(sort(table(newlist),decreasing=FALSE), horiz=TRUE,cex.names=.75)
dev.off()

