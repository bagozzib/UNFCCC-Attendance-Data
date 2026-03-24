#
#This R-script standardizing encodings and adds a series of final variables to each relevant CSV file
#

#clear memory
rm( list=ls() )

#set seed
set.seed(9)

#load necessary packages
library(tm)
library(foreign)
library(base)
library(Matrix)
library(plyr)
library(doBy)
library(TTR)
library(stats)
library(RColorBrewer)
library(stringr)

#################################################################################
#######################Read In Intermediate Datasets#############################
#################################################################################

#set working directory to folder with intermediate datasets
setwd("C:/Users/bagoz/Desktop/COPsFormat/")

#read in intermediate untranslated data
cops.data<-read.csv("cops.cleaned with 2025 data.csv",header=TRUE, row.names=NULL,encoding="ISO-8859-13") 
precops.data<-read.csv("precops.cleaned.old.csv",header=TRUE, row.names=NULL,encoding="ISO-8859-13") 

#read in intermediate translated data
cops.data.translated<-read.csv("cops.cleaned_translated with 2025 data.csv",header=TRUE, row.names=NULL,encoding="ISO-8859-13") 
precops.data.translated<-read.csv("precops.cleaned.translated.old.csv",header=TRUE, row.names=NULL,encoding="latin1") 

#add extra variable to precops
#precops.data$Overflow<-0
#precops.data.translated$Overflow<-0
precops.data$Relation<-NA
precops.data.translated$Relation<-NA

#reorder
precops.data <- precops.data[, c("Group_Type","Delegation","Honorific","Person_Name","Job_Title","Division","Affiliation","Relation","Overflow","Virtual","Year","Meeting","Location","Female","IGO",         "NGO","Observer","Party", "IO" )]

precops.data.translated <- precops.data.translated[, c("Group_Type","Delegation","Honorific","Person_Name","Job_Title","Division","Affiliation","Relation","Overflow","Virtual","Year","Meeting","Location","Female","IGO",         "NGO","Observer","Party", "IO" )]

##########################################################################################
#######################Final Cleaning for 2025 Non-Translated#############################
##########################################################################################

#split 2024/2025
cop.example<-cops.data[cops.data$Year==2025,]
cops.data.old<-cops.data[cops.data$Year<2025,]

#########################################################
#########step 2: fix remaining name-title issues#########
#########################################################

#If name has "H.E." or "HE." or "Dr." or "DR." at the start, move this to instead appear at the start of the person's Title-entry
cop.example$Honorific<-ifelse(grepl("^+HE\\.",cop.example$Person_Name),paste(cop.example$Honorific,"HE.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HE\\.",cop.example$Person_Name),gsub("^+HE\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+HE ",cop.example$Person_Name),paste(cop.example$Honorific,"HE.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HE ",cop.example$Person_Name),gsub("^+HE ","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+H\\.E\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+H\\.E\\.",cop.example$Person_Name),gsub("^+H\\.E\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+HL\\.E\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HL\\.E\\.",cop.example$Person_Name),gsub("^+HL\\.E\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+HLE\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HLE\\.",cop.example$Person_Name),gsub("^+HLE\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+H\\.L\\.E\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+H\\.L\\.E\\.",cop.example$Person_Name),gsub("^+H\\.L\\.E\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+DR\\.",cop.example$Person_Name),paste(cop.example$Honorific,"DR.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+DR\\.",cop.example$Person_Name),gsub("^+DR\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+Dr\\.",cop.example$Person_Name),paste(cop.example$Honorific,"Dr.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Dr\\.",cop.example$Person_Name),gsub("^+Dr\\.","",cop.example$Person_Name),cop.example$Person_Name)

#If Title is blank and name starts with "Mr." or "Mrs.", etc. shift that to title.
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mr\\.",cop.example$Person_Name), "Mr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mr\\.",cop.example$Person_Name),gsub("^+Mr\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mrs\\.",cop.example$Person_Name), "Mrs.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mrs\\.",cop.example$Person_Name),gsub("^+Mrs\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sr\\.",cop.example$Person_Name), "Sr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sr\\.",cop.example$Person_Name),gsub("^+Sr\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sra\\.",cop.example$Person_Name), "Sra.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sra\\.",cop.example$Person_Name),gsub("^+Sra\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mme\\.",cop.example$Person_Name), "Mme.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mme\\.",cop.example$Person_Name),gsub("^+Mme\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+M\\.",cop.example$Person_Name), "M.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+M\\.",cop.example$Person_Name),gsub("^+M\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Miss.",cop.example$Person_Name), "Ms.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Miss\\.",cop.example$Person_Name),gsub("^+Miss\\.","",cop.example$Person_Name),cop.example$Person_Name)

#If Title is blank and name starts with "Mr," or "Mrs,", etc. shift that to title.
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mr\\,",cop.example$Person_Name), "Mr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mr\\,",cop.example$Person_Name),gsub("^+Mr\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mrs\\,",cop.example$Person_Name), "Mrs.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mrs\\,",cop.example$Person_Name),gsub("^+Mrs\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sr\\,",cop.example$Person_Name), "Sr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sr\\,",cop.example$Person_Name),gsub("^+Sr\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sra\\,",cop.example$Person_Name), "Sra.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sra\\,",cop.example$Person_Name),gsub("^+Sra\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mme\\,",cop.example$Person_Name), "Mme.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mme\\,",cop.example$Person_Name),gsub("^+Mme\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+M\\,",cop.example$Person_Name), "M.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+M\\,",cop.example$Person_Name),gsub("^+M\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Miss",cop.example$Person_Name), "Ms",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Miss",cop.example$Person_Name),gsub("^+Miss","",cop.example$Person_Name),cop.example$Person_Name)


#fix some remaining common OCR mistakes in Name column
cop.example$Person_Name<-gsub(" \\!"," I",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" \\]"," I",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" \\["," I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\!","I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\]","I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\[","I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\!","l",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\]","l",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\[","l",cop.example$Person_Name)
cop.example$Person_Name<-gsub("- ","-",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" -","-",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" \\.",".",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" 0"," O",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+0","O",cop.example$Person_Name)
cop.example$Person_Name<-gsub("0","o",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\$","S",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\.","",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\,","",cop.example$Person_Name)

#trim remaining leading and trailing whitepsace entries from Name and Title columns
cop.example$Honorific<-trimws(cop.example$Honorific, which = "both")
cop.example$Person_Name<-trimws(cop.example$Person_Name, which = "both")

#Standardizing first name last name order. This is tricky!!
#some entries are "First LAST", others are "LAST, First". 
#But, some entries have a "First Middle-Initial Last" format where the middle initial has an incorrect "," instead of an ".". For example "John M, Smith"
#First I fix the latter problem below, then reverse remaining names.

#if name contains a single letter preceded by a space followed by a comma, as in " A,", replace the comma with a period
cop.example$Person_Name<-ifelse(grepl(" [A-Z],",cop.example$Person_Name),gsub(",",".",cop.example$Person_Name),cop.example$Person_Name)

#now actually standardize first and last name based on remaining commas, by swapping last-first in those cases
for(i in 1:nrow(cop.example)){
	if(grepl(",",cop.example$Person_Name[i])){
	temp<-strsplit(cop.example$Person_Name[i], ", ")
	cop.example$Person_Name[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#fix any remaining entries that end with a period
cop.example$Person_Name<-gsub("\\.+$","",cop.example$Person_Name)

#remove some lingering titles that may be in Name
cop.example$Person_Name<-gsub("^+Prof\\. ","",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+Dr\\. ","",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+The Honourable ","",cop.example$Person_Name)  
cop.example$Person_Name<-gsub("^+The Honorable ","",cop.example$Person_Name)

#fix one remaining issue
cop.example$Person_Name<-gsub("^+\\‘", " and ",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\’", " and ",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\‘+$", " and ",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\’+$", " and ",cop.example$Person_Name)

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
cop.example$Person_Name<-trimws(cop.example$Person_Name, which = "both")

#standardize name data to use capitals only (and always) at the start of each unigram
cop.example$Person_Name<-str_to_title(cop.example$Person_Name) 

#fix a common OCR issue in titles
cop.example$Honorific<-gsub("Mlle","Mme",cop.example$Honorific)
cop.example$Honorific<-gsub("St\\.","Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("St","Sr",cop.example$Honorific)
cop.example$Honorific<-gsub("Sta\\.","Sra.",cop.example$Honorific)
cop.example$Honorific<-gsub("Sta","Sra",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\.E\\.Ms\\.","H.E. Ms.",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\.E\\.Ms","H.E. Ms",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Dr\\.","S.E. Dr.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Dr","S.E. Dr",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.M\\.","S.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.M","S.E. M",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Mme\\.","S.E. Mme.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Mme","S.E. Mme",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Sr\\.","S.E. Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Sr","S.E. Sr",cop.example$Honorific)
cop.example$Honorific<-gsub("Mine","Mme",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\. E\\. Mr\\.","H.E. Mr.",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\.E\\.  Mr\\.","H.E. Mr.",cop.example$Honorific)
cop.example$Honorific<-gsub("M\\. H\\.E\\.","H.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("M\\. S\\.E\\.","S.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("Ms\\. H\\.E\\.","H.E. Ms.",cop.example$Honorific)
cop.example$Honorific<-gsub("Mrs\\. H\\.E\\.","H.E. Mrs.",cop.example$Honorific)
cop.example$Honorific<-gsub("Mr H\\.E\\.","H.E. Mr.",cop.example$Honorific)
cop.example$Honorific<-gsub("Sr\\. S\\.E\\.","S.E. Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("Sra\\. S\\.E\\.","S.E. Sra.",cop.example$Honorific)
cop.example$Honorific<-gsub("S E\\. Sr\\.","S.E. Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("S E\\. M\\.","S.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("^+The ","",cop.example$Honorific)
cop.example$Honorific<-gsub("Honourable","Hon.",cop.example$Honorific)
cop.example$Honorific<-gsub("Honorable","Hon.",cop.example$Honorific)
cop.example$Honorific<-gsub("Senhor","Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("DR","Dr",cop.example$Honorific)
cop.example$Honorific<-gsub("Assoc\\.Prof\\.","Prof.",cop.example$Honorific)
cop.example$Honorific<-gsub("HRH Princess HRH Princess","HRH Princess",cop.example$Honorific) 


#create female varaible 
#NOTE: there may be other variants in other pre-cop/cop years that we need to check...perhaps recover any remaining titles that aren't already listed below across all pre-cop/cops and send them to Daria and me to review.
cop.example$Female[cop.example$Honorific=="Sir"]<-0
cop.example$Female[cop.example$Honorific=="H.E. Mr."]<-0
cop.example$Female[cop.example$Honorific=="H.E.Mr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Mr"]<-0
cop.example$Female[cop.example$Honorific=="H.E. M."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Sr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Ms."]<-1
cop.example$Female[cop.example$Honorific=="H.E. Mrs."]<-1
cop.example$Female[cop.example$Honorific=="HE. Mr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Ms"]<-1
cop.example$Female[cop.example$Honorific=="H.E Ms."]<-1
cop.example$Female[cop.example$Honorific=="H.E Ms"]<-1
cop.example$Female[cop.example$Honorific=="H.E.Ms."]<-1
cop.example$Female[cop.example$Honorific=="H.H. Mr."]<-1
cop.example$Female[cop.example$Honorific=="H.E. Msgr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Msgr"]<-0
cop.example$Female[cop.example$Honorific=="Msgr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Archbishop"]<-0
cop.example$Female[cop.example$Honorific=="Cardinal"]<-0
cop.example$Female[cop.example$Honorific=="His"]<-0
cop.example$Female[cop.example$Honorific=="Rev."]<-0
cop.example$Female[cop.example$Honorific=="Rev"]<-0
cop.example$Female[cop.example$Honorific=="M."]<-0
cop.example$Female[cop.example$Honorific=="M"]<-0
cop.example$Female[cop.example$Honorific=="H.E. Dato’ M."]<-0
cop.example$Female[cop.example$Honorific=="Mme."]<-1
cop.example$Female[cop.example$Honorific=="Mme"]<-1
cop.example$Female[cop.example$Honorific=="Mr."]<-0
cop.example$Female[cop.example$Honorific=="Mr"]<-0
cop.example$Female[cop.example$Honorific=="M. R."]<-0
cop.example$Female[cop.example$Honorific=="Miss"]<-1
cop.example$Female[cop.example$Honorific=="Ms."]<-1
cop.example$Female[cop.example$Honorific=="Ms"]<-1
cop.example$Female[cop.example$Honorific=="Mrs."]<-1
cop.example$Female[cop.example$Honorific=="Mrs"]<-1
cop.example$Female[cop.example$Honorific=="S.E. M."]<-0
cop.example$Female[cop.example$Honorific=="S.E. M"]<-0
cop.example$Female[cop.example$Honorific=="S.E. Ms"]<-0
cop.example$Female[cop.example$Honorific=="S.E.M."]<-0
cop.example$Female[cop.example$Honorific=="S.E. Mme."]<-1
cop.example$Female[cop.example$Honorific=="S.E. Mme"]<-1
cop.example$Female[cop.example$Honorific=="S.E  Mme"]<-1
cop.example$Female[cop.example$Honorific=="S.E. Sr."]<-0
cop.example$Female[cop.example$Honorific=="S.E. Sr"]<-0
cop.example$Female[cop.example$Honorific=="S.E. Sra."]<-1
cop.example$Female[cop.example$Honorific=="S.E. Sra"]<-1
cop.example$Female[cop.example$Honorific=="S.E.Sra."]<-1
cop.example$Female[cop.example$Honorific=="S.E.Sr."]<-0
cop.example$Female[cop.example$Honorific=="S.E.Mme"]<-1
cop.example$Female[cop.example$Honorific=="Sr."]<-0
cop.example$Female[cop.example$Honorific=="Sr"]<-0
cop.example$Female[cop.example$Honorific=="Sr. D."]<-0
cop.example$Female[cop.example$Honorific=="Sra."]<-1
cop.example$Female[cop.example$Honorific=="Sra. Lady"]<-1
cop.example$Female[cop.example$Honorific=="Sra"]<-1
cop.example$Female[cop.example$Honorific=="Srta"]<-1
cop.example$Female[cop.example$Honorific=="Srta."]<-1
cop.example$Female[cop.example$Honorific=="Mr. H E."]<-0   
cop.example$Female[cop.example$Honorific=="Mr. H.E."]<-0   
cop.example$Female[cop.example$Honorific=="Mr. HE."]<-0   
cop.example$Female[cop.example$Honorific=="Mr. HLE."]<-0       
cop.example$Female[cop.example$Honorific=="Ms. HE."]<-0
cop.example$Female[cop.example$Honorific=="S.E.Ms"]<-1
cop.example$Female[cop.example$Honorific=="Fr."]<-1
cop.example$Female[cop.example$Honorific=="H.E. Mr. Dr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Mr. HE."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Ms. Dr."]<-1
cop.example$Female[cop.example$Honorific=="M. DR."]<-0       
cop.example$Female[cop.example$Honorific=="Mr. Dr."]<-0 
cop.example$Female[cop.example$Honorific=="Mr. Gen"]<-0 
cop.example$Female[cop.example$Honorific=="Mr. H.E. Dr."]<-0                  
cop.example$Female[cop.example$Honorific=="Mr. King"]<-0                    
cop.example$Female[cop.example$Honorific=="Mr. M."]<-0                   
cop.example$Female[cop.example$Honorific=="Mr. Min"]<-0                
cop.example$Female[cop.example$Honorific=="Mr. Prince"]<-1
cop.example$Female[cop.example$Honorific=="Ms. Dr."]<-1   
cop.example$Female[cop.example$Honorific=="Ms. DR."]<-1
cop.example$Female[cop.example$Honorific=="Baron"]<-0
cop.example$Female[cop.example$Honorific=="Dame"]<-1
cop.example$Female[cop.example$Honorific=="Lady"]<- 1 
cop.example$Female[cop.example$Honorific=="Lord"]<-0 
cop.example$Female[cop.example$Honorific=="Sr. Dr."]<-0
cop.example$Female[cop.example$Honorific=="Rev. Dr."]<-0
cop.example$Female[cop.example$Honorific=="Rev Dr"]<-0
cop.example$Female[cop.example$Honorific=="Pastor"]<-0
cop.example$Female[cop.example$Honorific=="His Eminence Card."]<-0
cop.example$Female[cop.example$Honorific=="Miss CBE MP"]<-1
cop.example$Female[cop.example$Honorific=="Hon. Ms."]<-1
cop.example$Female[cop.example$Honorific=="Ms. Princess"]<-1
cop.example$Female[cop.example$Honorific=="HRH Princess"]<-1
cop.example$Female[cop.example$Honorific=="Ms. He"]<-1
cop.example$Female[cop.example$Honorific=="Ms. Hon."]<-1
cop.example$Female[cop.example$Honorific=="Ms. Min"]<-1
cop.example$Female[cop.example$Honorific=="Mr. Dr."]<-0
cop.example$Female[cop.example$Honorific=="H.R.H Princess"]<-1
cop.example$Female[cop.example$Honorific=="H.R.H. Prince"]<-0
cop.example$Female[cop.example$Honorific=="Madame"]<-1      
cop.example$Female[cop.example$Honorific=="Monsignor"]<-0  
cop.example$Female[cop.example$Honorific=="Mr. Amb."]<-0
cop.example$Female[cop.example$Honorific=="HE Archbishop"]<-0
cop.example$Female[cop.example$Honorific=="H.E Lt. Gen."]<-0

#Standardize title a bit more
cop.example$Honorific<-gsub("\\.","",cop.example$Honorific)
cop.example$Honorific<-gsub("\\,","",cop.example$Honorific)
cop.example$Honorific<-gsub("SEM","SE M",cop.example$Honorific)
cop.example$Honorific<-gsub("HEMr","HE Mr",cop.example$Honorific)
cop.example$Honorific<-gsub("HEMrs","HE Mrs",cop.example$Honorific)
cop.example$Honorific<-gsub("MrHE","HE Mr",cop.example$Honorific)
cop.example$Honorific<-gsub("MrsHE","HE Mrs",cop.example$Honorific)
cop.example$Honorific<-gsub("H E","HE",cop.example$Honorific)

#Always place HE at the start of a title
for(i in 1:nrow(cop.example)){
	if(grepl(" HE+$",cop.example$Honorific[i])){
	temp<-strsplit(cop.example$Honorific[i], " ")
	cop.example$Honorific[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#Always place SE at the start of a title
for(i in 1:nrow(cop.example)){
	if(grepl(" SE+$",cop.example$Honorific[i])){
	temp<-strsplit(cop.example$Honorific[i], " ")
	cop.example$Honorific[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
cop.example$Honorific<-trimws(cop.example$Honorific, which = "both")
cop.example$Person_Name<-trimws(cop.example$Person_Name, which = "both")
cop.example$Honorific<-str_squish(cop.example$Honorific)
cop.example$Person_Name<-str_squish(cop.example$Person_Name)


###########################################
#########step 4: fix entity entries########
###########################################

#standardize capitalization format
cop.example$Delegation<-str_to_title(cop.example$Delegation) 

#not all years have countries spelled in a foreign language (usually French or Spanish) but some do. 
#other cases have misspellings or OCR problems
#Let's fix all of these manually here:
cop.example$Delegation[cop.example$Delegation=="Albanie"]<-"Albania"                                         
cop.example$Delegation[cop.example$Delegation=="Algerie"]<-"Algeria"                                             
cop.example$Delegation[cop.example$Delegation=="Allemagne"]<-"Germany" 
cop.example$Delegation[cop.example$Delegation=="Allemagne (Suite)"]<-"Germany" 
cop.example$Delegation[cop.example$Delegation=="Antigua-Et-Barbuda"]<-"Antigua And Barbuda"     
cop.example$Delegation[cop.example$Delegation=="Antigua Et Barbuda"]<-"Antigua And Barbuda" 
cop.example$Delegation[cop.example$Delegation=="Arabie Saoudite"]<-"Saudi Arabia"   
cop.example$Delegation[cop.example$Delegation=="Arabie Saoudite (Suite)"]<-"Saudi Arabia" 
cop.example$Delegation[cop.example$Delegation=="Argentine" ]<-"Argentina"                                          
cop.example$Delegation[cop.example$Delegation=="Armenie"]<-"Armenia"                                            
cop.example$Delegation[cop.example$Delegation=="Australie" ]<-"Australia"
cop.example$Delegation[cop.example$Delegation=="Australie (Suite)" ]<-"Australia"         
cop.example$Delegation[cop.example$Delegation=="Autriche"   ]<-"Austria"
cop.example$Delegation[cop.example$Delegation=="Autriche (Suite)"    ]<-"Austria"
cop.example$Delegation[cop.example$Delegation=="Azerbaidjan"    ]<-"Azerbaijan"
cop.example$Delegation[cop.example$Delegation=="Bahrein"  ]<-"Bahrain"                                         
cop.example$Delegation[cop.example$Delegation=="Bangladesh" ]<-"Bangladesh"                                       
cop.example$Delegation[cop.example$Delegation=="Barbade"  ]<-"Barbados"                                         
cop.example$Delegation[cop.example$Delegation=="Belgique"  ]<-"Belgium"                                          
cop.example$Delegation[cop.example$Delegation=="Belize" ]<-"Belize"                                            
cop.example$Delegation[cop.example$Delegation=="Benin" ]<-"Benin"                                             
cop.example$Delegation[cop.example$Delegation=="Bhoutan"   ]<-"Bhutan"  
cop.example$Delegation[cop.example$Delegation=="Bolivia (Plurinational State Of)"  ]<-"Bolivia"
cop.example$Delegation[cop.example$Delegation=="Bolivie"  ]<-"Bolivia"                                          
cop.example$Delegation[cop.example$Delegation=="Bostwana"]<-"Botswana"                                           
cop.example$Delegation[cop.example$Delegation=="Bresil" ]<-"Brazil"                                           
cop.example$Delegation[cop.example$Delegation=="Bulgarie"  ]<-"Bulgaria"                                         
cop.example$Delegation[cop.example$Delegation=="Burkina Faso"   ]<-"Burkina Faso"                                    
cop.example$Delegation[cop.example$Delegation=="Cambodge"  ]<-"Cambodia"                                         
cop.example$Delegation[cop.example$Delegation=="Cameroun"  ]<-"Cameroon"                                         
cop.example$Delegation[cop.example$Delegation=="Canada"   ]<-"Canada"  
cop.example$Delegation[cop.example$Delegation=="Canada (Suite)"   ]<-"Canada"                                    
cop.example$Delegation[cop.example$Delegation=="Cap-Vert"   ]<-"Cape Verde"                                          
cop.example$Delegation[cop.example$Delegation=="Chili"]<-"Chile"                                              
cop.example$Delegation[cop.example$Delegation=="Chine" ]<-"China" 
cop.example$Delegation[cop.example$Delegation=="Republique Populaire De Chine"]<-"China" 
cop.example$Delegation[cop.example$Delegation=="Chine (Suite)"    ]<-"China"   
cop.example$Delegation[cop.example$Delegation=="Colombie"  ]<-"Colombia"
cop.example$Delegation[cop.example$Delegation=="Columbia"]<-"Colombia"
cop.example$Delegation[cop.example$Delegation=="Communaute Europeenne" ]<-"European Community"                               
cop.example$Delegation[cop.example$Delegation=="Comores"  ]<-"Comoros"                                          
cop.example$Delegation[cop.example$Delegation=="Costa Rica"  ]<-"Costa Rica"  
cop.example$Delegation[cop.example$Delegation=="Costarica"  ]<-"Costa Rica"                
cop.example$Delegation[cop.example$Delegation=="Cote D' Ivoire" ]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Cote D’ Ivoire" ]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Cote D’Ivoire" ]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Côte D’ivoire" ]<-"Cote D'ivoire" 
cop.example$Delegation[cop.example$Delegation=="Côte D'ivoire" ]<-"Cote D'ivoire" 
cop.example$Delegation[cop.example$Delegation=="Côté D'ivoire"]<-"Cote D'ivoire" 
cop.example$Delegation[cop.example$Delegation=="Côte D’ Ivoire"]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Cote D’ivoire"]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Croatie" ]<-"Croatia"                                           
cop.example$Delegation[cop.example$Delegation=="Cuba"  ]<-"Cuba"       
cop.example$Delegation[cop.example$Delegation=="Chypre"  ]<-"Cyprus" 
cop.example$Delegation[cop.example$Delegation=="Danemark" ]<-"Denmark"                                          
cop.example$Delegation[cop.example$Delegation=="Djibouti"  ]<-"Djibouti"                                         
cop.example$Delegation[cop.example$Delegation=="Egypte"  ]<-"Egypt"                                           
cop.example$Delegation[cop.example$Delegation=="El Salvador"    ]<-"El Salvador"                                     
cop.example$Delegation[cop.example$Delegation=="Emirats Arabes Unis"    ]<-"United Arab Emirates"                           
cop.example$Delegation[cop.example$Delegation=="Equateur"  ]<-"Ecuador"                                         
cop.example$Delegation[cop.example$Delegation=="Erythree"  ]<-"Eritrea"                                         
cop.example$Delegation[cop.example$Delegation=="Espagne"   ]<-"Spain"                                         
cop.example$Delegation[cop.example$Delegation=="Estonie"   ]<-"Estonia"                                        
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D' Amerique"    ]<-"United States Of America" 
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D'amerique"  ]<-"United States Of America"                             
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D'amerique (Suite)"]<-"United States Of America" 
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’amérique"]<-"United States Of America"                                
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’'Amérique"]<-"United States Of America" 
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’amerique" ]<-"United States Of America"                                    
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’amerique (Suite)"]<-"United States Of America"               
cop.example$Delegation[cop.example$Delegation=="Ethiopie"  ]<-"Ethiopia"                                          
cop.example$Delegation[cop.example$Delegation=="Federation De Russie"  ]<-"Russian Federation"                             
cop.example$Delegation[cop.example$Delegation=="Fidj1"    ]<-"Fiji"  
cop.example$Delegation[cop.example$Delegation=="Fidji"    ]<-"Fiji"                    
cop.example$Delegation[cop.example$Delegation=="Finlande"    ]<-"Finland"                                        
cop.example$Delegation[cop.example$Delegation=="France"    ]<-"France"
cop.example$Delegation[cop.example$Delegation=="France (Suite)"    ]<-"France"        
cop.example$Delegation[cop.example$Delegation=="Gambie"  ]<-"Gambia"                                            
cop.example$Delegation[cop.example$Delegation=="Georgie"    ]<-"Georgia"                                         
cop.example$Delegation[cop.example$Delegation=="Ghana"  ]<-"Ghana"      
cop.example$Delegation[cop.example$Delegation=="Grenade"  ]<-"Grenada"                
cop.example$Delegation[cop.example$Delegation=="Grece"   ]<-"Greece"                                           
cop.example$Delegation[cop.example$Delegation=="Guatemala"  ]<-"Guatemala"
cop.example$Delegation[cop.example$Delegation=="Guinee- Bissau" ]<-"Guinea-Bissau"  
cop.example$Delegation[cop.example$Delegation=="Guinee-Bissau"  ]<-"Guinea-Bissau"           
cop.example$Delegation[cop.example$Delegation=="Guinea-Bissau"   ]<-"Guinea-Bissau"                                  
cop.example$Delegation[cop.example$Delegation=="Guinee"     ]<-"Guinea" 
cop.example$Delegation[cop.example$Delegation=="Guinee Equatoriale"]<-"Equatorial Guinea"
cop.example$Delegation[cop.example$Delegation=="Guyane"   ]<-"Guyana"                                          
cop.example$Delegation[cop.example$Delegation=="Honduras"   ]<-"Honduras"                                          
cop.example$Delegation[cop.example$Delegation=="Hongrie"   ]<-"Hungary"
cop.example$Delegation[cop.example$Delegation=="Iles Cook"   ]<-"Cook Islands" 
cop.example$Delegation[cop.example$Delegation=="Iles Marshall"   ]<-"Marshall Islands"                                   
cop.example$Delegation[cop.example$Delegation=="Iles Salomon"  ]<-"Solomon Islands"                                      
cop.example$Delegation[cop.example$Delegation=="Inde"   ]<-"India"                                           
cop.example$Delegation[cop.example$Delegation=="Indonesie"   ]<-"Indonesia" 
cop.example$Delegation[cop.example$Delegation=="Indonesie (Suite)"  ]<-"Indonesia" 
cop.example$Delegation[cop.example$Delegation=="Indonesie ( Suite)"]<-"Indonesia" 
cop.example$Delegation[cop.example$Delegation=="Irlande"  ]<-"Ireland"                                          
cop.example$Delegation[cop.example$Delegation=="Islande"   ]<-"Iceland"                                          
cop.example$Delegation[cop.example$Delegation=="Italie"     ]<-"Italy"                                         
cop.example$Delegation[cop.example$Delegation=="Jamaique"  ]<-"Jamaica"                                          
cop.example$Delegation[cop.example$Delegation=="Japon"  ]<-"Japan"    
cop.example$Delegation[cop.example$Delegation=="Japon (Suite)"]<-"Japan"
cop.example$Delegation[cop.example$Delegation=="Japan (Suite)"]<-"Japan"
cop.example$Delegation[cop.example$Delegation=="Jordanie"  ]<-"Jordan"
cop.example$Delegation[cop.example$Delegation=="Joordan"  ]<-"Jordan"
cop.example$Delegation[cop.example$Delegation=="Kenya"  ]<-"Kenya"                                            
cop.example$Delegation[cop.example$Delegation=="Kiribati" ]<-"Kiribati"                                          
cop.example$Delegation[cop.example$Delegation=="Koweit"   ]<-"Kuwait"                                         
cop.example$Delegation[cop.example$Delegation=="Lesotho"   ]<-"Lesotho"                                           
cop.example$Delegation[cop.example$Delegation=="Lettonie"    ]<-"Latvia"                                       
cop.example$Delegation[cop.example$Delegation=="Liban"     ]<-"Lebanon"                                         
cop.example$Delegation[cop.example$Delegation=="Liechtenstein"  ]<-"Liechtenstein"                                  
cop.example$Delegation[cop.example$Delegation=="Lituanie"      ]<-"Lithuania"                                    
cop.example$Delegation[cop.example$Delegation=="Luxembourg"  ]<-"Luxembourg"                                        
cop.example$Delegation[cop.example$Delegation=="Malaisie"   ]<-"Malaysia"                                        
cop.example$Delegation[cop.example$Delegation=="Malawi"   ]<-"Malawi"                                         
cop.example$Delegation[cop.example$Delegation=="Maldives" ]<-"Maldives"                                           
cop.example$Delegation[cop.example$Delegation=="Mali"     ]<-"Mali"                                          
cop.example$Delegation[cop.example$Delegation=="Malte"    ]<-"Malta"                                          
cop.example$Delegation[cop.example$Delegation=="Maroc"   ]<-"Morocco"                                          
cop.example$Delegation[cop.example$Delegation=="Maurice" ]<-"Mauritius"                                           
cop.example$Delegation[cop.example$Delegation=="Mauritanie"  ]<-"Mauritania"                                       
cop.example$Delegation[cop.example$Delegation=="Mexique (Suite)"   ]<-"Mexico" 
cop.example$Delegation[cop.example$Delegation=="Mexique"   ]<-"Mexico"  
cop.example$Delegation[cop.example$Delegation=="Micronesie"    ]<-"Micronesia (Federated States Of)"                
cop.example$Delegation[cop.example$Delegation=="Micronesie (Etats Federes De)"    ]<-"Micronesia (Federated States Of)"                  
cop.example$Delegation[cop.example$Delegation=="Monaco"      ]<-"Monaco"                                         
cop.example$Delegation[cop.example$Delegation=="Mongolie"  ]<-"Mongolia"                                          
cop.example$Delegation[cop.example$Delegation=="Myanmar"  ]<-"Myanmar"                                        
cop.example$Delegation[cop.example$Delegation=="Namibie"    ]<-"Namibia"                                       
cop.example$Delegation[cop.example$Delegation=="Nepal"    ]<-"Nepal"                                           
cop.example$Delegation[cop.example$Delegation=="Nicaragua"    ]<-"Nicaragua"                                      
cop.example$Delegation[cop.example$Delegation=="Niger"      ]<-"Niger"                                        
cop.example$Delegation[cop.example$Delegation=="Nigeria"      ]<-"Nigeria"                                      
cop.example$Delegation[cop.example$Delegation=="Nioue"       ]<-"Niue"                                      
cop.example$Delegation[cop.example$Delegation=="Norvege"       ]<-"Norway"                                    
cop.example$Delegation[cop.example$Delegation=="Nouvelle-Zelande"  ]<-"New Zealand" 
cop.example$Delegation[cop.example$Delegation=="Nouvelle~Zelande"    ]<-"New Zealand"
cop.example$Delegation[cop.example$Delegation=="Nouvelle- Zelande"      ]<-"New Zealand"                    
cop.example$Delegation[cop.example$Delegation=="Oman"         ]<-"Oman" 
cop.example$Delegation[cop.example$Delegation=="Oman (Suite)"         ]<-"Oman"        
cop.example$Delegation[cop.example$Delegation=="Ouganda"    ]<-"Uganda"                                        
cop.example$Delegation[cop.example$Delegation=="Ouzbekistan"   ]<-"Uzbekistan"                                      
cop.example$Delegation[cop.example$Delegation=="Pakistan"    ]<-"Pakistan"                                         
cop.example$Delegation[cop.example$Delegation=="Panama"   ]<-"Panama"                                          
cop.example$Delegation[cop.example$Delegation=="Paraguay"  ]<-"Paraguay"   
cop.example$Delegation[cop.example$Delegation=="Pays-Bas (Suite)"]<-"Netherlands"    
cop.example$Delegation[cop.example$Delegation=="Pays-Bas" ]<-"Netherlands"                                          
cop.example$Delegation[cop.example$Delegation=="Perou" ]<-"Peru"       
cop.example$Delegation[cop.example$Delegation=="Perou (Suite)"]<-"Peru" 
cop.example$Delegation[cop.example$Delegation=="Philippines"  ]<-"Philippines"  
cop.example$Delegation[cop.example$Delegation=="Philippines (Suite)"    ]<-"Philippines"                    
cop.example$Delegation[cop.example$Delegation=="Pologne"    ]<-"Poland"                                        
cop.example$Delegation[cop.example$Delegation=="Portugal"    ]<-"Portugal"    
cop.example$Delegation[cop.example$Delegation=="Republique Socialiste Sovietique De Bielorussie"]<-"Belarus"
cop.example$Delegation[cop.example$Delegation=="Qatar"    ]<-"Qatar"                                          
cop.example$Delegation[cop.example$Delegation=="Republique Arabe Syrienne"]<-"Syrian Arab Republic"
cop.example$Delegation[cop.example$Delegation=="République Arabe Syrienne"]<-"Syrian Arab Republic"
cop.example$Delegation[cop.example$Delegation=="Republique Centrafricaine" ]<-"Central African Republic"                         
cop.example$Delegation[cop.example$Delegation=="Republique De Coree"   ]<-"Republic Of Korea"          
cop.example$Delegation[cop.example$Delegation=="Republique De Coree (Suite)"]<-"Republic Of Korea"     
cop.example$Delegation[cop.example$Delegation=="Republique De Moldova"   ]<-"Moldova, Republic Of"   
cop.example$Delegation[cop.example$Delegation=="Lao People’s Democratic Republic"   ]<-"Lao Peoples Democratic Republic" 
cop.example$Delegation[cop.example$Delegation=="Republique Democratique Populaire Lao"   ]<-"Lao Peoples Democratic Republic"  
cop.example$Delegation[cop.example$Delegation=="Lao People's Democratic Republic" ]<-"Lao Peoples Democratic Republic" 
cop.example$Delegation[cop.example$Delegation=="Democratic People’s Republic Of Korea" ]<-"Democratic People's Republic Of Korea"
cop.example$Delegation[cop.example$Delegation=="Democratic Peoples Republic Of Korea" ]<-"Democratic People's Republic Of Korea"        
cop.example$Delegation[cop.example$Delegation=="Republique Populaire Democratique De Coree" ]<-"Democratic People's Republic Of Korea"      
cop.example$Delegation[cop.example$Delegation=="Republique Tcheque"     ]<-"Czech Republic"  
cop.example$Delegation[cop.example$Delegation=="Czechia"     ]<-"Czech Republic"
cop.example$Delegation[cop.example$Delegation=="Republique-Unie De Tanzanite"]<-"United Republic Of Tanzania" 
cop.example$Delegation[cop.example$Delegation=="Republique-Unie De Tanzanie" ]<-"United Republic Of Tanzania"                        
cop.example$Delegation[cop.example$Delegation=="Roumanie"   ]<-"Romania"            
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande-Bretagne Et D'irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D' Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="Royaume- Uni De Grande- Bretagne Et D’irlande Du Nord" ]<-"United Kingdom Of Great Britain And Northern Ireland"       
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande- Bretagne Et D’irlande Du Nord (Suite)" ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord"     ]<-"United Kingdom Of Great Britain And Northern Ireland"     
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord (Suite)"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="United Kingdom"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="Saint-Kitts-Et-Nevis" ]<-"Saint Kitts And Nevis"                            
cop.example$Delegation[cop.example$Delegation=="Sainte-Lucie" ]<-"Saint Lucia"  
cop.example$Delegation[cop.example$Delegation=="Sao Tome-Et-Principe"    ]<-"Sao Tome and Principe"         
cop.example$Delegation[cop.example$Delegation=="Samoa" ]<-"Samoa"   
cop.example$Delegation[cop.example$Delegation=="Papouasie-Nouvelle-Guinee"     ]<-"Papua New Guinea" 
cop.example$Delegation[cop.example$Delegation=="Papouasie-Nouvelle- Guinee"     ]<-"Papua New Guinea" 
cop.example$Delegation[cop.example$Delegation=="Senegal"    ]<-"Senegal"                                         
cop.example$Delegation[cop.example$Delegation=="Sierra Leone"  ]<-"Sierra Leone"                                    
cop.example$Delegation[cop.example$Delegation=="Slovaquie"   ]<-"Slovak Republic"  
cop.example$Delegation[cop.example$Delegation=="Slovakia"   ]<-"Slovak Republic"
cop.example$Delegation[cop.example$Delegation=="Slovenie"  ]<-"Slovenia"                                         
cop.example$Delegation[cop.example$Delegation=="Soudan"  ]<-"Sudan"                                           
cop.example$Delegation[cop.example$Delegation=="Sri Lanka"  ]<-"Sri Lanka"
cop.example$Delegation[cop.example$Delegation=="Srilanka"   ]<-"Sri Lanka"
cop.example$Delegation[cop.example$Delegation=="Suede"   ]<-"Swedan" 
cop.example$Delegation[cop.example$Delegation=="Suede (Suite)"   ]<-"Swedan" 
cop.example$Delegation[cop.example$Delegation=="Sutsse"    ]<-"Switzerland"               
cop.example$Delegation[cop.example$Delegation=="Suisse" ]<-"Switzerland"                                           
cop.example$Delegation[cop.example$Delegation=="Tchad"  ]<-"Chad"                                             
cop.example$Delegation[cop.example$Delegation=="Thailand" ]<-"Thailand"  
cop.example$Delegation[cop.example$Delegation=="Thailande (Suite)" ]<-"Thailand"   
cop.example$Delegation[cop.example$Delegation=="Jamahiriya Arabe Libyenne"]<-"Libyan Arab Jamahiriya"       
cop.example$Delegation[cop.example$Delegation=="Libya"]<-"Libyan Arab Jamahiriya"
cop.example$Delegation[cop.example$Delegation=="Togo"   ]<-"Togo"                                            
cop.example$Delegation[cop.example$Delegation=="Trinite-Et-Tobago"   ]<-"Trinidad And Tobago"
cop.example$Delegation[cop.example$Delegation=="Trinite-Et- Tobago"  ]<-"Trinidad And Tobago"
cop.example$Delegation[cop.example$Delegation=="Tunisie"   ]<-"Tunisia"   
cop.example$Delegation[cop.example$Delegation=="Tchecoslovaquie" ] <-"Czechoslovakia"                                
cop.example$Delegation[cop.example$Delegation=="Thailande"    ]<-"Thailand" 
cop.example$Delegation[cop.example$Delegation=="Turkmenistan" ]<-"Turkmenistan"                                     
cop.example$Delegation[cop.example$Delegation=="Uruguay"     ]<-"Uruguay"                                      
cop.example$Delegation[cop.example$Delegation=="Vanuatu"    ]<-"Vanuatu"    
cop.example$Delegation[cop.example$Delegation=="Venezuela (Bolivarian Republic Of)"]<-"Venezuela"
cop.example$Delegation[cop.example$Delegation=="Venezuela"    ]<-"Venezuela" 
cop.example$Delegation[cop.example$Delegation=="Venezuela (Suite)"    ]<-"Venezuela" 
cop.example$Delegation[cop.example$Delegation=="Vietnam" ]<-"Viet Nam" 
cop.example$Delegation[cop.example$Delegation=="Viet Nam." ]<-"Viet Nam"                                          
cop.example$Delegation[cop.example$Delegation=="Yemen"    ]<-"Yemen"                                          
cop.example$Delegation[cop.example$Delegation=="Zaire"   ]<-"Democratic Republic of The Congo"                
cop.example$Delegation[cop.example$Delegation=="Zambie"  ]<-"Zambia"                                           
cop.example$Delegation[cop.example$Delegation=="Zimbabwe"]<-"Zimbabwe"
cop.example$Delegation[cop.example$Delegation=="Afrique Du Sud"    ]<-"South Africa"                                                                                                                     
cop.example$Delegation[cop.example$Delegation=="Gabon"   ]<-"Gabon"                                                                                                                              
cop.example$Delegation[cop.example$Delegation=="Haiti"       ]<-"Haiti"                                                                                                                           
cop.example$Delegation[cop.example$Delegation=="Tran (Islamic Republic Of)"  ]<-"Iran (Islamic Republic Of)" 
cop.example$Delegation[cop.example$Delegation=="Iran (Republique Islamique De)"  ]<-"Iran (Islamic Republic Of)"   
cop.example$Delegation[cop.example$Delegation=="Iran (Republique Islamique D’)"    ]<-"Iran (Islamic Republic Of)"                                  
cop.example$Delegation[cop.example$Delegation=="Iraq"     ]<-"Iraq"                                                                                                                              
cop.example$Delegation[cop.example$Delegation=="Israel"     ]<-"Israel"  
cop.example$Delegation[cop.example$Delegation=="The Former Yugoslay Republic Of Macedonia" ]<-"The Former Yugoslav Republic Of Macedonia" 
cop.example$Delegation[cop.example$Delegation=="North Macedonia"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                                            
cop.example$Delegation[cop.example$Delegation=="L'ex-Republique Yougoslave De Macedoi!"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                               
cop.example$Delegation[cop.example$Delegation=="Madagascar"          ]<-"Madagascar"                                                                                                                  
cop.example$Delegation[cop.example$Delegation=="Republique Dominicaine"    ]<-"Domnican Republic"                                                                                                             
cop.example$Delegation[cop.example$Delegation=="Saint-Siege"    ]<-"Holy See" 
cop.example$Delegation[cop.example$Delegation=="Saint Siege"     ]<-"Holy See"                                                                                                           
cop.example$Delegation[cop.example$Delegation=="Singapour"   ]<-"Singapore"                                                                                                                          
cop.example$Delegation[cop.example$Delegation=="Swaziland"   ]<-"Swaziland"      
cop.example$Delegation[cop.example$Delegation=="Türkiye"   ]<-"Turkey"                                                    
cop.example$Delegation[cop.example$Delegation=="Turquie"     ]<-"Turkey"                                                                                                                           
cop.example$Delegation[cop.example$Delegation=="Ukrainian Soviet Socialist Republic"]<-"Ukraine" 
cop.example$Delegation[cop.example$Delegation=="Ukraine"]<-"Ukraine" 
cop.example$Delegation[cop.example$Delegation=="Union Des Republiques Socialistes Sovietiques" ]<-"Union of Soviet Socialist Republics"
cop.example$Delegation[cop.example$Delegation=="Yougoslavie" ]<-"Yugoslavia"
cop.example$Delegation[cop.example$Delegation=="Swedan" ]<-"Sweden"                                             
cop.example$Delegation[cop.example$Delegation=="Solomon Islands (Continued0" ]<-"Solomon Islands"                                            
cop.example$Delegation[cop.example$Delegation=="Serbia and Montenegro" ]<-"Serbia"                                            
cop.example$Delegation[cop.example$Delegation=="Seychelles (Continued0" ]<-"Seychelles"                                          
cop.example$Delegation[cop.example$Delegation=="Republic of Cabo Verde"]<-"Cape Verde"                                     
cop.example$Delegation[cop.example$Delegation=="Cote D Ivoire"]<-"Cote D'ivoire"                                         
cop.example$Delegation[cop.example$Delegation=="Côte D`Ivoire"]<-"Cote D'ivoire"                               
cop.example$Delegation[cop.example$Delegation=="Domnican Republic"]<-"Dominican Republic" 
cop.example$Delegation[cop.example$Delegation=="Eswatini"]<-"Swaziland" 
cop.example$Delegation[cop.example$Delegation=="Holysee" ]<-"Holy See" 
cop.example$Delegation[cop.example$Delegation=="Kazakstan"]<-"Kazakhstan"
cop.example$Delegation[cop.example$Delegation=="Moldova, Republic Of"]<- "Moldova"   
cop.example$Delegation[cop.example$Delegation=="Flj1" ]<-"Fiji"  
cop.example$Delegation[cop.example$Delegation=="Guiea"]<-"Guinea"                                           
cop.example$Delegation[cop.example$Delegation=="Georgla"]<-"Georgia"                                          
cop.example$Delegation[cop.example$Delegation=="State of Palestine"]<-"Palestine" 
cop.example$Delegation[cop.example$Delegation=="Nive"]<-"Niue"                                                        
cop.example$Delegation[cop.example$Delegation=="Cape Verde" ]<-"Cabo Verde" 

#fix a few minor over-capitalization issues
cop.example$Delegation<-gsub(" Of ", " of ",cop.example$Delegation)
cop.example$Delegation<-gsub(" Of)", " of)",cop.example$Delegation)
cop.example$Delegation<-gsub(" And ", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub(" For ", " for ",cop.example$Delegation)

#fix one remaining issue
cop.example$Delegation<-gsub("^+\\‘", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub("^+\\’", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub("\\‘+$", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub("\\’+$", " and ",cop.example$Delegation)

#fix a few more cases
cop.example$Delegation[cop.example$Delegation=="Iran(Islamic Republic of)" ]<-"Iran (Islamic Republic of)" 
cop.example$Delegation[cop.example$Delegation=="Netherlands (Kingdom of The)"]<-"Netherlands" 
cop.example$Delegation[cop.example$Delegation==" and Grenada"]<-"Grenada"                                         
cop.example$Delegation[cop.example$Delegation==" and Kenya"]<-"Kenya"   
cop.example$Group_Type[cop.example$Delegation=="Organisation Mondiale Du Commerce (Omc)"]<-"Specialized agencies and related organizations"  
cop.example$Delegation[cop.example$Delegation=="Republic of Moldova"]<- "Moldova" 
cop.example$Delegation[cop.example$Delegation=="Republic of Cabo Verde"]<- "Cabo Verde" 
cop.example$Delegation[cop.example$Delegation=="Serbia and Montenegro" ]<-"Serbia"  
cop.example$Delegation[cop.example$Delegation=="State of Palestine"]<-"Palestine" 
cop.example$Delegation[cop.example$Delegation=="Iran (Islamic Republic of)"]<-"Iran"                                                 
cop.example$Delegation[cop.example$Delegation=="Rhanda"]<-"Rwanda"   
cop.example$Delegation[cop.example$Delegation=="Dominique"]<-"Dominica" 

#Further standardization
cop.example$Delegation<-gsub("\\("," (",cop.example$Delegation)
cop.example$Delegation<-gsub("\\)",") ",cop.example$Delegation)
cop.example$Delegation<-gsub("\\’","'",cop.example$Delegation)
cop.example$Delegation<-gsub("\\( ","(",cop.example$Delegation)
cop.example$Delegation<-gsub(" \\)",")",cop.example$Delegation)
cop.example$Delegation<-str_squish(cop.example$Delegation)
cop.example$Delegation<-trimws(cop.example$Delegation, which = "both")

#create binary vars
cop.example$IGO<-0
cop.example$IGO[cop.example$Group_Type=="Intergovernmental organizations"]<-1
cop.example$NGO<-0
cop.example$NGO[cop.example$Group_Type=="Non-governmental organizations"]<-1
cop.example$Observer<-0
cop.example$Observer[cop.example$Group_Type=="Observer States"]<-1
cop.example$Party<-0
cop.example$Party[cop.example$Group_Type=="Parties"]<-1
cop.example$IO<-0
cop.example$IO[cop.example$Group_Type=="Specialized agencies and related organizations"]<-1
cop.example$IO[cop.example$Group_Type=="United Nations Secretariat units and bodies"]<-1


#re-combine
cops.data<-rbind(cops.data.old,cop.example)




######################################################################################
#######################Final Cleaning for 2025 Translated#############################
######################################################################################

#split 2024/2025
cop.example<-cops.data.translated[cops.data.translated$Year==2025,]
cops.data.translated.old<-cops.data.translated[cops.data.translated$Year<2025,]

#########################################################
#########step 2: fix remaining name-title issues#########
#########################################################

#If name has "H.E." or "HE." or "Dr." or "DR." at the start, move this to instead appear at the start of the person's Title-entry
cop.example$Honorific<-ifelse(grepl("^+HE\\.",cop.example$Person_Name),paste(cop.example$Honorific,"HE.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HE\\.",cop.example$Person_Name),gsub("^+HE\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+HE ",cop.example$Person_Name),paste(cop.example$Honorific,"HE.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HE ",cop.example$Person_Name),gsub("^+HE ","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+H\\.E\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+H\\.E\\.",cop.example$Person_Name),gsub("^+H\\.E\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+HL\\.E\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HL\\.E\\.",cop.example$Person_Name),gsub("^+HL\\.E\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+HLE\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+HLE\\.",cop.example$Person_Name),gsub("^+HLE\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+H\\.L\\.E\\.",cop.example$Person_Name),paste(cop.example$Honorific,"H.E.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+H\\.L\\.E\\.",cop.example$Person_Name),gsub("^+H\\.L\\.E\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+DR\\.",cop.example$Person_Name),paste(cop.example$Honorific,"DR.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+DR\\.",cop.example$Person_Name),gsub("^+DR\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(grepl("^+Dr\\.",cop.example$Person_Name),paste(cop.example$Honorific,"Dr.",sep=" "),cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Dr\\.",cop.example$Person_Name),gsub("^+Dr\\.","",cop.example$Person_Name),cop.example$Person_Name)

#If Title is blank and name starts with "Mr." or "Mrs.", etc. shift that to title.
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mr\\.",cop.example$Person_Name), "Mr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mr\\.",cop.example$Person_Name),gsub("^+Mr\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mrs\\.",cop.example$Person_Name), "Mrs.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mrs\\.",cop.example$Person_Name),gsub("^+Mrs\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sr\\.",cop.example$Person_Name), "Sr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sr\\.",cop.example$Person_Name),gsub("^+Sr\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sra\\.",cop.example$Person_Name), "Sra.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sra\\.",cop.example$Person_Name),gsub("^+Sra\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mme\\.",cop.example$Person_Name), "Mme.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mme\\.",cop.example$Person_Name),gsub("^+Mme\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+M\\.",cop.example$Person_Name), "M.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+M\\.",cop.example$Person_Name),gsub("^+M\\.","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Miss.",cop.example$Person_Name), "Ms.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Miss\\.",cop.example$Person_Name),gsub("^+Miss\\.","",cop.example$Person_Name),cop.example$Person_Name)

#If Title is blank and name starts with "Mr," or "Mrs,", etc. shift that to title.
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mr\\,",cop.example$Person_Name), "Mr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mr\\,",cop.example$Person_Name),gsub("^+Mr\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mrs\\,",cop.example$Person_Name), "Mrs.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mrs\\,",cop.example$Person_Name),gsub("^+Mrs\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sr\\,",cop.example$Person_Name), "Sr.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sr\\,",cop.example$Person_Name),gsub("^+Sr\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Sra\\,",cop.example$Person_Name), "Sra.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Sra\\,",cop.example$Person_Name),gsub("^+Sra\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Mme\\,",cop.example$Person_Name), "Mme.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Mme\\,",cop.example$Person_Name),gsub("^+Mme\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+M\\,",cop.example$Person_Name), "M.",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+M\\,",cop.example$Person_Name),gsub("^+M\\,","",cop.example$Person_Name),cop.example$Person_Name)
cop.example$Honorific<-ifelse(cop.example$Honorific=="" & grepl("^+Miss",cop.example$Person_Name), "Ms",cop.example$Honorific)
cop.example$Person_Name<-ifelse(grepl("^+Miss",cop.example$Person_Name),gsub("^+Miss","",cop.example$Person_Name),cop.example$Person_Name)


#fix some remaining common OCR mistakes in Name column
cop.example$Person_Name<-gsub(" \\!"," I",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" \\]"," I",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" \\["," I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\!","I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\]","I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\[","I",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\!","l",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\]","l",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\[","l",cop.example$Person_Name)
cop.example$Person_Name<-gsub("- ","-",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" -","-",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" \\.",".",cop.example$Person_Name)
cop.example$Person_Name<-gsub(" 0"," O",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+0","O",cop.example$Person_Name)
cop.example$Person_Name<-gsub("0","o",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\$","S",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\.","",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\,","",cop.example$Person_Name)

#trim remaining leading and trailing whitepsace entries from Name and Title columns
cop.example$Honorific<-trimws(cop.example$Honorific, which = "both")
cop.example$Person_Name<-trimws(cop.example$Person_Name, which = "both")

#Standardizing first name last name order. This is tricky!!
#some entries are "First LAST", others are "LAST, First". 
#But, some entries have a "First Middle-Initial Last" format where the middle initial has an incorrect "," instead of an ".". For example "John M, Smith"
#First I fix the latter problem below, then reverse remaining names.

#if name contains a single letter preceded by a space followed by a comma, as in " A,", replace the comma with a period
cop.example$Person_Name<-ifelse(grepl(" [A-Z],",cop.example$Person_Name),gsub(",",".",cop.example$Person_Name),cop.example$Person_Name)

#now actually standardize first and last name based on remaining commas, by swapping last-first in those cases
for(i in 1:nrow(cop.example)){
	if(grepl(",",cop.example$Person_Name[i])){
	temp<-strsplit(cop.example$Person_Name[i], ", ")
	cop.example$Person_Name[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#fix any remaining entries that end with a period
cop.example$Person_Name<-gsub("\\.+$","",cop.example$Person_Name)

#remove some lingering titles that may be in Name
cop.example$Person_Name<-gsub("^+Prof\\. ","",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+Dr\\. ","",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+The Honourable ","",cop.example$Person_Name)  
cop.example$Person_Name<-gsub("^+The Honorable ","",cop.example$Person_Name)

#fix one remaining issue
cop.example$Person_Name<-gsub("^+\\‘", " and ",cop.example$Person_Name)
cop.example$Person_Name<-gsub("^+\\’", " and ",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\‘+$", " and ",cop.example$Person_Name)
cop.example$Person_Name<-gsub("\\’+$", " and ",cop.example$Person_Name)

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
cop.example$Person_Name<-trimws(cop.example$Person_Name, which = "both")

#standardize name data to use capitals only (and always) at the start of each unigram
cop.example$Person_Name<-str_to_title(cop.example$Person_Name) 

#fix a common OCR issue in titles
cop.example$Honorific<-gsub("Mlle","Mme",cop.example$Honorific)
cop.example$Honorific<-gsub("St\\.","Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("St","Sr",cop.example$Honorific)
cop.example$Honorific<-gsub("Sta\\.","Sra.",cop.example$Honorific)
cop.example$Honorific<-gsub("Sta","Sra",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\.E\\.Ms\\.","H.E. Ms.",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\.E\\.Ms","H.E. Ms",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Dr\\.","S.E. Dr.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Dr","S.E. Dr",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.M\\.","S.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.M","S.E. M",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Mme\\.","S.E. Mme.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Mme","S.E. Mme",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Sr\\.","S.E. Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("S\\.E\\.Sr","S.E. Sr",cop.example$Honorific)
cop.example$Honorific<-gsub("Mine","Mme",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\. E\\. Mr\\.","H.E. Mr.",cop.example$Honorific)
cop.example$Honorific<-gsub("H\\.E\\.  Mr\\.","H.E. Mr.",cop.example$Honorific)
cop.example$Honorific<-gsub("M\\. H\\.E\\.","H.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("M\\. S\\.E\\.","S.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("Ms\\. H\\.E\\.","H.E. Ms.",cop.example$Honorific)
cop.example$Honorific<-gsub("Mrs\\. H\\.E\\.","H.E. Mrs.",cop.example$Honorific)
cop.example$Honorific<-gsub("Mr H\\.E\\.","H.E. Mr.",cop.example$Honorific)
cop.example$Honorific<-gsub("Sr\\. S\\.E\\.","S.E. Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("Sra\\. S\\.E\\.","S.E. Sra.",cop.example$Honorific)
cop.example$Honorific<-gsub("S E\\. Sr\\.","S.E. Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("S E\\. M\\.","S.E. M.",cop.example$Honorific)
cop.example$Honorific<-gsub("^+The ","",cop.example$Honorific)
cop.example$Honorific<-gsub("Honourable","Hon.",cop.example$Honorific)
cop.example$Honorific<-gsub("Honorable","Hon.",cop.example$Honorific)
cop.example$Honorific<-gsub("Senhor","Sr.",cop.example$Honorific)
cop.example$Honorific<-gsub("DR","Dr",cop.example$Honorific)
cop.example$Honorific<-gsub("Assoc\\.Prof\\.","Prof.",cop.example$Honorific)
cop.example$Honorific<-gsub("HRH Princess HRH Princess","HRH Princess",cop.example$Honorific) 


#create female varaible 
#NOTE: there may be other variants in other pre-cop/cop years that we need to check...perhaps recover any remaining titles that aren't already listed below across all pre-cop/cops and send them to Daria and me to review.
cop.example$Female[cop.example$Honorific=="Sir"]<-0
cop.example$Female[cop.example$Honorific=="H.E. Mr."]<-0
cop.example$Female[cop.example$Honorific=="H.E.Mr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Mr"]<-0
cop.example$Female[cop.example$Honorific=="H.E. M."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Sr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Ms."]<-1
cop.example$Female[cop.example$Honorific=="H.E. Mrs."]<-1
cop.example$Female[cop.example$Honorific=="HE. Mr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Ms"]<-1
cop.example$Female[cop.example$Honorific=="H.E Ms."]<-1
cop.example$Female[cop.example$Honorific=="H.E Ms"]<-1
cop.example$Female[cop.example$Honorific=="H.E.Ms."]<-1
cop.example$Female[cop.example$Honorific=="H.H. Mr."]<-1
cop.example$Female[cop.example$Honorific=="H.E. Msgr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Msgr"]<-0
cop.example$Female[cop.example$Honorific=="Msgr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Archbishop"]<-0
cop.example$Female[cop.example$Honorific=="Cardinal"]<-0
cop.example$Female[cop.example$Honorific=="His"]<-0
cop.example$Female[cop.example$Honorific=="Rev."]<-0
cop.example$Female[cop.example$Honorific=="Rev"]<-0
cop.example$Female[cop.example$Honorific=="M."]<-0
cop.example$Female[cop.example$Honorific=="M"]<-0
cop.example$Female[cop.example$Honorific=="H.E. Dato’ M."]<-0
cop.example$Female[cop.example$Honorific=="Mme."]<-1
cop.example$Female[cop.example$Honorific=="Mme"]<-1
cop.example$Female[cop.example$Honorific=="Mr."]<-0
cop.example$Female[cop.example$Honorific=="Mr"]<-0
cop.example$Female[cop.example$Honorific=="M. R."]<-0
cop.example$Female[cop.example$Honorific=="Miss"]<-1
cop.example$Female[cop.example$Honorific=="Ms."]<-1
cop.example$Female[cop.example$Honorific=="Ms"]<-1
cop.example$Female[cop.example$Honorific=="Mrs."]<-1
cop.example$Female[cop.example$Honorific=="Mrs"]<-1
cop.example$Female[cop.example$Honorific=="S.E. M."]<-0
cop.example$Female[cop.example$Honorific=="S.E. M"]<-0
cop.example$Female[cop.example$Honorific=="S.E. Ms"]<-0
cop.example$Female[cop.example$Honorific=="S.E.M."]<-0
cop.example$Female[cop.example$Honorific=="S.E. Mme."]<-1
cop.example$Female[cop.example$Honorific=="S.E. Mme"]<-1
cop.example$Female[cop.example$Honorific=="S.E  Mme"]<-1
cop.example$Female[cop.example$Honorific=="S.E. Sr."]<-0
cop.example$Female[cop.example$Honorific=="S.E. Sr"]<-0
cop.example$Female[cop.example$Honorific=="S.E. Sra."]<-1
cop.example$Female[cop.example$Honorific=="S.E. Sra"]<-1
cop.example$Female[cop.example$Honorific=="S.E.Sra."]<-1
cop.example$Female[cop.example$Honorific=="S.E.Sr."]<-0
cop.example$Female[cop.example$Honorific=="S.E.Mme"]<-1
cop.example$Female[cop.example$Honorific=="Sr."]<-0
cop.example$Female[cop.example$Honorific=="Sr"]<-0
cop.example$Female[cop.example$Honorific=="Sr. D."]<-0
cop.example$Female[cop.example$Honorific=="Sra."]<-1
cop.example$Female[cop.example$Honorific=="Sra. Lady"]<-1
cop.example$Female[cop.example$Honorific=="Sra"]<-1
cop.example$Female[cop.example$Honorific=="Srta"]<-1
cop.example$Female[cop.example$Honorific=="Srta."]<-1
cop.example$Female[cop.example$Honorific=="Mr. H E."]<-0   
cop.example$Female[cop.example$Honorific=="Mr. H.E."]<-0   
cop.example$Female[cop.example$Honorific=="Mr. HE."]<-0   
cop.example$Female[cop.example$Honorific=="Mr. HLE."]<-0       
cop.example$Female[cop.example$Honorific=="Ms. HE."]<-0
cop.example$Female[cop.example$Honorific=="S.E.Ms"]<-1
cop.example$Female[cop.example$Honorific=="Fr."]<-1
cop.example$Female[cop.example$Honorific=="H.E. Mr. Dr."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Mr. HE."]<-0
cop.example$Female[cop.example$Honorific=="H.E. Ms. Dr."]<-1
cop.example$Female[cop.example$Honorific=="M. DR."]<-0       
cop.example$Female[cop.example$Honorific=="Mr. Dr."]<-0 
cop.example$Female[cop.example$Honorific=="Mr. Gen"]<-0 
cop.example$Female[cop.example$Honorific=="Mr. H.E. Dr."]<-0                  
cop.example$Female[cop.example$Honorific=="Mr. King"]<-0                    
cop.example$Female[cop.example$Honorific=="Mr. M."]<-0                   
cop.example$Female[cop.example$Honorific=="Mr. Min"]<-0                
cop.example$Female[cop.example$Honorific=="Mr. Prince"]<-1
cop.example$Female[cop.example$Honorific=="Ms. Dr."]<-1   
cop.example$Female[cop.example$Honorific=="Ms. DR."]<-1
cop.example$Female[cop.example$Honorific=="Baron"]<-0
cop.example$Female[cop.example$Honorific=="Dame"]<-1
cop.example$Female[cop.example$Honorific=="Lady"]<- 1 
cop.example$Female[cop.example$Honorific=="Lord"]<-0 
cop.example$Female[cop.example$Honorific=="Sr. Dr."]<-0
cop.example$Female[cop.example$Honorific=="Rev. Dr."]<-0
cop.example$Female[cop.example$Honorific=="Rev Dr"]<-0
cop.example$Female[cop.example$Honorific=="Pastor"]<-0
cop.example$Female[cop.example$Honorific=="His Eminence Card."]<-0
cop.example$Female[cop.example$Honorific=="Miss CBE MP"]<-1
cop.example$Female[cop.example$Honorific=="Hon. Ms."]<-1
cop.example$Female[cop.example$Honorific=="Ms. Princess"]<-1
cop.example$Female[cop.example$Honorific=="HRH Princess"]<-1
cop.example$Female[cop.example$Honorific=="Ms. He"]<-1
cop.example$Female[cop.example$Honorific=="Ms. Hon."]<-1
cop.example$Female[cop.example$Honorific=="Ms. Min"]<-1
cop.example$Female[cop.example$Honorific=="Mr. Dr."]<-0
cop.example$Female[cop.example$Honorific=="H.R.H Princess"]<-1
cop.example$Female[cop.example$Honorific=="H.R.H. Prince"]<-0
cop.example$Female[cop.example$Honorific=="Madame"]<-1      
cop.example$Female[cop.example$Honorific=="Monsignor"]<-0  
cop.example$Female[cop.example$Honorific=="Mr. Amb."]<-0
cop.example$Female[cop.example$Honorific=="HE Archbishop"]<-0
cop.example$Female[cop.example$Honorific=="H.E Lt. Gen."]<-0

#Standardize title a bit more
cop.example$Honorific<-gsub("\\.","",cop.example$Honorific)
cop.example$Honorific<-gsub("\\,","",cop.example$Honorific)
cop.example$Honorific<-gsub("SEM","SE M",cop.example$Honorific)
cop.example$Honorific<-gsub("HEMr","HE Mr",cop.example$Honorific)
cop.example$Honorific<-gsub("HEMrs","HE Mrs",cop.example$Honorific)
cop.example$Honorific<-gsub("MrHE","HE Mr",cop.example$Honorific)
cop.example$Honorific<-gsub("MrsHE","HE Mrs",cop.example$Honorific)
cop.example$Honorific<-gsub("H E","HE",cop.example$Honorific)

#Always place HE at the start of a title
for(i in 1:nrow(cop.example)){
	if(grepl(" HE+$",cop.example$Honorific[i])){
	temp<-strsplit(cop.example$Honorific[i], " ")
	cop.example$Honorific[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#Always place SE at the start of a title
for(i in 1:nrow(cop.example)){
	if(grepl(" SE+$",cop.example$Honorific[i])){
	temp<-strsplit(cop.example$Honorific[i], " ")
	cop.example$Honorific[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
cop.example$Honorific<-trimws(cop.example$Honorific, which = "both")
cop.example$Person_Name<-trimws(cop.example$Person_Name, which = "both")
cop.example$Honorific<-str_squish(cop.example$Honorific)
cop.example$Person_Name<-str_squish(cop.example$Person_Name)


###########################################
#########step 4: fix entity entries########
###########################################

#standardize capitalization format
cop.example$Delegation<-str_to_title(cop.example$Delegation) 

#not all years have countries spelled in a foreign language (usually French or Spanish) but some do. 
#other cases have misspellings or OCR problems
#Let's fix all of these manually here:
cop.example$Delegation[cop.example$Delegation=="Albanie"]<-"Albania"                                         
cop.example$Delegation[cop.example$Delegation=="Algerie"]<-"Algeria"                                             
cop.example$Delegation[cop.example$Delegation=="Allemagne"]<-"Germany" 
cop.example$Delegation[cop.example$Delegation=="Allemagne (Suite)"]<-"Germany" 
cop.example$Delegation[cop.example$Delegation=="Antigua-Et-Barbuda"]<-"Antigua And Barbuda"     
cop.example$Delegation[cop.example$Delegation=="Antigua Et Barbuda"]<-"Antigua And Barbuda" 
cop.example$Delegation[cop.example$Delegation=="Arabie Saoudite"]<-"Saudi Arabia"   
cop.example$Delegation[cop.example$Delegation=="Arabie Saoudite (Suite)"]<-"Saudi Arabia" 
cop.example$Delegation[cop.example$Delegation=="Argentine" ]<-"Argentina"                                          
cop.example$Delegation[cop.example$Delegation=="Armenie"]<-"Armenia"                                            
cop.example$Delegation[cop.example$Delegation=="Australie" ]<-"Australia"
cop.example$Delegation[cop.example$Delegation=="Australie (Suite)" ]<-"Australia"         
cop.example$Delegation[cop.example$Delegation=="Autriche"   ]<-"Austria"
cop.example$Delegation[cop.example$Delegation=="Autriche (Suite)"    ]<-"Austria"
cop.example$Delegation[cop.example$Delegation=="Azerbaidjan"    ]<-"Azerbaijan"
cop.example$Delegation[cop.example$Delegation=="Bahrein"  ]<-"Bahrain"                                         
cop.example$Delegation[cop.example$Delegation=="Bangladesh" ]<-"Bangladesh"                                       
cop.example$Delegation[cop.example$Delegation=="Barbade"  ]<-"Barbados"                                         
cop.example$Delegation[cop.example$Delegation=="Belgique"  ]<-"Belgium"                                          
cop.example$Delegation[cop.example$Delegation=="Belize" ]<-"Belize"                                            
cop.example$Delegation[cop.example$Delegation=="Benin" ]<-"Benin"                                             
cop.example$Delegation[cop.example$Delegation=="Bhoutan"   ]<-"Bhutan"  
cop.example$Delegation[cop.example$Delegation=="Bolivia (Plurinational State Of)"  ]<-"Bolivia"
cop.example$Delegation[cop.example$Delegation=="Bolivie"  ]<-"Bolivia"                                          
cop.example$Delegation[cop.example$Delegation=="Bostwana"]<-"Botswana"                                           
cop.example$Delegation[cop.example$Delegation=="Bresil" ]<-"Brazil"                                           
cop.example$Delegation[cop.example$Delegation=="Bulgarie"  ]<-"Bulgaria"                                         
cop.example$Delegation[cop.example$Delegation=="Burkina Faso"   ]<-"Burkina Faso"                                    
cop.example$Delegation[cop.example$Delegation=="Cambodge"  ]<-"Cambodia"                                         
cop.example$Delegation[cop.example$Delegation=="Cameroun"  ]<-"Cameroon"                                         
cop.example$Delegation[cop.example$Delegation=="Canada"   ]<-"Canada"  
cop.example$Delegation[cop.example$Delegation=="Canada (Suite)"   ]<-"Canada"                                    
cop.example$Delegation[cop.example$Delegation=="Cap-Vert"   ]<-"Cape Verde"                                          
cop.example$Delegation[cop.example$Delegation=="Chili"]<-"Chile"                                              
cop.example$Delegation[cop.example$Delegation=="Chine" ]<-"China" 
cop.example$Delegation[cop.example$Delegation=="Republique Populaire De Chine"]<-"China" 
cop.example$Delegation[cop.example$Delegation=="Chine (Suite)"    ]<-"China"   
cop.example$Delegation[cop.example$Delegation=="Colombie"  ]<-"Colombia"
cop.example$Delegation[cop.example$Delegation=="Columbia"]<-"Colombia"
cop.example$Delegation[cop.example$Delegation=="Communaute Europeenne" ]<-"European Community"                               
cop.example$Delegation[cop.example$Delegation=="Comores"  ]<-"Comoros"                                          
cop.example$Delegation[cop.example$Delegation=="Costa Rica"  ]<-"Costa Rica"  
cop.example$Delegation[cop.example$Delegation=="Costarica"  ]<-"Costa Rica"                
cop.example$Delegation[cop.example$Delegation=="Cote D' Ivoire" ]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Cote D’ Ivoire" ]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Cote D’Ivoire" ]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Côte D’ivoire" ]<-"Cote D'ivoire" 
cop.example$Delegation[cop.example$Delegation=="Côte D'ivoire" ]<-"Cote D'ivoire" 
cop.example$Delegation[cop.example$Delegation=="Côté D'ivoire"]<-"Cote D'ivoire" 
cop.example$Delegation[cop.example$Delegation=="Côte D’ Ivoire"]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Cote D’ivoire"]<-"Cote D'ivoire"
cop.example$Delegation[cop.example$Delegation=="Croatie" ]<-"Croatia"                                           
cop.example$Delegation[cop.example$Delegation=="Cuba"  ]<-"Cuba"       
cop.example$Delegation[cop.example$Delegation=="Chypre"  ]<-"Cyprus" 
cop.example$Delegation[cop.example$Delegation=="Danemark" ]<-"Denmark"                                          
cop.example$Delegation[cop.example$Delegation=="Djibouti"  ]<-"Djibouti"                                         
cop.example$Delegation[cop.example$Delegation=="Egypte"  ]<-"Egypt"                                           
cop.example$Delegation[cop.example$Delegation=="El Salvador"    ]<-"El Salvador"                                     
cop.example$Delegation[cop.example$Delegation=="Emirats Arabes Unis"    ]<-"United Arab Emirates"                           
cop.example$Delegation[cop.example$Delegation=="Equateur"  ]<-"Ecuador"                                         
cop.example$Delegation[cop.example$Delegation=="Erythree"  ]<-"Eritrea"                                         
cop.example$Delegation[cop.example$Delegation=="Espagne"   ]<-"Spain"                                         
cop.example$Delegation[cop.example$Delegation=="Estonie"   ]<-"Estonia"                                        
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D' Amerique"    ]<-"United States Of America" 
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D'amerique"  ]<-"United States Of America"                             
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D'amerique (Suite)"]<-"United States Of America" 
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’amérique"]<-"United States Of America"                                
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’'Amérique"]<-"United States Of America" 
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’amerique" ]<-"United States Of America"                                    
cop.example$Delegation[cop.example$Delegation=="Etats-Unis D’amerique (Suite)"]<-"United States Of America"               
cop.example$Delegation[cop.example$Delegation=="Ethiopie"  ]<-"Ethiopia"                                          
cop.example$Delegation[cop.example$Delegation=="Federation De Russie"  ]<-"Russian Federation"                             
cop.example$Delegation[cop.example$Delegation=="Fidj1"    ]<-"Fiji"  
cop.example$Delegation[cop.example$Delegation=="Fidji"    ]<-"Fiji"                    
cop.example$Delegation[cop.example$Delegation=="Finlande"    ]<-"Finland"                                        
cop.example$Delegation[cop.example$Delegation=="France"    ]<-"France"
cop.example$Delegation[cop.example$Delegation=="France (Suite)"    ]<-"France"        
cop.example$Delegation[cop.example$Delegation=="Gambie"  ]<-"Gambia"                                            
cop.example$Delegation[cop.example$Delegation=="Georgie"    ]<-"Georgia"                                         
cop.example$Delegation[cop.example$Delegation=="Ghana"  ]<-"Ghana"      
cop.example$Delegation[cop.example$Delegation=="Grenade"  ]<-"Grenada"                
cop.example$Delegation[cop.example$Delegation=="Grece"   ]<-"Greece"                                           
cop.example$Delegation[cop.example$Delegation=="Guatemala"  ]<-"Guatemala"
cop.example$Delegation[cop.example$Delegation=="Guinee- Bissau" ]<-"Guinea-Bissau"  
cop.example$Delegation[cop.example$Delegation=="Guinee-Bissau"  ]<-"Guinea-Bissau"           
cop.example$Delegation[cop.example$Delegation=="Guinea-Bissau"   ]<-"Guinea-Bissau"                                  
cop.example$Delegation[cop.example$Delegation=="Guinee"     ]<-"Guinea" 
cop.example$Delegation[cop.example$Delegation=="Guinee Equatoriale"]<-"Equatorial Guinea"
cop.example$Delegation[cop.example$Delegation=="Guyane"   ]<-"Guyana"                                          
cop.example$Delegation[cop.example$Delegation=="Honduras"   ]<-"Honduras"                                          
cop.example$Delegation[cop.example$Delegation=="Hongrie"   ]<-"Hungary"
cop.example$Delegation[cop.example$Delegation=="Iles Cook"   ]<-"Cook Islands" 
cop.example$Delegation[cop.example$Delegation=="Iles Marshall"   ]<-"Marshall Islands"                                   
cop.example$Delegation[cop.example$Delegation=="Iles Salomon"  ]<-"Solomon Islands"                                      
cop.example$Delegation[cop.example$Delegation=="Inde"   ]<-"India"                                           
cop.example$Delegation[cop.example$Delegation=="Indonesie"   ]<-"Indonesia" 
cop.example$Delegation[cop.example$Delegation=="Indonesie (Suite)"  ]<-"Indonesia" 
cop.example$Delegation[cop.example$Delegation=="Indonesie ( Suite)"]<-"Indonesia" 
cop.example$Delegation[cop.example$Delegation=="Irlande"  ]<-"Ireland"                                          
cop.example$Delegation[cop.example$Delegation=="Islande"   ]<-"Iceland"                                          
cop.example$Delegation[cop.example$Delegation=="Italie"     ]<-"Italy"                                         
cop.example$Delegation[cop.example$Delegation=="Jamaique"  ]<-"Jamaica"                                          
cop.example$Delegation[cop.example$Delegation=="Japon"  ]<-"Japan"    
cop.example$Delegation[cop.example$Delegation=="Japon (Suite)"]<-"Japan"
cop.example$Delegation[cop.example$Delegation=="Japan (Suite)"]<-"Japan"
cop.example$Delegation[cop.example$Delegation=="Jordanie"  ]<-"Jordan"
cop.example$Delegation[cop.example$Delegation=="Joordan"  ]<-"Jordan"
cop.example$Delegation[cop.example$Delegation=="Kenya"  ]<-"Kenya"                                            
cop.example$Delegation[cop.example$Delegation=="Kiribati" ]<-"Kiribati"                                          
cop.example$Delegation[cop.example$Delegation=="Koweit"   ]<-"Kuwait"                                         
cop.example$Delegation[cop.example$Delegation=="Lesotho"   ]<-"Lesotho"                                           
cop.example$Delegation[cop.example$Delegation=="Lettonie"    ]<-"Latvia"                                       
cop.example$Delegation[cop.example$Delegation=="Liban"     ]<-"Lebanon"                                         
cop.example$Delegation[cop.example$Delegation=="Liechtenstein"  ]<-"Liechtenstein"                                  
cop.example$Delegation[cop.example$Delegation=="Lituanie"      ]<-"Lithuania"                                    
cop.example$Delegation[cop.example$Delegation=="Luxembourg"  ]<-"Luxembourg"                                        
cop.example$Delegation[cop.example$Delegation=="Malaisie"   ]<-"Malaysia"                                        
cop.example$Delegation[cop.example$Delegation=="Malawi"   ]<-"Malawi"                                         
cop.example$Delegation[cop.example$Delegation=="Maldives" ]<-"Maldives"                                           
cop.example$Delegation[cop.example$Delegation=="Mali"     ]<-"Mali"                                          
cop.example$Delegation[cop.example$Delegation=="Malte"    ]<-"Malta"                                          
cop.example$Delegation[cop.example$Delegation=="Maroc"   ]<-"Morocco"                                          
cop.example$Delegation[cop.example$Delegation=="Maurice" ]<-"Mauritius"                                           
cop.example$Delegation[cop.example$Delegation=="Mauritanie"  ]<-"Mauritania"                                       
cop.example$Delegation[cop.example$Delegation=="Mexique (Suite)"   ]<-"Mexico" 
cop.example$Delegation[cop.example$Delegation=="Mexique"   ]<-"Mexico"  
cop.example$Delegation[cop.example$Delegation=="Micronesie"    ]<-"Micronesia (Federated States Of)"                
cop.example$Delegation[cop.example$Delegation=="Micronesie (Etats Federes De)"    ]<-"Micronesia (Federated States Of)"                  
cop.example$Delegation[cop.example$Delegation=="Monaco"      ]<-"Monaco"                                         
cop.example$Delegation[cop.example$Delegation=="Mongolie"  ]<-"Mongolia"                                          
cop.example$Delegation[cop.example$Delegation=="Myanmar"  ]<-"Myanmar"                                        
cop.example$Delegation[cop.example$Delegation=="Namibie"    ]<-"Namibia"                                       
cop.example$Delegation[cop.example$Delegation=="Nepal"    ]<-"Nepal"                                           
cop.example$Delegation[cop.example$Delegation=="Nicaragua"    ]<-"Nicaragua"                                      
cop.example$Delegation[cop.example$Delegation=="Niger"      ]<-"Niger"                                        
cop.example$Delegation[cop.example$Delegation=="Nigeria"      ]<-"Nigeria"                                      
cop.example$Delegation[cop.example$Delegation=="Nioue"       ]<-"Niue"                                      
cop.example$Delegation[cop.example$Delegation=="Norvege"       ]<-"Norway"                                    
cop.example$Delegation[cop.example$Delegation=="Nouvelle-Zelande"  ]<-"New Zealand" 
cop.example$Delegation[cop.example$Delegation=="Nouvelle~Zelande"    ]<-"New Zealand"
cop.example$Delegation[cop.example$Delegation=="Nouvelle- Zelande"      ]<-"New Zealand"                    
cop.example$Delegation[cop.example$Delegation=="Oman"         ]<-"Oman" 
cop.example$Delegation[cop.example$Delegation=="Oman (Suite)"         ]<-"Oman"        
cop.example$Delegation[cop.example$Delegation=="Ouganda"    ]<-"Uganda"                                        
cop.example$Delegation[cop.example$Delegation=="Ouzbekistan"   ]<-"Uzbekistan"                                      
cop.example$Delegation[cop.example$Delegation=="Pakistan"    ]<-"Pakistan"                                         
cop.example$Delegation[cop.example$Delegation=="Panama"   ]<-"Panama"                                          
cop.example$Delegation[cop.example$Delegation=="Paraguay"  ]<-"Paraguay"   
cop.example$Delegation[cop.example$Delegation=="Pays-Bas (Suite)"]<-"Netherlands"    
cop.example$Delegation[cop.example$Delegation=="Pays-Bas" ]<-"Netherlands"                                          
cop.example$Delegation[cop.example$Delegation=="Perou" ]<-"Peru"       
cop.example$Delegation[cop.example$Delegation=="Perou (Suite)"]<-"Peru" 
cop.example$Delegation[cop.example$Delegation=="Philippines"  ]<-"Philippines"  
cop.example$Delegation[cop.example$Delegation=="Philippines (Suite)"    ]<-"Philippines"                    
cop.example$Delegation[cop.example$Delegation=="Pologne"    ]<-"Poland"                                        
cop.example$Delegation[cop.example$Delegation=="Portugal"    ]<-"Portugal"    
cop.example$Delegation[cop.example$Delegation=="Republique Socialiste Sovietique De Bielorussie"]<-"Belarus"
cop.example$Delegation[cop.example$Delegation=="Qatar"    ]<-"Qatar"                                          
cop.example$Delegation[cop.example$Delegation=="Republique Arabe Syrienne"]<-"Syrian Arab Republic"
cop.example$Delegation[cop.example$Delegation=="République Arabe Syrienne"]<-"Syrian Arab Republic"
cop.example$Delegation[cop.example$Delegation=="Republique Centrafricaine" ]<-"Central African Republic"                         
cop.example$Delegation[cop.example$Delegation=="Republique De Coree"   ]<-"Republic Of Korea"          
cop.example$Delegation[cop.example$Delegation=="Republique De Coree (Suite)"]<-"Republic Of Korea"     
cop.example$Delegation[cop.example$Delegation=="Republique De Moldova"   ]<-"Moldova, Republic Of"   
cop.example$Delegation[cop.example$Delegation=="Lao People’s Democratic Republic"   ]<-"Lao Peoples Democratic Republic" 
cop.example$Delegation[cop.example$Delegation=="Republique Democratique Populaire Lao"   ]<-"Lao Peoples Democratic Republic"  
cop.example$Delegation[cop.example$Delegation=="Lao People's Democratic Republic" ]<-"Lao Peoples Democratic Republic" 
cop.example$Delegation[cop.example$Delegation=="Democratic People’s Republic Of Korea" ]<-"Democratic People's Republic Of Korea"
cop.example$Delegation[cop.example$Delegation=="Democratic Peoples Republic Of Korea" ]<-"Democratic People's Republic Of Korea"        
cop.example$Delegation[cop.example$Delegation=="Republique Populaire Democratique De Coree" ]<-"Democratic People's Republic Of Korea"      
cop.example$Delegation[cop.example$Delegation=="Republique Tcheque"     ]<-"Czech Republic"  
cop.example$Delegation[cop.example$Delegation=="Czechia"     ]<-"Czech Republic"
cop.example$Delegation[cop.example$Delegation=="Republique-Unie De Tanzanite"]<-"United Republic Of Tanzania" 
cop.example$Delegation[cop.example$Delegation=="Republique-Unie De Tanzanie" ]<-"United Republic Of Tanzania"                        
cop.example$Delegation[cop.example$Delegation=="Roumanie"   ]<-"Romania"            
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande-Bretagne Et D'irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D' Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="Royaume- Uni De Grande- Bretagne Et D’irlande Du Nord" ]<-"United Kingdom Of Great Britain And Northern Ireland"       
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande- Bretagne Et D’irlande Du Nord (Suite)" ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord"     ]<-"United Kingdom Of Great Britain And Northern Ireland"     
cop.example$Delegation[cop.example$Delegation=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord (Suite)"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="United Kingdom"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Delegation[cop.example$Delegation=="Saint-Kitts-Et-Nevis" ]<-"Saint Kitts And Nevis"                            
cop.example$Delegation[cop.example$Delegation=="Sainte-Lucie" ]<-"Saint Lucia"  
cop.example$Delegation[cop.example$Delegation=="Sao Tome-Et-Principe"    ]<-"Sao Tome and Principe"         
cop.example$Delegation[cop.example$Delegation=="Samoa" ]<-"Samoa"   
cop.example$Delegation[cop.example$Delegation=="Papouasie-Nouvelle-Guinee"     ]<-"Papua New Guinea" 
cop.example$Delegation[cop.example$Delegation=="Papouasie-Nouvelle- Guinee"     ]<-"Papua New Guinea" 
cop.example$Delegation[cop.example$Delegation=="Senegal"    ]<-"Senegal"                                         
cop.example$Delegation[cop.example$Delegation=="Sierra Leone"  ]<-"Sierra Leone"                                    
cop.example$Delegation[cop.example$Delegation=="Slovaquie"   ]<-"Slovak Republic"  
cop.example$Delegation[cop.example$Delegation=="Slovakia"   ]<-"Slovak Republic"
cop.example$Delegation[cop.example$Delegation=="Slovenie"  ]<-"Slovenia"                                         
cop.example$Delegation[cop.example$Delegation=="Soudan"  ]<-"Sudan"                                           
cop.example$Delegation[cop.example$Delegation=="Sri Lanka"  ]<-"Sri Lanka"
cop.example$Delegation[cop.example$Delegation=="Srilanka"   ]<-"Sri Lanka"
cop.example$Delegation[cop.example$Delegation=="Suede"   ]<-"Swedan" 
cop.example$Delegation[cop.example$Delegation=="Suede (Suite)"   ]<-"Swedan" 
cop.example$Delegation[cop.example$Delegation=="Sutsse"    ]<-"Switzerland"               
cop.example$Delegation[cop.example$Delegation=="Suisse" ]<-"Switzerland"                                           
cop.example$Delegation[cop.example$Delegation=="Tchad"  ]<-"Chad"                                             
cop.example$Delegation[cop.example$Delegation=="Thailand" ]<-"Thailand"  
cop.example$Delegation[cop.example$Delegation=="Thailande (Suite)" ]<-"Thailand"   
cop.example$Delegation[cop.example$Delegation=="Jamahiriya Arabe Libyenne"]<-"Libyan Arab Jamahiriya"       
cop.example$Delegation[cop.example$Delegation=="Libya"]<-"Libyan Arab Jamahiriya"
cop.example$Delegation[cop.example$Delegation=="Togo"   ]<-"Togo"                                            
cop.example$Delegation[cop.example$Delegation=="Trinite-Et-Tobago"   ]<-"Trinidad And Tobago"
cop.example$Delegation[cop.example$Delegation=="Trinite-Et- Tobago"  ]<-"Trinidad And Tobago"
cop.example$Delegation[cop.example$Delegation=="Tunisie"   ]<-"Tunisia"   
cop.example$Delegation[cop.example$Delegation=="Tchecoslovaquie" ] <-"Czechoslovakia"                                
cop.example$Delegation[cop.example$Delegation=="Thailande"    ]<-"Thailand" 
cop.example$Delegation[cop.example$Delegation=="Turkmenistan" ]<-"Turkmenistan"                                     
cop.example$Delegation[cop.example$Delegation=="Uruguay"     ]<-"Uruguay"                                      
cop.example$Delegation[cop.example$Delegation=="Vanuatu"    ]<-"Vanuatu"    
cop.example$Delegation[cop.example$Delegation=="Venezuela (Bolivarian Republic Of)"]<-"Venezuela"
cop.example$Delegation[cop.example$Delegation=="Venezuela"    ]<-"Venezuela" 
cop.example$Delegation[cop.example$Delegation=="Venezuela (Suite)"    ]<-"Venezuela" 
cop.example$Delegation[cop.example$Delegation=="Vietnam" ]<-"Viet Nam" 
cop.example$Delegation[cop.example$Delegation=="Viet Nam." ]<-"Viet Nam"                                          
cop.example$Delegation[cop.example$Delegation=="Yemen"    ]<-"Yemen"                                          
cop.example$Delegation[cop.example$Delegation=="Zaire"   ]<-"Democratic Republic of The Congo"                
cop.example$Delegation[cop.example$Delegation=="Zambie"  ]<-"Zambia"                                           
cop.example$Delegation[cop.example$Delegation=="Zimbabwe"]<-"Zimbabwe"
cop.example$Delegation[cop.example$Delegation=="Afrique Du Sud"    ]<-"South Africa"                                                                                                                     
cop.example$Delegation[cop.example$Delegation=="Gabon"   ]<-"Gabon"                                                                                                                              
cop.example$Delegation[cop.example$Delegation=="Haiti"       ]<-"Haiti"                                                                                                                           
cop.example$Delegation[cop.example$Delegation=="Tran (Islamic Republic Of)"  ]<-"Iran (Islamic Republic Of)" 
cop.example$Delegation[cop.example$Delegation=="Iran (Republique Islamique De)"  ]<-"Iran (Islamic Republic Of)"   
cop.example$Delegation[cop.example$Delegation=="Iran (Republique Islamique D’)"    ]<-"Iran (Islamic Republic Of)"                                  
cop.example$Delegation[cop.example$Delegation=="Iraq"     ]<-"Iraq"                                                                                                                              
cop.example$Delegation[cop.example$Delegation=="Israel"     ]<-"Israel"  
cop.example$Delegation[cop.example$Delegation=="The Former Yugoslay Republic Of Macedonia" ]<-"The Former Yugoslav Republic Of Macedonia" 
cop.example$Delegation[cop.example$Delegation=="North Macedonia"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                                            
cop.example$Delegation[cop.example$Delegation=="L'ex-Republique Yougoslave De Macedoi!"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                               
cop.example$Delegation[cop.example$Delegation=="Madagascar"          ]<-"Madagascar"                                                                                                                  
cop.example$Delegation[cop.example$Delegation=="Republique Dominicaine"    ]<-"Domnican Republic"                                                                                                             
cop.example$Delegation[cop.example$Delegation=="Saint-Siege"    ]<-"Holy See" 
cop.example$Delegation[cop.example$Delegation=="Saint Siege"     ]<-"Holy See"                                                                                                           
cop.example$Delegation[cop.example$Delegation=="Singapour"   ]<-"Singapore"                                                                                                                          
cop.example$Delegation[cop.example$Delegation=="Swaziland"   ]<-"Swaziland"      
cop.example$Delegation[cop.example$Delegation=="Türkiye"   ]<-"Turkey"                                                    
cop.example$Delegation[cop.example$Delegation=="Turquie"     ]<-"Turkey"                                                                                                                           
cop.example$Delegation[cop.example$Delegation=="Ukrainian Soviet Socialist Republic"]<-"Ukraine" 
cop.example$Delegation[cop.example$Delegation=="Ukraine"]<-"Ukraine" 
cop.example$Delegation[cop.example$Delegation=="Union Des Republiques Socialistes Sovietiques" ]<-"Union of Soviet Socialist Republics"
cop.example$Delegation[cop.example$Delegation=="Yougoslavie" ]<-"Yugoslavia"
cop.example$Delegation[cop.example$Delegation=="Swedan" ]<-"Sweden"                                             
cop.example$Delegation[cop.example$Delegation=="Solomon Islands (Continued0" ]<-"Solomon Islands"                                            
cop.example$Delegation[cop.example$Delegation=="Serbia and Montenegro" ]<-"Serbia"                                            
cop.example$Delegation[cop.example$Delegation=="Seychelles (Continued0" ]<-"Seychelles"                                          
cop.example$Delegation[cop.example$Delegation=="Republic of Cabo Verde"]<-"Cape Verde"                                     
cop.example$Delegation[cop.example$Delegation=="Cote D Ivoire"]<-"Cote D'ivoire"                                         
cop.example$Delegation[cop.example$Delegation=="Côte D`Ivoire"]<-"Cote D'ivoire"                               
cop.example$Delegation[cop.example$Delegation=="Domnican Republic"]<-"Dominican Republic" 
cop.example$Delegation[cop.example$Delegation=="Eswatini"]<-"Swaziland" 
cop.example$Delegation[cop.example$Delegation=="Holysee" ]<-"Holy See" 
cop.example$Delegation[cop.example$Delegation=="Kazakstan"]<-"Kazakhstan"
cop.example$Delegation[cop.example$Delegation=="Moldova, Republic Of"]<- "Moldova"   
cop.example$Delegation[cop.example$Delegation=="Flj1" ]<-"Fiji"  
cop.example$Delegation[cop.example$Delegation=="Guiea"]<-"Guinea"                                           
cop.example$Delegation[cop.example$Delegation=="Georgla"]<-"Georgia"                                          
cop.example$Delegation[cop.example$Delegation=="State of Palestine"]<-"Palestine" 
cop.example$Delegation[cop.example$Delegation=="Nive"]<-"Niue"                                                        
cop.example$Delegation[cop.example$Delegation=="Cape Verde" ]<-"Cabo Verde" 

#fix a few minor over-capitalization issues
cop.example$Delegation<-gsub(" Of ", " of ",cop.example$Delegation)
cop.example$Delegation<-gsub(" Of)", " of)",cop.example$Delegation)
cop.example$Delegation<-gsub(" And ", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub(" For ", " for ",cop.example$Delegation)

#fix one remaining issue
cop.example$Delegation<-gsub("^+\\‘", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub("^+\\’", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub("\\‘+$", " and ",cop.example$Delegation)
cop.example$Delegation<-gsub("\\’+$", " and ",cop.example$Delegation)

#fix a few more cases
cop.example$Delegation[cop.example$Delegation=="Iran(Islamic Republic of)" ]<-"Iran (Islamic Republic of)" 
cop.example$Delegation[cop.example$Delegation=="Netherlands (Kingdom of The)"]<-"Netherlands" 
cop.example$Delegation[cop.example$Delegation==" and Grenada"]<-"Grenada"                                         
cop.example$Delegation[cop.example$Delegation==" and Kenya"]<-"Kenya"   
cop.example$Group_Type[cop.example$Delegation=="Organisation Mondiale Du Commerce (Omc)"]<-"Specialized agencies and related organizations"  
cop.example$Delegation[cop.example$Delegation=="Republic of Moldova"]<- "Moldova" 
cop.example$Delegation[cop.example$Delegation=="Republic of Cabo Verde"]<- "Cabo Verde" 
cop.example$Delegation[cop.example$Delegation=="Serbia and Montenegro" ]<-"Serbia"  
cop.example$Delegation[cop.example$Delegation=="State of Palestine"]<-"Palestine" 
cop.example$Delegation[cop.example$Delegation=="Iran (Islamic Republic of)"]<-"Iran"                                                 
cop.example$Delegation[cop.example$Delegation=="Rhanda"]<-"Rwanda"   
cop.example$Delegation[cop.example$Delegation=="Dominique"]<-"Dominica" 

#Further standardization
cop.example$Delegation<-gsub("\\("," (",cop.example$Delegation)
cop.example$Delegation<-gsub("\\)",") ",cop.example$Delegation)
cop.example$Delegation<-gsub("\\’","'",cop.example$Delegation)
cop.example$Delegation<-gsub("\\( ","(",cop.example$Delegation)
cop.example$Delegation<-gsub(" \\)",")",cop.example$Delegation)
cop.example$Delegation<-str_squish(cop.example$Delegation)
cop.example$Delegation<-trimws(cop.example$Delegation, which = "both")

#create binary vars
cop.example$IGO<-0
cop.example$IGO[cop.example$Group_Type=="Intergovernmental organizations"]<-1
cop.example$NGO<-0
cop.example$NGO[cop.example$Group_Type=="Non-governmental organizations"]<-1
cop.example$Observer<-0
cop.example$Observer[cop.example$Group_Type=="Observer States"]<-1
cop.example$Party<-0
cop.example$Party[cop.example$Group_Type=="Parties"]<-1
cop.example$IO<-0
cop.example$IO[cop.example$Group_Type=="Specialized agencies and related organizations"]<-1
cop.example$IO[cop.example$Group_Type=="United Nations Secretariat units and bodies"]<-1


#re-combine
cops.data.translated<-rbind(cops.data.translated.old,cop.example)

####################################################
############Fix Issue Across All Datasets###########
####################################################

#make sure Botswana is correct
precops.data$Delegation<-ifelse(precops.data$Delegation=="Bostwana","Botswana",precops.data$Delegation)
precops.data.translated$Delegation<-ifelse(precops.data.translated$Delegation=="Bostwana","Botswana",precops.data.translated$Delegation)
cops.data$Delegation<-ifelse(cops.data$Delegation=="Bostwana","Botswana",cops.data$Delegation)
cops.data.translated$Delegation<-ifelse(cops.data.translated$Delegation=="Bostwana","Botswana",cops.data.translated$Delegation)


#####################################################################
#######################Add Country-Codes#############################
#####################################################################

#subset to only party and observer cases
cops.data.sub<-subset(cops.data,(cops.data$Observer==1 | cops.data$Party==1))
precops.data.sub<-subset(precops.data,(precops.data$Observer==1 | precops.data$Party==1))
cops.data.translated.sub<-subset(cops.data.translated,(cops.data.translated$Observer==1 | cops.data.translated$Party==1))
precops.data.translated.sub<-subset(precops.data.translated,(precops.data.translated$Observer==1 | precops.data.translated$Party==1))

#get full list of countries
countries<-sort(unique(c(unique(cops.data.sub$Delegation), unique(precops.data.sub$Delegation), unique(cops.data.translated.sub$Delegation), unique(precops.data.translated.sub$Delegation))))

#Add COW and then ISO country codes to State/Party delegations
cops.data$Delegation_COW<-NA
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Afghanistan",700,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Albania",339,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Algeria",615,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Andorra",232,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Angola",540,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Antigua and Barbuda",58,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Argentina",160,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Armenia",371,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Australia",900,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Austria",305,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Azerbaijan",373,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bahamas",31,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bahrain",692,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bangladesh",771,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Barbados",53,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Belarus",370,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Belgium",211,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Belize",80,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Benin",434,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bhutan",760,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bolivia",145,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bosnia and Herzegovina",346,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Botswana",571,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Brazil",140,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Brunei Darussalam",835,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Bulgaria",355,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Burkina Faso",439,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Burundi",516,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cabo Verde",402,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cambodia",811,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cameroon",471,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Canada",20,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Central African Republic",482,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Chad",483,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Chile",155,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="China",710,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Colombia",100,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Comoros",581,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Congo",484,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cook Islands", NA, cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Costa Rica",94,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cote D'ivoire",437,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Croatia",344,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cuba",40,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Cyprus",352,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Czech Republic",316,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Czechoslovakia",315,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Democratic People's Republic of Korea",731,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Democratic Republic of The Congo",490,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Denmark",390,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Djibouti",522,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Dominica",54,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Dominican Republic",42,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Ecuador",130,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Egypt",651,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="El Salvador",92,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Equatorial Guinea",411,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Eritrea",531,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Estonia",366,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Ethiopia",530,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="European Community",NA,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="European Union",NA,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Fiji",950,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Finland",375,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="France",220,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Gabon",481,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Gambia",420,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Georgia",372,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Germany",255,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Ghana",452,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Greece",350,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Grenada",55,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Guatemala",90,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Guinea",438,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Guinea-Bissau",404,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Guyana",110,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Haiti",41,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Holy See",NA,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Honduras",91,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Hungary",310,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Iceland",395,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="India",750,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Indonesia",850,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Iran",630,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Iraq",645,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Ireland",205,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Israel",666,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Italy",325,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Jamaica",51,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Japan",740,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Jordan",663,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Kazakhstan",705,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Kenya",501,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Kiribati",946,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Kuwait",690,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Kyrgyzstan",703,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Lao Peoples Democratic Republic",812,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Latvia",367,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Lebanon",660,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Lesotho",570,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Liberia",450,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Libyan Arab Jamahiriya",620,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Liechtenstein",223,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Lithuania",368,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Luxembourg",212,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Madagascar",580,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Malawi",553,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Malaysia",820,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Maldives",781,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Mali",432,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Malta",338,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Marshall Islands",983,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Mauritania",435,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Mauritius",590,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Mexico",70,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Micronesia (Federated States of)",987,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Moldova",359,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Monaco",221,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Mongolia",712,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Montenegro",341,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Morocco",600,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Mozambique",541,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Myanmar",775,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Namibia",565,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Nauru",970,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Nepal",790,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Netherlands",210,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="New Zealand",920,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Nicaragua",93,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Niger",436,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Nigeria",475,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Niue",NA,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Norway",385,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Oman",698,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Pakistan",770,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Palau",986,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Palestine",NA,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Panama",95,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Papua New Guinea",910,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Paraguay",150,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Peru",135,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Philippines",840,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Poland",290,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Portugal",235,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Qatar",694,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Republic of Korea",732,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Romania",360,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Russian Federation",365,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Rwanda",517,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Saint Kitts and Nevis",60,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Saint Lucia",56,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Saint Vincent and The Grenadines",57,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Samoa",990,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="San Marino",331,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Sao Tome and Principe",403,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Saudi Arabia",670,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Senegal",433,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Serbia",345,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Seychelles",591,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Sierra Leone",451,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Singapore",830,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Slovak Republic",317,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Slovenia",349,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Solomon Islands",940,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Somalia",520,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="South Africa",560,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="South Sudan",626,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Spain",230,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Sri Lanka",780,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Sudan",625,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Suriname",115,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Swaziland",572,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Sweden",380,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Switzerland",225,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Syrian Arab Republic",652,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Tajikistan",702,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Thailand",800,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="The Former Yugoslav Republic of Macedonia",343,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Timor-Leste",860,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Togo",461,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Tonga",955,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Trinidad and Tobago",52,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Tunisia",616,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Turkey",640,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Turkmenistan",701,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Tuvalu",947,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Uganda",500,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Ukraine",369,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Union of Soviet Socialist Republics",365,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="United Arab Emirates",696,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="United Kingdom of Great Britain and Northern Ireland",200,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="United Republic of Tanzania",510,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="United States of America",2,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Uruguay",165,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Uzbekistan",704,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Vanuatu",935,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Venezuela",101,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Viet Nam",817,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Yemen",679,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Yugoslavia",345,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Zambia",551,cops.data$Delegation_COW)
cops.data$Delegation_COW<-ifelse(cops.data$Delegation=="Zimbabwe",552,cops.data$Delegation_COW)

precops.data$Delegation_COW<-NA
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Afghanistan",700,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Albania",339,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Algeria",615,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Andorra",232,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Angola",540,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Antigua and Barbuda",58,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Argentina",160,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Armenia",371,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Australia",900,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Austria",305,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Azerbaijan",373,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bahamas",31,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bahrain",692,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bangladesh",771,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Barbados",53,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Belarus",370,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Belgium",211,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Belize",80,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Benin",434,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bhutan",760,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bolivia",145,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bosnia and Herzegovina",346,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Botswana",571,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Brazil",140,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Brunei Darussalam",835,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Bulgaria",355,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Burkina Faso",439,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Burundi",516,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cabo Verde",402,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cambodia",811,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cameroon",471,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Canada",20,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Central African Republic",482,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Chad",483,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Chile",155,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="China",710,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Colombia",100,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Comoros",581,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Congo",484,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cook Islands", NA, precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Costa Rica",94,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cote D'ivoire",437,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Croatia",344,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cuba",40,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Cyprus",352,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Czech Republic",316,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Czechoslovakia",315,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Democratic People's Republic of Korea",731,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Democratic Republic of The Congo",490,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Denmark",390,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Djibouti",522,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Dominica",54,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Dominican Republic",42,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Ecuador",130,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Egypt",651,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="El Salvador",92,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Equatorial Guinea",411,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Eritrea",531,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Estonia",366,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Ethiopia",530,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="European Community",NA,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="European Union",NA,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Fiji",950,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Finland",375,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="France",220,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Gabon",481,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Gambia",420,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Georgia",372,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Germany",255,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Ghana",452,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Greece",350,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Grenada",55,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Guatemala",90,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Guinea",438,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Guinea-Bissau",404,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Guyana",110,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Haiti",41,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Holy See",NA,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Honduras",91,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Hungary",310,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Iceland",395,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="India",750,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Indonesia",850,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Iran",630,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Iraq",645,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Ireland",205,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Israel",666,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Italy",325,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Jamaica",51,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Japan",740,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Jordan",663,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Kazakhstan",705,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Kenya",501,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Kiribati",946,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Kuwait",690,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Kyrgyzstan",703,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Lao Peoples Democratic Republic",812,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Latvia",367,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Lebanon",660,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Lesotho",570,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Liberia",450,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Libyan Arab Jamahiriya",620,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Liechtenstein",223,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Lithuania",368,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Luxembourg",212,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Madagascar",580,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Malawi",553,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Malaysia",820,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Maldives",781,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Mali",432,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Malta",338,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Marshall Islands",983,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Mauritania",435,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Mauritius",590,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Mexico",70,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Micronesia (Federated States of)",987,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Moldova",359,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Monaco",221,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Mongolia",712,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Montenegro",341,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Morocco",600,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Mozambique",541,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Myanmar",775,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Namibia",565,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Nauru",970,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Nepal",790,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Netherlands",210,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="New Zealand",920,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Nicaragua",93,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Niger",436,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Nigeria",475,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Niue",NA,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Norway",385,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Oman",698,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Pakistan",770,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Palau",986,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Palestine",NA,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Panama",95,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Papua New Guinea",910,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Paraguay",150,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Peru",135,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Philippines",840,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Poland",290,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Portugal",235,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Qatar",694,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Republic of Korea",732,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Romania",360,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Russian Federation",365,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Rwanda",517,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Saint Kitts and Nevis",60,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Saint Lucia",56,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Saint Vincent and The Grenadines",57,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Samoa",990,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="San Marino",331,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Sao Tome and Principe",403,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Saudi Arabia",670,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Senegal",433,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Serbia",345,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Seychelles",591,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Sierra Leone",451,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Singapore",830,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Slovak Republic",317,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Slovenia",349,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Solomon Islands",940,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Somalia",520,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="South Africa",560,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="South Sudan",626,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Spain",230,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Sri Lanka",780,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Sudan",625,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Suriname",115,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Swaziland",572,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Sweden",380,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Switzerland",225,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Syrian Arab Republic",652,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Tajikistan",702,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Thailand",800,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="The Former Yugoslav Republic of Macedonia",343,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Timor-Leste",860,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Togo",461,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Tonga",955,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Trinidad and Tobago",52,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Tunisia",616,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Turkey",640,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Turkmenistan",701,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Tuvalu",947,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Uganda",500,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Ukraine",369,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Union of Soviet Socialist Republics",365,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="United Arab Emirates",696,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="United Kingdom of Great Britain and Northern Ireland",200,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="United Republic of Tanzania",510,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="United States of America",2,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Uruguay",165,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Uzbekistan",704,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Vanuatu",935,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Venezuela",101,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Viet Nam",817,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Yemen",679,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Yugoslavia",345,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Zambia",551,precops.data$Delegation_COW)
precops.data$Delegation_COW<-ifelse(precops.data$Delegation=="Zimbabwe",552,precops.data$Delegation_COW)

cops.data.translated$Delegation_COW<-NA
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Afghanistan",700,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Albania",339,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Algeria",615,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Andorra",232,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Angola",540,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Antigua and Barbuda",58,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Argentina",160,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Armenia",371,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Australia",900,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Austria",305,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Azerbaijan",373,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bahamas",31,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bahrain",692,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bangladesh",771,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Barbados",53,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Belarus",370,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Belgium",211,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Belize",80,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Benin",434,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bhutan",760,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bolivia",145,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bosnia and Herzegovina",346,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Botswana",571,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Brazil",140,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Brunei Darussalam",835,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Bulgaria",355,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Burkina Faso",439,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Burundi",516,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cabo Verde",402,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cambodia",811,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cameroon",471,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Canada",20,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Central African Republic",482,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Chad",483,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Chile",155,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="China",710,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Colombia",100,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Comoros",581,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Congo",484,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cook Islands", NA, cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Costa Rica",94,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cote D'ivoire",437,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Croatia",344,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cuba",40,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Cyprus",352,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Czech Republic",316,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Czechoslovakia",315,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Democratic People's Republic of Korea",731,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Democratic Republic of The Congo",490,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Denmark",390,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Djibouti",522,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Dominica",54,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Dominican Republic",42,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Ecuador",130,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Egypt",651,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="El Salvador",92,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Equatorial Guinea",411,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Eritrea",531,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Estonia",366,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Ethiopia",530,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="European Community",NA,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="European Union",NA,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Fiji",950,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Finland",375,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="France",220,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Gabon",481,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Gambia",420,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Georgia",372,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Germany",255,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Ghana",452,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Greece",350,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Grenada",55,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Guatemala",90,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Guinea",438,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Guinea-Bissau",404,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Guyana",110,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Haiti",41,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Holy See",NA,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Honduras",91,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Hungary",310,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Iceland",395,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="India",750,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Indonesia",850,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Iran",630,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Iraq",645,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Ireland",205,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Israel",666,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Italy",325,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Jamaica",51,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Japan",740,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Jordan",663,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Kazakhstan",705,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Kenya",501,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Kiribati",946,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Kuwait",690,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Kyrgyzstan",703,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Lao Peoples Democratic Republic",812,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Latvia",367,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Lebanon",660,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Lesotho",570,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Liberia",450,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Libyan Arab Jamahiriya",620,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Liechtenstein",223,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Lithuania",368,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Luxembourg",212,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Madagascar",580,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Malawi",553,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Malaysia",820,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Maldives",781,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Mali",432,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Malta",338,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Marshall Islands",983,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Mauritania",435,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Mauritius",590,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Mexico",70,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Micronesia (Federated States of)",987,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Moldova",359,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Monaco",221,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Mongolia",712,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Montenegro",341,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Morocco",600,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Mozambique",541,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Myanmar",775,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Namibia",565,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Nauru",970,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Nepal",790,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Netherlands",210,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="New Zealand",920,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Nicaragua",93,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Niger",436,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Nigeria",475,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Niue",NA,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Norway",385,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Oman",698,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Pakistan",770,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Palau",986,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Palestine",NA,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Panama",95,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Papua New Guinea",910,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Paraguay",150,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Peru",135,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Philippines",840,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Poland",290,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Portugal",235,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Qatar",694,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Republic of Korea",732,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Romania",360,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Russian Federation",365,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Rwanda",517,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Saint Kitts and Nevis",60,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Saint Lucia",56,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Saint Vincent and The Grenadines",57,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Samoa",990,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="San Marino",331,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Sao Tome and Principe",403,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Saudi Arabia",670,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Senegal",433,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Serbia",345,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Seychelles",591,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Sierra Leone",451,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Singapore",830,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Slovak Republic",317,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Slovenia",349,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Solomon Islands",940,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Somalia",520,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="South Africa",560,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="South Sudan",626,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Spain",230,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Sri Lanka",780,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Sudan",625,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Suriname",115,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Swaziland",572,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Sweden",380,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Switzerland",225,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Syrian Arab Republic",652,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Tajikistan",702,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Thailand",800,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="The Former Yugoslav Republic of Macedonia",343,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Timor-Leste",860,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Togo",461,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Tonga",955,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Trinidad and Tobago",52,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Tunisia",616,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Turkey",640,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Turkmenistan",701,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Tuvalu",947,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Uganda",500,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Ukraine",369,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Union of Soviet Socialist Republics",365,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="United Arab Emirates",696,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="United Kingdom of Great Britain and Northern Ireland",200,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="United Republic of Tanzania",510,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="United States of America",2,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Uruguay",165,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Uzbekistan",704,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Vanuatu",935,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Venezuela",101,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Viet Nam",817,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Yemen",679,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Yugoslavia",345,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Zambia",551,cops.data.translated$Delegation_COW)
cops.data.translated$Delegation_COW<-ifelse(cops.data.translated$Delegation=="Zimbabwe",552,cops.data.translated$Delegation_COW)

precops.data.translated$Delegation_COW<-NA
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Afghanistan",700,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Albania",339,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Algeria",615,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Andorra",232,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Angola",540,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Antigua and Barbuda",58,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Argentina",160,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Armenia",371,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Australia",900,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Austria",305,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Azerbaijan",373,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bahamas",31,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bahrain",692,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bangladesh",771,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Barbados",53,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Belarus",370,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Belgium",211,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Belize",80,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Benin",434,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bhutan",760,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bolivia",145,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bosnia and Herzegovina",346,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Botswana",571,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Brazil",140,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Brunei Darussalam",835,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Bulgaria",355,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Burkina Faso",439,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Burundi",516,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cabo Verde",402,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cambodia",811,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cameroon",471,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Canada",20,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Central African Republic",482,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Chad",483,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Chile",155,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="China",710,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Colombia",100,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Comoros",581,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Congo",484,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cook Islands", NA, precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Costa Rica",94,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cote D'ivoire",437,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Croatia",344,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cuba",40,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Cyprus",352,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Czech Republic",316,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Czechoslovakia",315,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Democratic People's Republic of Korea",731,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Democratic Republic of The Congo",490,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Denmark",390,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Djibouti",522,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Dominica",54,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Dominican Republic",42,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Ecuador",130,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Egypt",651,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="El Salvador",92,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Equatorial Guinea",411,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Eritrea",531,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Estonia",366,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Ethiopia",530,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="European Community",NA,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="European Union",NA,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Fiji",950,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Finland",375,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="France",220,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Gabon",481,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Gambia",420,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Georgia",372,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Germany",255,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Ghana",452,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Greece",350,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Grenada",55,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Guatemala",90,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Guinea",438,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Guinea-Bissau",404,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Guyana",110,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Haiti",41,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Holy See",NA,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Honduras",91,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Hungary",310,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Iceland",395,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="India",750,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Indonesia",850,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Iran",630,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Iraq",645,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Ireland",205,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Israel",666,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Italy",325,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Jamaica",51,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Japan",740,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Jordan",663,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Kazakhstan",705,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Kenya",501,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Kiribati",946,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Kuwait",690,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Kyrgyzstan",703,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Lao Peoples Democratic Republic",812,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Latvia",367,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Lebanon",660,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Lesotho",570,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Liberia",450,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Libyan Arab Jamahiriya",620,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Liechtenstein",223,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Lithuania",368,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Luxembourg",212,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Madagascar",580,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Malawi",553,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Malaysia",820,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Maldives",781,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Mali",432,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Malta",338,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Marshall Islands",983,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Mauritania",435,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Mauritius",590,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Mexico",70,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Micronesia (Federated States of)",987,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Moldova",359,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Monaco",221,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Mongolia",712,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Montenegro",341,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Morocco",600,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Mozambique",541,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Myanmar",775,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Namibia",565,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Nauru",970,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Nepal",790,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Netherlands",210,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="New Zealand",920,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Nicaragua",93,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Niger",436,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Nigeria",475,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Niue",NA,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Norway",385,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Oman",698,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Pakistan",770,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Palau",986,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Palestine",NA,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Panama",95,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Papua New Guinea",910,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Paraguay",150,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Peru",135,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Philippines",840,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Poland",290,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Portugal",235,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Qatar",694,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Republic of Korea",732,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Romania",360,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Russian Federation",365,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Rwanda",517,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Saint Kitts and Nevis",60,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Saint Lucia",56,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Saint Vincent and The Grenadines",57,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Samoa",990,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="San Marino",331,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Sao Tome and Principe",403,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Saudi Arabia",670,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Senegal",433,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Serbia",345,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Seychelles",591,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Sierra Leone",451,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Singapore",830,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Slovak Republic",317,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Slovenia",349,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Solomon Islands",940,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Somalia",520,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="South Africa",560,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="South Sudan",626,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Spain",230,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Sri Lanka",780,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Sudan",625,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Suriname",115,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Swaziland",572,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Sweden",380,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Switzerland",225,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Syrian Arab Republic",652,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Tajikistan",702,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Thailand",800,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="The Former Yugoslav Republic of Macedonia",343,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Timor-Leste",860,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Togo",461,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Tonga",955,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Trinidad and Tobago",52,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Tunisia",616,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Turkey",640,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Turkmenistan",701,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Tuvalu",947,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Uganda",500,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Ukraine",369,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Union of Soviet Socialist Republics",365,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="United Arab Emirates",696,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="United Kingdom of Great Britain and Northern Ireland",200,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="United Republic of Tanzania",510,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="United States of America",2,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Uruguay",165,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Uzbekistan",704,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Vanuatu",935,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Venezuela",101,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Viet Nam",817,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Yemen",679,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Yugoslavia",345,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Zambia",551,precops.data.translated$Delegation_COW)
precops.data.translated$Delegation_COW<-ifelse(precops.data.translated$Delegation=="Zimbabwe",552,precops.data.translated$Delegation_COW)

cops.data$Delegation_ISO<-NA
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Afghanistan","AFG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Albania","ALB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Algeria","DZA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Andorra","AND",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Angola","AGO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Antigua and Barbuda","ATG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Argentina","ARG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Armenia","ARM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Australia","AUS",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Austria","AUT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Azerbaijan","AZE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bahamas","BHS",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bahrain","BHR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bangladesh","BGD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Barbados","BRB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Belarus","BLR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Belgium","BEL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Belize","BLZ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Benin","BEN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bhutan","BTN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bolivia","BOL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bosnia and Herzegovina","BIH",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Botswana","BWA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Brazil","BRA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Brunei Darussalam","BRN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Bulgaria","BGR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Burkina Faso","BFA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Burundi","BDI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cabo Verde","CPV",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cambodia","KHM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cameroon","CMR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Canada","CAN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Central African Republic","CAF",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Chad","TCD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Chile","CHL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="China","CHN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Colombia","COL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Comoros","COM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Congo","COG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cook Islands", "COK", cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Costa Rica","CRI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cote D'ivoire","CIV",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Croatia","HRV",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cuba","CUB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Cyprus","CYP",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Czech Republic","CZE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Czechoslovakia","CSK",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Democratic People's Republic of Korea","PRK",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Democratic Republic of The Congo","COD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Denmark","DNK",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Djibouti","DJI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Dominica","DMA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Dominican Republic","DOM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Ecuador","ECU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Egypt","EGY",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="El Salvador","SLV",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Equatorial Guinea","GNQ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Eritrea","ERI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Estonia","EST",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Ethiopia","ETH",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="European Community",NA,cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="European Union","EUE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Fiji","FJI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Finland","FIN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="France","FRA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Gabon","GAB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Gambia","GMB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Georgia","GEO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Germany","DEU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Ghana","GHA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Greece","GRC",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Grenada","GRD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Guatemala","GTM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Guinea","GIN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Guinea-Bissau","GNB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Guyana","GUY",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Haiti","HTI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Holy See","VAT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Honduras","HND",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Hungary","HUN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Iceland","ISL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="India","IND",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Indonesia","IDN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Iran","IRN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Iraq","IRQ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Ireland","IRL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Israel","ISR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Italy","ITA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Jamaica","JAM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Japan","JPN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Jordan","JOR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Kazakhstan","KAZ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Kenya","KEN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Kiribati","KIR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Kuwait","KWT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Kyrgyzstan","KGZ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Lao Peoples Democratic Republic","LAO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Latvia","LVA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Lebanon","LBN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Lesotho","LSO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Liberia","LBR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Libyan Arab Jamahiriya","LBY",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Liechtenstein","LIE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Lithuania","LTU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Luxembourg","LUX",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Madagascar","MDG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Malawi","MWI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Malaysia","MYS",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Maldives","MDV",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Mali","MLI",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Malta","MLT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Marshall Islands","MHL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Mauritania","MRT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Mauritius","MUS",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Mexico","MEX",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Micronesia (Federated States of)","FSM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Moldova","MDA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Monaco","MCO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Mongolia","MNG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Montenegro","MNE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Morocco","MAR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Mozambique","MOZ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Myanmar","MMR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Namibia","NAM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Nauru","NRU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Nepal","NPL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Netherlands","NLD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="New Zealand","NZL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Nicaragua","NIC",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Niger","NER",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Nigeria","NGA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Niue","NIU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Norway","NOR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Oman","OMN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Pakistan","PAK",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Palau","PLW",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Palestine","PSE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Panama","PNA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Papua New Guinea","PNG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Paraguay","PRY",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Peru","PER",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Philippines","PHL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Poland","POL",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Portugal","PRT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Qatar","QAT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Republic of Korea","KOR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Romania","ROU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Russian Federation","RUS",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Rwanda","RWA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Saint Kitts and Nevis","KNA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Saint Lucia","LCA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Saint Vincent and The Grenadines","VCT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Samoa","WSM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="San Marino","SMR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Sao Tome and Principe","STP",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Saudi Arabia","SAU",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Senegal","SEN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Serbia","SRB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Seychelles","SYC",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Sierra Leone","SLE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Singapore","SGP",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Slovak Republic","SVK",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Slovenia","SVN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Solomon Islands","SLB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Somalia","SOM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="South Africa","ZAF",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="South Sudan","SSD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Spain","ESP",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Sri Lanka","LKA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Sudan","SDN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Suriname","SUR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Swaziland","SWZ",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Sweden","SWE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Switzerland","CHE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Syrian Arab Republic","SYR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Tajikistan","TJK",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Thailand","THA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="The Former Yugoslav Republic of Macedonia","MKD",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Timor-Leste","TLS",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Togo","TGO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Tonga","TON",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Trinidad and Tobago","TTO",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Tunisia","TUN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Turkey","TUR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Turkmenistan","TKM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Tuvalu","TUV",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Uganda","UGA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Ukraine","UKR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Union of Soviet Socialist Republics","SUN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="United Arab Emirates","ARE",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="United Kingdom of Great Britain and Northern Ireland","GBR",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="United Republic of Tanzania","TZA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="United States of America","USA",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Uruguay","URY",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Uzbekistan","UZB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Vanuatu","VUT",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Venezuela","VEN",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Viet Nam","VNM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Yemen","YEM",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Yugoslavia","YUG",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Zambia","ZMB",cops.data$Delegation_ISO)
cops.data$Delegation_ISO<-ifelse(cops.data$Delegation=="Zimbabwe","ZWE",cops.data$Delegation_ISO)

precops.data$Delegation_ISO<-NA
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Afghanistan","AFG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Albania","ALB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Algeria","DZA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Andorra","AND",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Angola","AGO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Antigua and Barbuda","ATG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Argentina","ARG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Armenia","ARM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Australia","AUS",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Austria","AUT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Azerbaijan","AZE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bahamas","BHS",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bahrain","BHR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bangladesh","BGD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Barbados","BRB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Belarus","BLR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Belgium","BEL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Belize","BLZ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Benin","BEN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bhutan","BTN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bolivia","BOL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bosnia and Herzegovina","BIH",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Botswana","BWA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Brazil","BRA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Brunei Darussalam","BRN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Bulgaria","BGR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Burkina Faso","BFA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Burundi","BDI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cabo Verde","CPV",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cambodia","KHM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cameroon","CMR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Canada","CAN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Central African Republic","CAF",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Chad","TCD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Chile","CHL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="China","CHN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Colombia","COL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Comoros","COM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Congo","COG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cook Islands", "COK", precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Costa Rica","CRI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cote D'ivoire","CIV",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Croatia","HRV",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cuba","CUB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Cyprus","CYP",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Czech Republic","CZE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Czechoslovakia","CSK",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Democratic People's Republic of Korea","PRK",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Democratic Republic of The Congo","COD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Denmark","DNK",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Djibouti","DJI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Dominica","DMA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Dominican Republic","DOM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Ecuador","ECU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Egypt","EGY",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="El Salvador","SLV",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Equatorial Guinea","GNQ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Eritrea","ERI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Estonia","EST",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Ethiopia","ETH",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="European Community",NA,precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="European Union","EUE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Fiji","FJI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Finland","FIN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="France","FRA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Gabon","GAB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Gambia","GMB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Georgia","GEO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Germany","DEU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Ghana","GHA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Greece","GRC",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Grenada","GRD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Guatemala","GTM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Guinea","GIN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Guinea-Bissau","GNB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Guyana","GUY",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Haiti","HTI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Holy See","VAT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Honduras","HND",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Hungary","HUN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Iceland","ISL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="India","IND",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Indonesia","IDN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Iran","IRN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Iraq","IRQ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Ireland","IRL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Israel","ISR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Italy","ITA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Jamaica","JAM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Japan","JPN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Jordan","JOR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Kazakhstan","KAZ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Kenya","KEN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Kiribati","KIR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Kuwait","KWT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Kyrgyzstan","KGZ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Lao Peoples Democratic Republic","LAO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Latvia","LVA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Lebanon","LBN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Lesotho","LSO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Liberia","LBR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Libyan Arab Jamahiriya","LBY",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Liechtenstein","LIE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Lithuania","LTU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Luxembourg","LUX",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Madagascar","MDG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Malawi","MWI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Malaysia","MYS",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Maldives","MDV",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Mali","MLI",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Malta","MLT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Marshall Islands","MHL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Mauritania","MRT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Mauritius","MUS",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Mexico","MEX",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Micronesia (Federated States of)","FSM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Moldova","MDA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Monaco","MCO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Mongolia","MNG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Montenegro","MNE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Morocco","MAR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Mozambique","MOZ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Myanmar","MMR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Namibia","NAM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Nauru","NRU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Nepal","NPL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Netherlands","NLD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="New Zealand","NZL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Nicaragua","NIC",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Niger","NER",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Nigeria","NGA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Niue","NIU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Norway","NOR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Oman","OMN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Pakistan","PAK",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Palau","PLW",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Palestine","PSE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Panama","PNA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Papua New Guinea","PNG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Paraguay","PRY",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Peru","PER",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Philippines","PHL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Poland","POL",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Portugal","PRT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Qatar","QAT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Republic of Korea","KOR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Romania","ROU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Russian Federation","RUS",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Rwanda","RWA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Saint Kitts and Nevis","KNA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Saint Lucia","LCA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Saint Vincent and The Grenadines","VCT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Samoa","WSM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="San Marino","SMR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Sao Tome and Principe","STP",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Saudi Arabia","SAU",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Senegal","SEN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Serbia","SRB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Seychelles","SYC",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Sierra Leone","SLE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Singapore","SGP",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Slovak Republic","SVK",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Slovenia","SVN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Solomon Islands","SLB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Somalia","SOM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="South Africa","ZAF",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="South Sudan","SSD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Spain","ESP",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Sri Lanka","LKA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Sudan","SDN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Suriname","SUR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Swaziland","SWZ",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Sweden","SWE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Switzerland","CHE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Syrian Arab Republic","SYR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Tajikistan","TJK",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Thailand","THA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="The Former Yugoslav Republic of Macedonia","MKD",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Timor-Leste","TLS",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Togo","TGO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Tonga","TON",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Trinidad and Tobago","TTO",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Tunisia","TUN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Turkey","TUR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Turkmenistan","TKM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Tuvalu","TUV",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Uganda","UGA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Ukraine","UKR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Union of Soviet Socialist Republics","SUN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="United Arab Emirates","ARE",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="United Kingdom of Great Britain and Northern Ireland","GBR",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="United Republic of Tanzania","TZA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="United States of America","USA",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Uruguay","URY",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Uzbekistan","UZB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Vanuatu","VUT",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Venezuela","VEN",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Viet Nam","VNM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Yemen","YEM",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Yugoslavia","YUG",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Zambia","ZMB",precops.data$Delegation_ISO)
precops.data$Delegation_ISO<-ifelse(precops.data$Delegation=="Zimbabwe","ZWE",precops.data$Delegation_ISO)


cops.data.translated$Delegation_ISO<-NA
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Afghanistan","AFG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Albania","ALB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Algeria","DZA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Andorra","AND",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Angola","AGO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Antigua and Barbuda","ATG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Argentina","ARG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Armenia","ARM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Australia","AUS",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Austria","AUT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Azerbaijan","AZE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bahamas","BHS",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bahrain","BHR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bangladesh","BGD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Barbados","BRB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Belarus","BLR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Belgium","BEL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Belize","BLZ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Benin","BEN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bhutan","BTN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bolivia","BOL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bosnia and Herzegovina","BIH",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Botswana","BWA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Brazil","BRA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Brunei Darussalam","BRN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Bulgaria","BGR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Burkina Faso","BFA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Burundi","BDI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cabo Verde","CPV",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cambodia","KHM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cameroon","CMR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Canada","CAN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Central African Republic","CAF",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Chad","TCD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Chile","CHL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="China","CHN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Colombia","COL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Comoros","COM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Congo","COG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cook Islands", "COK", cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Costa Rica","CRI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cote D'ivoire","CIV",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Croatia","HRV",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cuba","CUB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Cyprus","CYP",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Czech Republic","CZE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Czechoslovakia","CSK",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Democratic People's Republic of Korea","PRK",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Democratic Republic of The Congo","COD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Denmark","DNK",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Djibouti","DJI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Dominica","DMA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Dominican Republic","DOM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Ecuador","ECU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Egypt","EGY",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="El Salvador","SLV",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Equatorial Guinea","GNQ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Eritrea","ERI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Estonia","EST",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Ethiopia","ETH",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="European Community",NA,cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="European Union","EUE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Fiji","FJI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Finland","FIN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="France","FRA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Gabon","GAB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Gambia","GMB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Georgia","GEO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Germany","DEU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Ghana","GHA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Greece","GRC",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Grenada","GRD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Guatemala","GTM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Guinea","GIN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Guinea-Bissau","GNB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Guyana","GUY",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Haiti","HTI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Holy See","VAT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Honduras","HND",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Hungary","HUN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Iceland","ISL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="India","IND",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Indonesia","IDN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Iran","IRN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Iraq","IRQ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Ireland","IRL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Israel","ISR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Italy","ITA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Jamaica","JAM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Japan","JPN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Jordan","JOR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Kazakhstan","KAZ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Kenya","KEN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Kiribati","KIR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Kuwait","KWT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Kyrgyzstan","KGZ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Lao Peoples Democratic Republic","LAO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Latvia","LVA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Lebanon","LBN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Lesotho","LSO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Liberia","LBR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Libyan Arab Jamahiriya","LBY",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Liechtenstein","LIE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Lithuania","LTU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Luxembourg","LUX",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Madagascar","MDG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Malawi","MWI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Malaysia","MYS",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Maldives","MDV",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Mali","MLI",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Malta","MLT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Marshall Islands","MHL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Mauritania","MRT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Mauritius","MUS",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Mexico","MEX",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Micronesia (Federated States of)","FSM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Moldova","MDA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Monaco","MCO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Mongolia","MNG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Montenegro","MNE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Morocco","MAR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Mozambique","MOZ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Myanmar","MMR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Namibia","NAM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Nauru","NRU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Nepal","NPL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Netherlands","NLD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="New Zealand","NZL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Nicaragua","NIC",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Niger","NER",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Nigeria","NGA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Niue","NIU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Norway","NOR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Oman","OMN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Pakistan","PAK",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Palau","PLW",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Palestine","PSE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Panama","PNA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Papua New Guinea","PNG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Paraguay","PRY",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Peru","PER",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Philippines","PHL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Poland","POL",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Portugal","PRT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Qatar","QAT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Republic of Korea","KOR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Romania","ROU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Russian Federation","RUS",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Rwanda","RWA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Saint Kitts and Nevis","KNA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Saint Lucia","LCA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Saint Vincent and The Grenadines","VCT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Samoa","WSM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="San Marino","SMR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Sao Tome and Principe","STP",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Saudi Arabia","SAU",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Senegal","SEN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Serbia","SRB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Seychelles","SYC",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Sierra Leone","SLE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Singapore","SGP",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Slovak Republic","SVK",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Slovenia","SVN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Solomon Islands","SLB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Somalia","SOM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="South Africa","ZAF",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="South Sudan","SSD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Spain","ESP",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Sri Lanka","LKA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Sudan","SDN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Suriname","SUR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Swaziland","SWZ",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Sweden","SWE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Switzerland","CHE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Syrian Arab Republic","SYR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Tajikistan","TJK",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Thailand","THA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="The Former Yugoslav Republic of Macedonia","MKD",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Timor-Leste","TLS",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Togo","TGO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Tonga","TON",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Trinidad and Tobago","TTO",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Tunisia","TUN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Turkey","TUR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Turkmenistan","TKM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Tuvalu","TUV",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Uganda","UGA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Ukraine","UKR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Union of Soviet Socialist Republics","SUN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="United Arab Emirates","ARE",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="United Kingdom of Great Britain and Northern Ireland","GBR",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="United Republic of Tanzania","TZA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="United States of America","USA",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Uruguay","URY",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Uzbekistan","UZB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Vanuatu","VUT",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Venezuela","VEN",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Viet Nam","VNM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Yemen","YEM",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Yugoslavia","YUG",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Zambia","ZMB",cops.data.translated$Delegation_ISO)
cops.data.translated$Delegation_ISO<-ifelse(cops.data.translated$Delegation=="Zimbabwe","ZWE",cops.data.translated$Delegation_ISO)


precops.data.translated$Delegation_ISO<-NA
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Afghanistan","AFG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Albania","ALB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Algeria","DZA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Andorra","AND",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Angola","AGO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Antigua and Barbuda","ATG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Argentina","ARG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Armenia","ARM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Australia","AUS",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Austria","AUT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Azerbaijan","AZE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bahamas","BHS",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bahrain","BHR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bangladesh","BGD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Barbados","BRB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Belarus","BLR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Belgium","BEL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Belize","BLZ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Benin","BEN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bhutan","BTN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bolivia","BOL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bosnia and Herzegovina","BIH",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Botswana","BWA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Brazil","BRA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Brunei Darussalam","BRN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Bulgaria","BGR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Burkina Faso","BFA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Burundi","BDI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cabo Verde","CPV",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cambodia","KHM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cameroon","CMR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Canada","CAN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Central African Republic","CAF",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Chad","TCD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Chile","CHL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="China","CHN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Colombia","COL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Comoros","COM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Congo","COG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cook Islands", "COK", precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Costa Rica","CRI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cote D'ivoire","CIV",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Croatia","HRV",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cuba","CUB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Cyprus","CYP",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Czech Republic","CZE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Czechoslovakia","CSK",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Democratic People's Republic of Korea","PRK",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Democratic Republic of The Congo","COD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Denmark","DNK",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Djibouti","DJI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Dominica","DMA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Dominican Republic","DOM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Ecuador","ECU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Egypt","EGY",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="El Salvador","SLV",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Equatorial Guinea","GNQ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Eritrea","ERI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Estonia","EST",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Ethiopia","ETH",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="European Community",NA,precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="European Union","EUE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Fiji","FJI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Finland","FIN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="France","FRA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Gabon","GAB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Gambia","GMB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Georgia","GEO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Germany","DEU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Ghana","GHA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Greece","GRC",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Grenada","GRD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Guatemala","GTM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Guinea","GIN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Guinea-Bissau","GNB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Guyana","GUY",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Haiti","HTI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Holy See","VAT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Honduras","HND",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Hungary","HUN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Iceland","ISL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="India","IND",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Indonesia","IDN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Iran","IRN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Iraq","IRQ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Ireland","IRL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Israel","ISR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Italy","ITA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Jamaica","JAM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Japan","JPN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Jordan","JOR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Kazakhstan","KAZ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Kenya","KEN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Kiribati","KIR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Kuwait","KWT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Kyrgyzstan","KGZ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Lao Peoples Democratic Republic","LAO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Latvia","LVA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Lebanon","LBN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Lesotho","LSO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Liberia","LBR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Libyan Arab Jamahiriya","LBY",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Liechtenstein","LIE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Lithuania","LTU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Luxembourg","LUX",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Madagascar","MDG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Malawi","MWI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Malaysia","MYS",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Maldives","MDV",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Mali","MLI",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Malta","MLT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Marshall Islands","MHL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Mauritania","MRT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Mauritius","MUS",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Mexico","MEX",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Micronesia (Federated States of)","FSM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Moldova","MDA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Monaco","MCO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Mongolia","MNG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Montenegro","MNE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Morocco","MAR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Mozambique","MOZ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Myanmar","MMR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Namibia","NAM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Nauru","NRU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Nepal","NPL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Netherlands","NLD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="New Zealand","NZL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Nicaragua","NIC",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Niger","NER",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Nigeria","NGA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Niue","NIU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Norway","NOR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Oman","OMN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Pakistan","PAK",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Palau","PLW",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Palestine","PSE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Panama","PNA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Papua New Guinea","PNG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Paraguay","PRY",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Peru","PER",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Philippines","PHL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Poland","POL",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Portugal","PRT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Qatar","QAT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Republic of Korea","KOR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Romania","ROU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Russian Federation","RUS",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Rwanda","RWA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Saint Kitts and Nevis","KNA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Saint Lucia","LCA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Saint Vincent and The Grenadines","VCT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Samoa","WSM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="San Marino","SMR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Sao Tome and Principe","STP",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Saudi Arabia","SAU",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Senegal","SEN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Serbia","SRB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Seychelles","SYC",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Sierra Leone","SLE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Singapore","SGP",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Slovak Republic","SVK",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Slovenia","SVN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Solomon Islands","SLB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Somalia","SOM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="South Africa","ZAF",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="South Sudan","SSD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Spain","ESP",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Sri Lanka","LKA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Sudan","SDN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Suriname","SUR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Swaziland","SWZ",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Sweden","SWE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Switzerland","CHE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Syrian Arab Republic","SYR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Tajikistan","TJK",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Thailand","THA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="The Former Yugoslav Republic of Macedonia","MKD",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Timor-Leste","TLS",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Togo","TGO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Tonga","TON",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Trinidad and Tobago","TTO",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Tunisia","TUN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Turkey","TUR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Turkmenistan","TKM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Tuvalu","TUV",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Uganda","UGA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Ukraine","UKR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Union of Soviet Socialist Republics","SUN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="United Arab Emirates","ARE",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="United Kingdom of Great Britain and Northern Ireland","GBR",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="United Republic of Tanzania","TZA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="United States of America","USA",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Uruguay","URY",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Uzbekistan","UZB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Vanuatu","VUT",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Venezuela","VEN",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Viet Nam","VNM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Yemen","YEM",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Yugoslavia","YUG",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Zambia","ZMB",precops.data.translated$Delegation_ISO)
precops.data.translated$Delegation_ISO<-ifelse(precops.data.translated$Delegation=="Zimbabwe","ZWE",precops.data.translated$Delegation_ISO)

#######################################################################
#######################Final Checks and Fixes##########################
#######################################################################

#Female
table(cops.data.translated$Honorific[is.na(cops.data.translated$Female)])
table(cops.data$Honorific[is.na(cops.data$Female)])
table(precops.data.translated$Honorific[is.na(precops.data.translated$Female)])
table(precops.data$Honorific[is.na(precops.data$Female)])

#fix some persistant honrific issues
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="HEDr","HE Dr",cops.data.translated$Honorific)
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="Prof A/ Dr","Prof Dr",cops.data.translated$Honorific)
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="Professor","Prof",cops.data.translated$Honorific)
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="Drs","Dr",cops.data.translated$Honorific)
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="Doctor","Dr",cops.data.translated$Honorific)
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="HRH HRH","HRH",cops.data.translated$Honorific)
cops.data.translated$Honorific<-ifelse(cops.data.translated$Honorific=="Monsignor","Msgr",cops.data.translated$Honorific)
cops.data.translated$Female[cops.data.translated$Honorific=="HH Mr"]<-0
cops.data.translated$Female[cops.data.translated$Honorific=="Fr"]<-1
cops.data.translated$Female[cops.data.translated$Honorific=="Msgr"]<-0

cops.data$Honorific<-ifelse(cops.data$Honorific=="HEDr","HE Dr",cops.data$Honorific)
cops.data$Honorific<-ifelse(cops.data$Honorific=="Prof A/ Dr","Prof Dr",cops.data$Honorific)
cops.data$Honorific<-ifelse(cops.data$Honorific=="Professor","Prof",cops.data$Honorific)
cops.data$Honorific<-ifelse(cops.data$Honorific=="Drs","Dr",cops.data$Honorific)
cops.data$Honorific<-ifelse(cops.data$Honorific=="Doctor","Dr",cops.data$Honorific)
cops.data$Honorific<-ifelse(cops.data$Honorific=="HRH HRH","HRH",cops.data$Honorific)
cops.data$Honorific<-ifelse(cops.data$Honorific=="Monsignor","Msgr",cops.data$Honorific)
cops.data$Female[cops.data$Honorific=="HH Mr"]<-0
cops.data$Female[cops.data$Honorific=="Fr"]<-1
cops.data$Female[cops.data$Honorific=="Msgr"]<-0


#######################################################################
#######################Save Final Datasets#############################
#######################################################################

#set working directory
setwd("C:/Users/bagoz/Desktop/COPsFormat/")

#Save final untranslated data
write.csv(cops.data,"cops.cleaned.csv",row.names=FALSE,fileEncoding = "UTF-8") 
write.csv(precops.data,"precops.cleaned.csv",row.names=FALSE,fileEncoding = "UTF-8") 

#Save final translated data
write.csv(cops.data.translated,"cops.cleaned.translated.csv",row.names=FALSE,fileEncoding = "UTF-8")
write.csv(precops.data.translated,"precops.cleaned.translated.csv",row.names=FALSE,fileEncoding = "UTF-8") 

