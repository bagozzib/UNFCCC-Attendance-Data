###Clean Pre-COP files and create variables

##############
####Set Up####
##############

#clear memory
rm( list=ls() )

#Load some packages
library(tm)
library(tidyverse)
library(readxl)
library(stringr)
library(rvest)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(XML)
library(xml2)
library(doBy)
library(reshape2)

#set seed
set.seed(50)

#set working directory
setwd("C:/Users/bagozzib/Desktop/Completed Excel Files/New files/PreCOPs")


#######################################
#########Set-up Read-in Space##########
#######################################

#get all files
files<-list.files()

#create storage set
precop.full<-NULL

#loop through all files
for(i in 1:length(files)){

#assign current file
currentfile<-files[i]
print(currentfile)

#read in data 
precop.example <- read_excel(currentfile)

#get properties
properties<-unlist(strsplit(currentfile,"_",fixed=T))

######################################################
#########step 1: standardize retained columns#########
######################################################

#omit English-translated columns and other unnecessary columns 
#These vary from pre-COP to pre-COP, and code below is meant to be robust to this varition

#remove spaces from column names, which R does not always like
colnames(precop.example)<-gsub(" ","",colnames(precop.example))

#remove irrelevant columns, if they exist
if(ncol(precop.example[ , -which(names(precop.example) %in% c(colnames(precop.example)[grep("English",colnames(precop.example))],"Extras","Namedoubt"))])>0){
precop.example<-precop.example[ , -which(names(precop.example) %in% c(colnames(precop.example)[grep("English",colnames(precop.example))],"Extras","Namedoubt"))]
}
precop.example<-as.data.frame(precop.example)

#add virtual indicator
precop.example$Virtual<-0

#add COP and year variables
precop.example$Year<-properties[1]
precop.example$Meeting<-paste(properties[2],properties[3],sep=" ")

#location varaible
location<-paste(properties[4:length(properties)],collapse=" ")
location<-gsub("(1)","",location)
location<-gsub("\\.xlsx","",location)
location<-gsub("_new","",location)
location<-gsub("Part 1","",location)
location<-gsub("Part 2","",location)
precop.example$Location<-location


#########################################################
#########step 2: fix remaining name-title issues#########
#########################################################

#If name has "H.E." or "HE." or "Dr." or "DR." at the start, move this to instead appear at the start of the person's Title-entry
precop.example$Title<-ifelse(grepl("^+HE\\.",precop.example$Name),paste(precop.example$Title,"HE.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+HE\\.",precop.example$Name),gsub("^+HE\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+HE ",precop.example$Name),paste(precop.example$Title,"HE.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+HE ",precop.example$Name),gsub("^+HE ","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+H\\.E\\.",precop.example$Name),paste(precop.example$Title,"H.E.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+H\\.E\\.",precop.example$Name),gsub("^+H\\.E\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+HL\\.E\\.",precop.example$Name),paste(precop.example$Title,"H.E.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+HL\\.E\\.",precop.example$Name),gsub("^+HL\\.E\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+HLE\\.",precop.example$Name),paste(precop.example$Title,"H.E.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+HLE\\.",precop.example$Name),gsub("^+HLE\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+H\\.L\\.E\\.",precop.example$Name),paste(precop.example$Title,"H.E.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+H\\.L\\.E\\.",precop.example$Name),gsub("^+H\\.L\\.E\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+DR\\.",precop.example$Name),paste(precop.example$Title,"DR.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+DR\\.",precop.example$Name),gsub("^+DR\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(grepl("^+Dr\\.",precop.example$Name),paste(precop.example$Title,"Dr.",sep=" "),precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Dr\\.",precop.example$Name),gsub("^+Dr\\.","",precop.example$Name),precop.example$Name)

#If Title is blank and name starts with "Mr." or "Mrs.", etc. shift that to title.
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Mr\\.",precop.example$Name), "Mr.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Mr\\.",precop.example$Name),gsub("^+Mr\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Mrs\\.",precop.example$Name), "Mrs.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Mrs\\.",precop.example$Name),gsub("^+Mrs\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Sr\\.",precop.example$Name), "Sr.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Sr\\.",precop.example$Name),gsub("^+Sr\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Sra\\.",precop.example$Name), "Sra.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Sra\\.",precop.example$Name),gsub("^+Sra\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Mme\\.",precop.example$Name), "Mme.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Mme\\.",precop.example$Name),gsub("^+Mme\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+M\\.",precop.example$Name), "M.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+M\\.",precop.example$Name),gsub("^+M\\.","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Miss.",precop.example$Name), "Ms.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Miss\\.",precop.example$Name),gsub("^+Miss\\.","",precop.example$Name),precop.example$Name)

#If Title is blank and name starts with "Mr," or "Mrs,", etc. shift that to title.
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Mr\\,",precop.example$Name), "Mr.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Mr\\,",precop.example$Name),gsub("^+Mr\\,","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Mrs\\,",precop.example$Name), "Mrs.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Mrs\\,",precop.example$Name),gsub("^+Mrs\\,","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Sr\\,",precop.example$Name), "Sr.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Sr\\,",precop.example$Name),gsub("^+Sr\\,","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Sra\\,",precop.example$Name), "Sra.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Sra\\,",precop.example$Name),gsub("^+Sra\\,","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Mme\\,",precop.example$Name), "Mme.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Mme\\,",precop.example$Name),gsub("^+Mme\\,","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+M\\,",precop.example$Name), "M.",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+M\\,",precop.example$Name),gsub("^+M\\,","",precop.example$Name),precop.example$Name)
precop.example$Title<-ifelse(precop.example$Title=="" & grepl("^+Miss",precop.example$Name), "Ms",precop.example$Title)
precop.example$Name<-ifelse(grepl("^+Miss",precop.example$Name),gsub("^+Miss","",precop.example$Name),precop.example$Name)


#fix some remaining common OCR mistakes in Name column
precop.example$Name<-gsub(" \\!"," I",precop.example$Name)
precop.example$Name<-gsub(" \\]"," I",precop.example$Name)
precop.example$Name<-gsub(" \\["," I",precop.example$Name)
precop.example$Name<-gsub("^+\\!","I",precop.example$Name)
precop.example$Name<-gsub("^+\\]","I",precop.example$Name)
precop.example$Name<-gsub("^+\\[","I",precop.example$Name)
precop.example$Name<-gsub("\\!","l",precop.example$Name)
precop.example$Name<-gsub("\\]","l",precop.example$Name)
precop.example$Name<-gsub("\\[","l",precop.example$Name)
precop.example$Name<-gsub("- ","-",precop.example$Name)
precop.example$Name<-gsub(" -","-",precop.example$Name)
precop.example$Name<-gsub(" \\.",".",precop.example$Name)
precop.example$Name<-gsub(" 0"," O",precop.example$Name)
precop.example$Name<-gsub("^+0","O",precop.example$Name)
precop.example$Name<-gsub("0","o",precop.example$Name)
precop.example$Name<-gsub("\\$","S",precop.example$Name)
precop.example$Name<-gsub("^+\\.","",precop.example$Name)
precop.example$Name<-gsub("^+\\,","",precop.example$Name)

#trim remaining leading and trailing whitepsace entries from Name and Title columns
precop.example$Title<-trimws(precop.example$Title, which = "both")
precop.example$Name<-trimws(precop.example$Name, which = "both")

#Standardizing first name last name order. This is tricky!!
#some entries are "First LAST", others are "LAST, First". 
#But, some entries have a "First Middle-Initial Last" format where the middle initial has an incorrect "," instead of an ".". For example "John M, Smith"
#First I fix the latter problem below, then reverse remaining names.

#if name contains a single letter preceded by a space followed by a comma, as in " A,", replace the comma with a period
precop.example$Name<-ifelse(grepl(" [A-Z],",precop.example$Name),gsub(",",".",precop.example$Name),precop.example$Name)

#now actually standardize first and last name based on remaining commas, by swapping last-first in those cases
for(i in 1:nrow(precop.example)){
	if(grepl(",",precop.example$Name[i])){
	temp<-strsplit(precop.example$Name[i], ", ")
	precop.example$Name[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#fix any remaining entries that end with a period
precop.example$Name<-gsub("\\.+$","",precop.example$Name)

#remove some lingering titles that may be in Name
precop.example$Name<-gsub("^+Prof\\. ","",precop.example$Name)
precop.example$Name<-gsub("^+Dr\\. ","",precop.example$Name)
precop.example$Name<-gsub("^+The Honourable ","",precop.example$Name)  
precop.example$Name<-gsub("^+The Honorable ","",precop.example$Name)

#fix one remaining issue
precop.example$Name<-gsub("^+\\‘", " and ",precop.example$Name)
precop.example$Name<-gsub("^+\\’", " and ",precop.example$Name)
precop.example$Name<-gsub("\\‘+$", " and ",precop.example$Name)
precop.example$Name<-gsub("\\’+$", " and ",precop.example$Name)

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
precop.example$Name<-trimws(precop.example$Name, which = "both")

#standardize name data to use capitals only (and always) at the start of each unigram
precop.example$Name<-str_to_title(precop.example$Name) 

#fix a common OCR issue in titles
precop.example$Title<-gsub("Mlle","Mme",precop.example$Title)
precop.example$Title<-gsub("St\\.","Sr.",precop.example$Title)
precop.example$Title<-gsub("St","Sr",precop.example$Title)
precop.example$Title<-gsub("Sta\\.","Sra.",precop.example$Title)
precop.example$Title<-gsub("Sta","Sra",precop.example$Title)
precop.example$Title<-gsub("H\\.E\\.Ms\\.","H.E. Ms.",precop.example$Title)
precop.example$Title<-gsub("H\\.E\\.Ms","H.E. Ms",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.Dr\\.","S.E. Dr.",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.Dr","S.E. Dr",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.M\\.","S.E. M.",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.M","S.E. M",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.Mme\\.","S.E. Mme.",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.Mme","S.E. Mme",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.Sr\\.","S.E. Sr.",precop.example$Title)
precop.example$Title<-gsub("S\\.E\\.Sr","S.E. Sr",precop.example$Title)
precop.example$Title<-gsub("Mine","Mme",precop.example$Title)
precop.example$Title<-gsub("H\\. E\\. Mr\\.","H.E. Mr.",precop.example$Title)
precop.example$Title<-gsub("H\\.E\\.  Mr\\.","H.E. Mr.",precop.example$Title)
precop.example$Title<-gsub("M\\. H\\.E\\.","H.E. M.",precop.example$Title)
precop.example$Title<-gsub("M\\. S\\.E\\.","S.E. M.",precop.example$Title)
precop.example$Title<-gsub("Ms\\. H\\.E\\.","H.E. Ms.",precop.example$Title)
precop.example$Title<-gsub("Mrs\\. H\\.E\\.","H.E. Mrs.",precop.example$Title)
precop.example$Title<-gsub("Mr H\\.E\\.","H.E. Mr.",precop.example$Title)
precop.example$Title<-gsub("Sr\\. S\\.E\\.","S.E. Sr.",precop.example$Title)
precop.example$Title<-gsub("Sra\\. S\\.E\\.","S.E. Sra.",precop.example$Title)
precop.example$Title<-gsub("S E\\. Sr\\.","S.E. Sr.",precop.example$Title)
precop.example$Title<-gsub("S E\\. M\\.","S.E. M.",precop.example$Title)
precop.example$Title<-gsub("M\\. HE\\.","H.E. M.",precop.example$Title)


#create female varaible 
#NOTE: there may be other variants in other pre-cop/cop years that we need to check...perhaps recover any remaining titles that aren't already listed below across all pre-cop/cops and send them to Daria and me to review.
precop.example$female<-NA
precop.example$female[precop.example$Title=="Sir"]<-0
precop.example$female[precop.example$Title=="H.E. Mr."]<-0
precop.example$female[precop.example$Title=="H.E.Mr."]<-0
precop.example$female[precop.example$Title=="H.E. Mr"]<-0
precop.example$female[precop.example$Title=="H.E. M."]<-0
precop.example$female[precop.example$Title=="H.E. Sr."]<-0
precop.example$female[precop.example$Title=="H.E. Ms."]<-1
precop.example$female[precop.example$Title=="H.E. Mrs."]<-1
precop.example$female[precop.example$Title=="HE. Mr."]<-0
precop.example$female[precop.example$Title=="H.E. M."]<-0
precop.example$female[precop.example$Title=="H.E. Ms"]<-1
precop.example$female[precop.example$Title=="H.E Ms."]<-1
precop.example$female[precop.example$Title=="H.E Ms"]<-1
precop.example$female[precop.example$Title=="H.E.Ms."]<-1
precop.example$female[precop.example$Title=="H.H. Mr."]<-1
precop.example$female[precop.example$Title=="H.E. Msgr."]<-0
precop.example$female[precop.example$Title=="H.E. Msgr"]<-0
precop.example$female[precop.example$Title=="Msgr."]<-0
precop.example$female[precop.example$Title=="H.E. Archbishop"]<-0
precop.example$female[precop.example$Title=="His"]<-0
precop.example$female[precop.example$Title=="Rev."]<-0
precop.example$female[precop.example$Title=="Rev"]<-0
precop.example$female[precop.example$Title=="M."]<-0
precop.example$female[precop.example$Title=="M"]<-0
precop.example$female[precop.example$Title=="H.E. Dato’ M."]<-0
precop.example$female[precop.example$Title=="Mme."]<-1
precop.example$female[precop.example$Title=="Mme"]<-1
precop.example$female[precop.example$Title=="Mr."]<-0
precop.example$female[precop.example$Title=="Mr"]<-0
precop.example$female[precop.example$Title=="M. R."]<-0
precop.example$female[precop.example$Title=="Miss"]<-1
precop.example$female[precop.example$Title=="Miss."]<-1
precop.example$female[precop.example$Title=="Ms."]<-1
precop.example$female[precop.example$Title=="Ms"]<-1
precop.example$female[precop.example$Title=="Mrs."]<-1
precop.example$female[precop.example$Title=="Mrs"]<-1
precop.example$female[precop.example$Title=="S.E. M."]<-0
precop.example$female[precop.example$Title=="S.E. M"]<-0
precop.example$female[precop.example$Title=="S.E. Ms"]<-0
precop.example$female[precop.example$Title=="S.E.M."]<-0
precop.example$female[precop.example$Title=="S.E. Mme."]<-1
precop.example$female[precop.example$Title=="S.E. Mme"]<-1
precop.example$female[precop.example$Title=="S.E  Mme"]<-1
precop.example$female[precop.example$Title=="S.E. Sr."]<-0
precop.example$female[precop.example$Title=="S.E. Sr"]<-0
precop.example$female[precop.example$Title=="S.E. Sra."]<-1
precop.example$female[precop.example$Title=="S.E. Sra"]<-1
precop.example$female[precop.example$Title=="S.E.Sra."]<-1
precop.example$female[precop.example$Title=="S.E.Sr."]<-0
precop.example$female[precop.example$Title=="S.E.Mme"]<-1
precop.example$female[precop.example$Title=="Sr."]<-0
precop.example$female[precop.example$Title=="Sr"]<-0
precop.example$female[precop.example$Title=="Sr. D."]<-0
precop.example$female[precop.example$Title=="Sra."]<-1
precop.example$female[precop.example$Title=="Sra"]<-1
precop.example$female[precop.example$Title=="Srta"]<-1
precop.example$female[precop.example$Title=="Srta."]<-1
precop.example$female[precop.example$Title=="Mr. H E."]<-0   
precop.example$female[precop.example$Title=="Mr. H.E."]<-0   
precop.example$female[precop.example$Title=="Mr. HE."]<-0   
precop.example$female[precop.example$Title=="Mr. HLE."]<-0       
precop.example$female[precop.example$Title=="Ms. HE."]<-0
precop.example$female[precop.example$Title=="S.E.Ms"]<-1
precop.example$female[precop.example$Title=="Fr."]<-1
precop.example$female[precop.example$Title=="H.E. Mr. Dr."]<-0
precop.example$female[precop.example$Title=="H.E. Mr. HE."]<-0
precop.example$female[precop.example$Title=="H.E. Ms. Dr."]<-1
precop.example$female[precop.example$Title=="M. DR."]<-0       
precop.example$female[precop.example$Title=="Mr. Dr."]<-0 
precop.example$female[precop.example$Title=="Ms. Dr."]<-1   
precop.example$female[precop.example$Title=="Ms. DR."]<-1
precop.example$female[precop.example$Title=="Baron"]<-0
precop.example$female[precop.example$Title=="Dame"]<-1
precop.example$female[precop.example$Title=="Lady"]<- 1 
precop.example$female[precop.example$Title=="Lord"]<-0 
precop.example$female[precop.example$Title=="Sr. Dr."]<-0
precop.example$female[precop.example$Title=="Rev. Dr."]<-0
precop.example$female[precop.example$Title=="H.E. Sir" ]<-0
precop.example$female[precop.example$Title=="S.E. Mr." ]<-0
 

#Standardize title a bit more
precop.example$Title<-gsub("\\.","",precop.example$Title)
precop.example$Title<-gsub("\\,","",precop.example$Title)
precop.example$Title<-gsub("SEM","SE M",precop.example$Title)
precop.example$Title<-gsub("HEMr","HE Mr",precop.example$Title)
precop.example$Title<-gsub("HEMrs","HE Mrs",precop.example$Title)
precop.example$Title<-gsub("MrHE","HE Mr",precop.example$Title)
precop.example$Title<-gsub("MrsHE","HE Mrs",precop.example$Title)
precop.example$Title<-gsub("H E","HE",precop.example$Title)

#Always place HE at the start of a title
for(i in 1:nrow(precop.example)){
	if(grepl(" HE+$",precop.example$Title[i])){
	temp<-strsplit(precop.example$Title[i], " ")
	precop.example$Title[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#Always place SE at the start of a title
for(i in 1:nrow(precop.example)){
	if(grepl(" SE+$",precop.example$Title[i])){
	temp<-strsplit(precop.example$Title[i], " ")
	precop.example$Title[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
precop.example$Title<-trimws(precop.example$Title, which = "both")
precop.example$Name<-trimws(precop.example$Name, which = "both")
precop.example$Title<-str_squish(precop.example$Title)
precop.example$Name<-str_squish(precop.example$Name)


###########################################
#########step 3: fix group entries#########
###########################################

#standardize entries (there may be other variants in other COPs/Pre-COPs that we need to add here
precop.example$Group<-ifelse(precop.example$Group=="InterGovernmental Organizations","Intergovernmental organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="OTHER INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="ORGANISATIONS INTERGOUVERNEMENTALES","Intergovernmental organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="NON-GOVERNMENTAL ORGANIZATIONS","Non-governmental organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="ORGANISATIONS NON-GOUVERNEMENTALES","Non-governmental organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Non-Governmental Organizations","Non-governmental organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="LIBERATION MOVEMENT","Non-governmental organizations",precop.example$Group)  
precop.example$Group<-ifelse(precop.example$Group=="OBSERVER STATES","Observer States",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Observer states","Observer States",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters","Observer States",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters.","Observer States",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters\\.","Observer States",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="PARTIES","Parties",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTATIVES","Parties",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Representatives","Parties",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTANTS","Parties",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="INSTITUTIONS SPECIALISEES ET AUTRES ORGANISATIONS","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Specialized Agencies and Related Organizations","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Specialized agencies","Specialized agencies and related organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="SPECIALIZED AGENCIES","Specialized agencies and related organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS","Specialized agencies and related organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS IN THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="OTHER ORGANIZATIONS","Specialized agencies and related organizations",precop.example$Group)  
precop.example$Group<-ifelse(precop.example$Group=="OTHER ORGANIZATIONS INVITED TO PARTICIPATE PURSUANT TO GENERAL ASSEMBLY RESOLUTION 3237 (XXIX)","Specialized agencies and related organizations",precop.example$Group)  
precop.example$Group<-ifelse(precop.example$Group=="SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM ","Specialized agencies and related organizations",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="REPRESENTATIVES OF UNITED NATIONS SECRETARIAT UNITS AND BODIES","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="NATIONS UNIES","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Representatives of United Nations Secretariat units and bodies","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="UN Secretariat units and Bodies","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="Representatives of United Nations secretariat units and bodies","United Nations Secretariat units and bodies",precop.example$Group)
precop.example$Group<-ifelse(precop.example$Group=="United Nations secretariat units and bodies","United Nations Secretariat units and bodies",precop.example$Group)                  
precop.example$Group<-ifelse(precop.example$Group=="United Nations secretariat units and related bodies","United Nations Secretariat units and bodies" ,precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="Interested United Nations organs","United Nations Secretariat units and bodies" ,precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="United Nations specialized agencies and related organizations","United Nations Secretariat units and bodies",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="Interested United Nations organs","United Nations Secretariat units and bodies",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="UNITED NATIONS","United Nations Secretariat units and bodies",precop.example$Group) 
precop.example$Group<-ifelse(precop.example$Group=="SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","United Nations Secretariat units and bodies",precop.example$Group) 


#create binary vars
precop.example$IGO<-0
precop.example$IGO[precop.example$Group=="Intergovernmental organizations"]<-1
precop.example$NGO<-0
precop.example$NGO[precop.example$Group=="Non-governmental organizations"]<-1
precop.example$Observer<-0
precop.example$Observer[precop.example$Group=="Observer States"]<-1
precop.example$Party<-0
precop.example$Party[precop.example$Group=="Parties"]<-1
precop.example$IO<-0
precop.example$IO[precop.example$Group=="Specialized agencies and related organizations"]<-1
precop.example$IO[precop.example$Group=="United Nations Secretariat units and bodies"]<-1


###########################################
#########step 4: fix entity entries########
###########################################

#standardize capitalization format
precop.example$Entity<-str_to_title(precop.example$Entity) 

#not all years have countries spelled in a foreign language (usually French or Spanish) but some do. 
#other cases have misspellings or OCR problems
#Let's fix all of these manually here:
precop.example$Entity[precop.example$Entity=="Albanie"]<-"Albania"                                         
precop.example$Entity[precop.example$Entity=="Algerie"]<-"Algeria"                                             
precop.example$Entity[precop.example$Entity=="Allemagne"]<-"Germany" 
precop.example$Entity[precop.example$Entity=="Allemagne (Suite)"]<-"Germany" 
precop.example$Entity[precop.example$Entity=="Antigua-Et-Barbuda"]<-"Antigua And Barbuda"     
precop.example$Entity[precop.example$Entity=="Antigua Et Barbuda"]<-"Antigua And Barbuda" 
precop.example$Entity[precop.example$Entity=="Arabie Saoudite"]<-"Saudi Arabia"   
precop.example$Entity[precop.example$Entity=="Arabie Saoudite (Suite)"]<-"Saudi Arabia" 
precop.example$Entity[precop.example$Entity=="Argentine" ]<-"Argentina"                                          
precop.example$Entity[precop.example$Entity=="Armenie"]<-"Armenia"                                            
precop.example$Entity[precop.example$Entity=="Australie" ]<-"Australia"
precop.example$Entity[precop.example$Entity=="Australie (Suite)" ]<-"Australia"         
precop.example$Entity[precop.example$Entity=="Autriche"   ]<-"Austria"
precop.example$Entity[precop.example$Entity=="Autriche (Suite)"    ]<-"Austria"
precop.example$Entity[precop.example$Entity=="Azerbaidjan"    ]<-"Azerbaijan"
precop.example$Entity[precop.example$Entity=="Bahrein"  ]<-"Bahrain"                                         
precop.example$Entity[precop.example$Entity=="Bangladesh" ]<-"Bangladesh"                                       
precop.example$Entity[precop.example$Entity=="Barbade"  ]<-"Barbados"                                         
precop.example$Entity[precop.example$Entity=="Belgique"  ]<-"Belgium"                                          
precop.example$Entity[precop.example$Entity=="Belize" ]<-"Belize"                                            
precop.example$Entity[precop.example$Entity=="Benin" ]<-"Benin"                                             
precop.example$Entity[precop.example$Entity=="Bhoutan"   ]<-"Bhutan"  
precop.example$Entity[precop.example$Entity=="Bolivia (Plurinational State Of)"  ]<-"Bolivia"
precop.example$Entity[precop.example$Entity=="Bolivie"  ]<-"Bolivia"                                          
precop.example$Entity[precop.example$Entity=="Botswana"]<-"Bostwana"                                           
precop.example$Entity[precop.example$Entity=="Bresil" ]<-"Brazil"                                           
precop.example$Entity[precop.example$Entity=="Bulgarie"  ]<-"Bulgaria"                                         
precop.example$Entity[precop.example$Entity=="Burkina Faso"   ]<-"Burkina Faso"                                    
precop.example$Entity[precop.example$Entity=="Cambodge"  ]<-"Cambodia"                                         
precop.example$Entity[precop.example$Entity=="Cameroun"  ]<-"Cameroon"                                         
precop.example$Entity[precop.example$Entity=="Canada"   ]<-"Canada"  
precop.example$Entity[precop.example$Entity=="Canada (Suite)"   ]<-"Canada"                                    
precop.example$Entity[precop.example$Entity=="Cap-Vert"   ]<-"Cape Verde"                                          
precop.example$Entity[precop.example$Entity=="Chili"]<-"Chile"                                              
precop.example$Entity[precop.example$Entity=="Chine" ]<-"China" 
precop.example$Entity[precop.example$Entity=="Republique Populaire De Chine"]<-"China" 
precop.example$Entity[precop.example$Entity=="Chine (Suite)"    ]<-"China"   
precop.example$Entity[precop.example$Entity=="Colombie"  ]<-"Colombia"
precop.example$Entity[precop.example$Entity=="Columbia"]<-"Colombia"
precop.example$Entity[precop.example$Entity=="Communaute Europeenne" ]<-"European Community"                               
precop.example$Entity[precop.example$Entity=="Comores"  ]<-"Comoros"                                          
precop.example$Entity[precop.example$Entity=="Costa Rica"  ]<-"Costa Rica"  
precop.example$Entity[precop.example$Entity=="Costarica"  ]<-"Costa Rica"                
precop.example$Entity[precop.example$Entity=="Cote D' Ivoire" ]<-"Cote D'ivoire"
precop.example$Entity[precop.example$Entity=="Cote D’ Ivoire" ]<-"Cote D'ivoire"
precop.example$Entity[precop.example$Entity=="Cote D’Ivoire" ]<-"Cote D'ivoire"
precop.example$Entity[precop.example$Entity=="Côte D’ivoire" ]<-"Cote D'ivoire" 
precop.example$Entity[precop.example$Entity=="Côte D'ivoire" ]<-"Cote D'ivoire" 
precop.example$Entity[precop.example$Entity=="Côté D'ivoire"]<-"Cote D'ivoire" 
precop.example$Entity[precop.example$Entity=="Côte D’ Ivoire"]<-"Cote D'ivoire"
precop.example$Entity[precop.example$Entity=="Cote D’ivoire"]<-"Cote D'ivoire"
precop.example$Entity[precop.example$Entity=="Croatie" ]<-"Croatia"                                           
precop.example$Entity[precop.example$Entity=="Cuba"  ]<-"Cuba"       
precop.example$Entity[precop.example$Entity=="Chypre"  ]<-"Cyprus" 
precop.example$Entity[precop.example$Entity=="Danemark" ]<-"Denmark"                                          
precop.example$Entity[precop.example$Entity=="Djibouti"  ]<-"Djibouti"                                         
precop.example$Entity[precop.example$Entity=="Egypte"  ]<-"Egypt"                                           
precop.example$Entity[precop.example$Entity=="El Salvador"    ]<-"El Salvador"                                     
precop.example$Entity[precop.example$Entity=="Emirats Arabes Unis"    ]<-"United Arab Emirates"                           
precop.example$Entity[precop.example$Entity=="Equateur"  ]<-"Ecuador"                                         
precop.example$Entity[precop.example$Entity=="Erythree"  ]<-"Eritrea"                                         
precop.example$Entity[precop.example$Entity=="Espagne"   ]<-"Spain"                                         
precop.example$Entity[precop.example$Entity=="Estonie"   ]<-"Estonia"                                        
precop.example$Entity[precop.example$Entity=="Etats-Unis D' Amerique"    ]<-"United States Of America" 
precop.example$Entity[precop.example$Entity=="Etats-Unis D'amerique"  ]<-"United States Of America"                             
precop.example$Entity[precop.example$Entity=="Etats-Unis D'amerique (Suite)"]<-"United States Of America" 
precop.example$Entity[precop.example$Entity=="Etats-Unis D’amérique"]<-"United States Of America"                                
precop.example$Entity[precop.example$Entity=="Etats-Unis D’'Amérique"]<-"United States Of America" 
precop.example$Entity[precop.example$Entity=="Etats-Unis D’amerique" ]<-"United States Of America"                                    
precop.example$Entity[precop.example$Entity=="Etats-Unis D’amerique (Suite)"]<-"United States Of America"               
precop.example$Entity[precop.example$Entity=="Ethiopie"  ]<-"Ethiopia"                                          
precop.example$Entity[precop.example$Entity=="Federation De Russie"  ]<-"Russian Federation"                             
precop.example$Entity[precop.example$Entity=="Fidj1"    ]<-"Fiji"  
precop.example$Entity[precop.example$Entity=="Fidji"    ]<-"Fiji"                    
precop.example$Entity[precop.example$Entity=="Finlande"    ]<-"Finland"                                        
precop.example$Entity[precop.example$Entity=="France"    ]<-"France"
precop.example$Entity[precop.example$Entity=="France (Suite)"    ]<-"France"        
precop.example$Entity[precop.example$Entity=="Gambie"  ]<-"Gambia"                                            
precop.example$Entity[precop.example$Entity=="Georgie"    ]<-"Georgia"                                         
precop.example$Entity[precop.example$Entity=="Ghana"  ]<-"Ghana"      
precop.example$Entity[precop.example$Entity=="Grenade"  ]<-"Grenada"                
precop.example$Entity[precop.example$Entity=="Grece"   ]<-"Greece"                                           
precop.example$Entity[precop.example$Entity=="Guatemala"  ]<-"Guatemala"
precop.example$Entity[precop.example$Entity=="Guinee- Bissau" ]<-"Guinea-Bissau"  
precop.example$Entity[precop.example$Entity=="Guinee-Bissau"  ]<-"Guinea-Bissau"           
precop.example$Entity[precop.example$Entity=="Guinea-Bissau"   ]<-"Guinea-Bissau"                                  
precop.example$Entity[precop.example$Entity=="Guinee"     ]<-"Guinea" 
precop.example$Entity[precop.example$Entity=="Guinee Equatoriale"]<-"Equatorial Guinea"
precop.example$Entity[precop.example$Entity=="Guyane"   ]<-"Guyana"                                          
precop.example$Entity[precop.example$Entity=="Honduras"   ]<-"Honduras"                                          
precop.example$Entity[precop.example$Entity=="Hongrie"   ]<-"Hungary"
precop.example$Entity[precop.example$Entity=="Iles Cook"   ]<-"Cook Islands" 
precop.example$Entity[precop.example$Entity=="Iles Marshall"   ]<-"Marshall Islands"                                   
precop.example$Entity[precop.example$Entity=="Iles Salomon"  ]<-"Solomon Islands"                                      
precop.example$Entity[precop.example$Entity=="Inde"   ]<-"India"                                           
precop.example$Entity[precop.example$Entity=="Indonesie"   ]<-"Indonesia" 
precop.example$Entity[precop.example$Entity=="Indonesie (Suite)"  ]<-"Indonesia" 
precop.example$Entity[precop.example$Entity=="Indonesie ( Suite)"]<-"Indonesia" 
precop.example$Entity[precop.example$Entity=="Irlande"  ]<-"Ireland"                                          
precop.example$Entity[precop.example$Entity=="Islande"   ]<-"Iceland"                                          
precop.example$Entity[precop.example$Entity=="Italie"     ]<-"Italy"                                         
precop.example$Entity[precop.example$Entity=="Jamaique"  ]<-"Jamaica"                                          
precop.example$Entity[precop.example$Entity=="Japon"  ]<-"Japan"    
precop.example$Entity[precop.example$Entity=="Japon (Suite)"]<-"Japan"
precop.example$Entity[precop.example$Entity=="Japan (Suite)"]<-"Japan"
precop.example$Entity[precop.example$Entity=="Jordanie"  ]<-"Jordan"
precop.example$Entity[precop.example$Entity=="Joordan"  ]<-"Jordan"
precop.example$Entity[precop.example$Entity=="Kenya"  ]<-"Kenya"                                            
precop.example$Entity[precop.example$Entity=="Kiribati" ]<-"Kiribati"                                          
precop.example$Entity[precop.example$Entity=="Koweit"   ]<-"Kuwait"                                         
precop.example$Entity[precop.example$Entity=="Lesotho"   ]<-"Lesotho"                                           
precop.example$Entity[precop.example$Entity=="Lettonie"    ]<-"Latvia"                                       
precop.example$Entity[precop.example$Entity=="Liban"     ]<-"Lebanon"                                         
precop.example$Entity[precop.example$Entity=="Liechtenstein"  ]<-"Liechtenstein"                                  
precop.example$Entity[precop.example$Entity=="Lituanie"      ]<-"Lithuania"                                    
precop.example$Entity[precop.example$Entity=="Luxembourg"  ]<-"Luxembourg"                                        
precop.example$Entity[precop.example$Entity=="Malaisie"   ]<-"Malaysia"                                        
precop.example$Entity[precop.example$Entity=="Malawi"   ]<-"Malawi"                                         
precop.example$Entity[precop.example$Entity=="Maldives" ]<-"Maldives"                                           
precop.example$Entity[precop.example$Entity=="Mali"     ]<-"Mali"                                          
precop.example$Entity[precop.example$Entity=="Malte"    ]<-"Malta"                                          
precop.example$Entity[precop.example$Entity=="Maroc"   ]<-"Morocco"                                          
precop.example$Entity[precop.example$Entity=="Maurice" ]<-"Mauritius"                                           
precop.example$Entity[precop.example$Entity=="Mauritanie"  ]<-"Mauritania"                                       
precop.example$Entity[precop.example$Entity=="Mexique (Suite)"   ]<-"Mexico" 
precop.example$Entity[precop.example$Entity=="Mexique"   ]<-"Mexico"  
precop.example$Entity[precop.example$Entity=="Micronesie"    ]<-"Micronesia (Federated States Of)"                
precop.example$Entity[precop.example$Entity=="Micronesie (Etats Federes De)"    ]<-"Micronesia (Federated States Of)"                  
precop.example$Entity[precop.example$Entity=="Monaco"      ]<-"Monaco"                                         
precop.example$Entity[precop.example$Entity=="Mongolie"  ]<-"Mongolia"                                          
precop.example$Entity[precop.example$Entity=="Myanmar"  ]<-"Myanmar"                                        
precop.example$Entity[precop.example$Entity=="Namibie"    ]<-"Namibia"                                       
precop.example$Entity[precop.example$Entity=="Nepal"    ]<-"Nepal"                                           
precop.example$Entity[precop.example$Entity=="Nicaragua"    ]<-"Nicaragua"                                      
precop.example$Entity[precop.example$Entity=="Niger"      ]<-"Niger"                                        
precop.example$Entity[precop.example$Entity=="Nigeria"      ]<-"Nigeria"                                      
precop.example$Entity[precop.example$Entity=="Nioue"       ]<-"Niue"                                      
precop.example$Entity[precop.example$Entity=="Norvege"       ]<-"Norway"                                    
precop.example$Entity[precop.example$Entity=="Nouvelle-Zelande"  ]<-"New Zealand" 
precop.example$Entity[precop.example$Entity=="Nouvelle~Zelande"    ]<-"New Zealand"
precop.example$Entity[precop.example$Entity=="Nouvelle- Zelande"      ]<-"New Zealand"                    
precop.example$Entity[precop.example$Entity=="Oman"         ]<-"Oman" 
precop.example$Entity[precop.example$Entity=="Oman (Suite)"         ]<-"Oman"        
precop.example$Entity[precop.example$Entity=="Ouganda"    ]<-"Uganda"                                        
precop.example$Entity[precop.example$Entity=="Ouzbekistan"   ]<-"Uzbekistan"                                      
precop.example$Entity[precop.example$Entity=="Pakistan"    ]<-"Pakistan"                                         
precop.example$Entity[precop.example$Entity=="Panama"   ]<-"Panama"                                          
precop.example$Entity[precop.example$Entity=="Paraguay"  ]<-"Paraguay"   
precop.example$Entity[precop.example$Entity=="Pays-Bas (Suite)"]<-"Netherlands"    
precop.example$Entity[precop.example$Entity=="Pays-Bas" ]<-"Netherlands"                                          
precop.example$Entity[precop.example$Entity=="Perou" ]<-"Peru"       
precop.example$Entity[precop.example$Entity=="Perou (Suite)"]<-"Peru" 
precop.example$Entity[precop.example$Entity=="Philippines"  ]<-"Philippines"  
precop.example$Entity[precop.example$Entity=="Philippines (Suite)"    ]<-"Philippines"                    
precop.example$Entity[precop.example$Entity=="Pologne"    ]<-"Poland"                                        
precop.example$Entity[precop.example$Entity=="Portugal"    ]<-"Portugal"    
precop.example$Entity[precop.example$Entity=="Republique Socialiste Sovietique De Bielorussie"]<-"Belarus"
precop.example$Entity[precop.example$Entity=="Qatar"    ]<-"Qatar"                                          
precop.example$Entity[precop.example$Entity=="Republique Arabe Syrienne"]<-"Syrian Arab Republic"
precop.example$Entity[precop.example$Entity=="République Arabe Syrienne"]<-"Syrian Arab Republic"
precop.example$Entity[precop.example$Entity=="Republique Centrafricaine" ]<-"Central African Republic"                         
precop.example$Entity[precop.example$Entity=="Republique De Coree"   ]<-"Republic Of Korea"          
precop.example$Entity[precop.example$Entity=="Republique De Coree (Suite)"]<-"Republic Of Korea"     
precop.example$Entity[precop.example$Entity=="Republique De Moldova"   ]<-"Moldova, Republic Of"   
precop.example$Entity[precop.example$Entity=="Lao People’s Democratic Republic"   ]<-"Lao Peoples Democratic Republic" 
precop.example$Entity[precop.example$Entity=="Republique Democratique Populaire Lao"   ]<-"Lao Peoples Democratic Republic"  
precop.example$Entity[precop.example$Entity=="Lao People's Democratic Republic" ]<-"Lao Peoples Democratic Republic" 
precop.example$Entity[precop.example$Entity=="Democratic People’s Republic Of Korea" ]<-"Democratic People's Republic Of Korea"
precop.example$Entity[precop.example$Entity=="Democratic Peoples Republic Of Korea" ]<-"Democratic People's Republic Of Korea"        
precop.example$Entity[precop.example$Entity=="Republique Populaire Democratique De Coree" ]<-"Democratic People's Republic Of Korea"      
precop.example$Entity[precop.example$Entity=="Republique Tcheque"     ]<-"Czech Republic"  
precop.example$Entity[precop.example$Entity=="Czechia"     ]<-"Czech Republic"
precop.example$Entity[precop.example$Entity=="Republique-Unie De Tanzanite"]<-"United Republic Of Tanzania" 
precop.example$Entity[precop.example$Entity=="Republique-Unie De Tanzanie" ]<-"United Republic Of Tanzania"                        
precop.example$Entity[precop.example$Entity=="Roumanie"   ]<-"Romania"            
precop.example$Entity[precop.example$Entity=="Royaume-Uni De Grande-Bretagne Et D'irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
precop.example$Entity[precop.example$Entity=="Royaume-Uni De Grande Bretagne Et D Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
precop.example$Entity[precop.example$Entity=="Royaume-Uni De Grande Bretagne Et D' Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland" 
precop.example$Entity[precop.example$Entity=="Royaume- Uni De Grande- Bretagne Et D’irlande Du Nord" ]<-"United Kingdom Of Great Britain And Northern Ireland"       
precop.example$Entity[precop.example$Entity=="Royaume-Uni De Grande- Bretagne Et D’irlande Du Nord (Suite)" ]<-"United Kingdom Of Great Britain And Northern Ireland" 
precop.example$Entity[precop.example$Entity=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord"     ]<-"United Kingdom Of Great Britain And Northern Ireland"     
precop.example$Entity[precop.example$Entity=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord (Suite)"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
precop.example$Entity[precop.example$Entity=="United Kingdom"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
precop.example$Entity[precop.example$Entity=="Saint-Kitts-Et-Nevis" ]<-"Saint Kitts And Nevis"                            
precop.example$Entity[precop.example$Entity=="Sainte-Lucie" ]<-"Saint Lucia"  
precop.example$Entity[precop.example$Entity=="Sao Tome-Et-Principe"    ]<-"Sao Tome and Principe"         
precop.example$Entity[precop.example$Entity=="Samoa" ]<-"Samoa"   
precop.example$Entity[precop.example$Entity=="Papouasie-Nouvelle-Guinee"     ]<-"Papua New Guinea" 
precop.example$Entity[precop.example$Entity=="Papouasie-Nouvelle- Guinee"     ]<-"Papua New Guinea" 
precop.example$Entity[precop.example$Entity=="Senegal"    ]<-"Senegal"                                         
precop.example$Entity[precop.example$Entity=="Sierra Leone"  ]<-"Sierra Leone"                                    
precop.example$Entity[precop.example$Entity=="Slovaquie"   ]<-"Slovak Republic"  
precop.example$Entity[precop.example$Entity=="Slovakia"   ]<-"Slovak Republic"
precop.example$Entity[precop.example$Entity=="Slovenie"  ]<-"Slovenia"                                         
precop.example$Entity[precop.example$Entity=="Soudan"  ]<-"Sudan"                                           
precop.example$Entity[precop.example$Entity=="Sri Lanka"  ]<-"Sri Lanka"
precop.example$Entity[precop.example$Entity=="Srilanka"   ]<-"Sri Lanka"
precop.example$Entity[precop.example$Entity=="Suede"   ]<-"Swedan" 
precop.example$Entity[precop.example$Entity=="Suede (Suite)"   ]<-"Swedan" 
precop.example$Entity[precop.example$Entity=="Sutsse"    ]<-"Switzerland"               
precop.example$Entity[precop.example$Entity=="Suisse" ]<-"Switzerland"                                           
precop.example$Entity[precop.example$Entity=="Tchad"  ]<-"Chad"                                             
precop.example$Entity[precop.example$Entity=="Thailand" ]<-"Thailand"  
precop.example$Entity[precop.example$Entity=="Thailande (Suite)" ]<-"Thailand"   
precop.example$Entity[precop.example$Entity=="Jamahiriya Arabe Libyenne"]<-"Libyan Arab Jamahiriya"       
precop.example$Entity[precop.example$Entity=="Libya"]<-"Libyan Arab Jamahiriya"
precop.example$Entity[precop.example$Entity=="Togo"   ]<-"Togo"                                            
precop.example$Entity[precop.example$Entity=="Trinite-Et-Tobago"   ]<-"Trinidad And Tobago"
precop.example$Entity[precop.example$Entity=="Trinite-Et- Tobago"  ]<-"Trinidad And Tobago"
precop.example$Entity[precop.example$Entity=="Tunisie"   ]<-"Tunisia"   
precop.example$Entity[precop.example$Entity=="Tchecoslovaquie" ] <-"Czechoslovakia"                                
precop.example$Entity[precop.example$Entity=="Thailande"    ]<-"Thailand" 
precop.example$Entity[precop.example$Entity=="Turkmenistan" ]<-"Turkmenistan"                                     
precop.example$Entity[precop.example$Entity=="Uruguay"     ]<-"Uruguay"                                      
precop.example$Entity[precop.example$Entity=="Vanuatu"    ]<-"Vanuatu"    
precop.example$Entity[precop.example$Entity=="Venezuela (Bolivarian Republic Of)"]<-"Venezuela"
precop.example$Entity[precop.example$Entity=="Venezuela"    ]<-"Venezuela" 
precop.example$Entity[precop.example$Entity=="Venezuela (Suite)"    ]<-"Venezuela" 
precop.example$Entity[precop.example$Entity=="Vietnam" ]<-"Viet Nam" 
precop.example$Entity[precop.example$Entity=="Viet Nam." ]<-"Viet Nam"                                          
precop.example$Entity[precop.example$Entity=="Yemen"    ]<-"Yemen"                                          
precop.example$Entity[precop.example$Entity=="Zaire"   ]<-"Democratic Republic of The Congo"                
precop.example$Entity[precop.example$Entity=="Zambie"  ]<-"Zambia"                                           
precop.example$Entity[precop.example$Entity=="Zimbabwe"]<-"Zimbabwe"
precop.example$Entity[precop.example$Entity=="Afrique Du Sud"    ]<-"South Africa"                                                                                                                     
precop.example$Entity[precop.example$Entity=="Gabon"   ]<-"Gabon"                                                                                                                              
precop.example$Entity[precop.example$Entity=="Haiti"       ]<-"Haiti"                                                                                                                           
precop.example$Entity[precop.example$Entity=="Tran (Islamic Republic Of)"  ]<-"Iran (Islamic Republic Of)" 
precop.example$Entity[precop.example$Entity=="Iran (Republique Islamique De)"  ]<-"Iran (Islamic Republic Of)"   
precop.example$Entity[precop.example$Entity=="Iran (Republique Islamique D’)"    ]<-"Iran (Islamic Republic Of)"                                  
precop.example$Entity[precop.example$Entity=="Iraq"     ]<-"Iraq"                                                                                                                              
precop.example$Entity[precop.example$Entity=="Israel"     ]<-"Israel"  
precop.example$Entity[precop.example$Entity=="The Former Yugoslay Republic Of Macedonia" ]<-"The Former Yugoslav Republic Of Macedonia" 
precop.example$Entity[precop.example$Entity=="North Macedonia"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                                            
precop.example$Entity[precop.example$Entity=="L'ex-Republique Yougoslave De Macedoi!"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                               
precop.example$Entity[precop.example$Entity=="Madagascar"          ]<-"Madagascar"                                                                                                                  
precop.example$Entity[precop.example$Entity=="Republique Dominicaine"    ]<-"Domnican Republic"                                                                                                             
precop.example$Entity[precop.example$Entity=="Saint-Siege"    ]<-"Holy See" 
precop.example$Entity[precop.example$Entity=="Saint Siege"     ]<-"Holy See"                                                                                                           
precop.example$Entity[precop.example$Entity=="Singapour"   ]<-"Singapore"                                                                                                                          
precop.example$Entity[precop.example$Entity=="Swaziland"   ]<-"Swaziland"      
precop.example$Entity[precop.example$Entity=="Türkiye"   ]<-"Turkey"                                                    
precop.example$Entity[precop.example$Entity=="Turquie"     ]<-"Turkey"                                                                                                                           
precop.example$Entity[precop.example$Entity=="Ukrainian Soviet Socialist Republic"]<-"Ukraine" 
precop.example$Entity[precop.example$Entity=="Ukraine"]<-"Ukraine" 
precop.example$Entity[precop.example$Entity=="Union Des Republiques Socialistes Sovietiques" ]<-"Union of Soviet Socialist Republics"
precop.example$Entity[precop.example$Entity=="Yougoslavie" ]<-"Yugoslavia"
precop.example$Entity[precop.example$Entity=="Swedan" ]<-"Sweden"                                             
precop.example$Entity[precop.example$Entity=="Solomon Islands (Continued0" ]<-"Solomon Islands"                                            
precop.example$Entity[precop.example$Entity=="Serbia and Montenegro" ]<-"Serbia"                                            
precop.example$Entity[precop.example$Entity=="Seychelles (Continued0" ]<-"Seychelles"                                          
precop.example$Entity[precop.example$Entity=="Republic of Cabo Verde"]<-"Cape Verde"                                     
precop.example$Entity[precop.example$Entity=="Cote D Ivoire"]<-"Cote D'ivoire"                                         
precop.example$Entity[precop.example$Entity=="Côte D`Ivoire"]<-"Cote D'ivoire"                               
precop.example$Entity[precop.example$Entity=="Domnican Republic"]<-"Dominican Republic" 
precop.example$Entity[precop.example$Entity=="Eswatini"]<-"Swaziland" 
precop.example$Entity[precop.example$Entity=="Holysee" ]<-"Holy See" 
precop.example$Entity[precop.example$Entity=="Kazakstan"]<-"Kazakhstan"
precop.example$Entity[precop.example$Entity=="Moldova, Republic Of"]<- "Moldova"   
precop.example$Entity[precop.example$Entity=="Flj1" ]<-"Fiji"  
precop.example$Entity[precop.example$Entity=="Guiea"]<-"Guinea"                                           
precop.example$Entity[precop.example$Entity=="Georgla"]<-"Georgia"                                          
precop.example$Entity[precop.example$Entity=="State of Palestine"]<-"Palestine" 
precop.example$Entity[precop.example$Entity=="Nive"]<-"Niue"                                                        
precop.example$Entity[precop.example$Entity=="Cape Verde" ]<-"Cabo Verde" 


#fix a few minor over-capitalization issues
precop.example$Entity<-gsub(" Of ", " of ",precop.example$Entity)
precop.example$Entity<-gsub(" Of)", " of)",precop.example$Entity)
precop.example$Entity<-gsub(" And ", " and ",precop.example$Entity)
precop.example$Entity<-gsub(" For ", " for ",precop.example$Entity)

#fix one remaining issue
precop.example$Entity<-gsub("^+\\‘", " and ",precop.example$Entity)
precop.example$Entity<-gsub("^+\\’", " and ",precop.example$Entity)
precop.example$Entity<-gsub("\\‘+$", " and ",precop.example$Entity)
precop.example$Entity<-gsub("\\’+$", " and ",precop.example$Entity)

#fix a few more cases
precop.example$Entity[precop.example$Entity=="Iran(Islamic Republic of)" ]<-"Iran (Islamic Republic of)" 
precop.example$Entity[precop.example$Entity=="Netherlands (Kingdom of The)"]<-"Netherlands" 
precop.example$Entity[precop.example$Entity==" and Grenada"]<-"Grenada"                                         
precop.example$Entity[precop.example$Entity==" and Kenya"]<-"Kenya"   
precop.example$Group[precop.example$Entity=="Organisation Mondiale Du Commerce (Omc)"]<-"Specialized agencies and related organizations"  
precop.example$Entity[precop.example$Entity=="Republic of Moldova"]<- "Moldova" 
precop.example$Entity[precop.example$Entity=="Republic of Cabo Verde"]<- "Cabo Verde" 
precop.example$Entity[precop.example$Entity=="Serbia and Montenegro" ]<-"Serbia"  
precop.example$Entity[precop.example$Entity=="State of Palestine"]<-"Palestine" 
precop.example$Entity[precop.example$Entity=="Jamahiriy A Arabe Libyenne"]<-"Libyan Arab Jamahiriya"     
precop.example$Entity[precop.example$Entity=="République Democratique Populaire Lao" ]<-"Lao Peoples Democratic Republic"    
precop.example$Entity[precop.example$Entity=="Papouas Ie-Nouvelle-Guinee" ]<-"Papua New Guinea"    
precop.example$Entity[precop.example$Entity=="Romanta"  ]<-"Romania"     
precop.example$Entity[precop.example$Entity=="Federated States of Micronesia"]<-"Micronesia (Federated States of)"   
precop.example$Entity[precop.example$Entity=="Micronesia"  ]<-"Micronesia (Federated States of)"   
precop.example$Entity[precop.example$Entity=="Micronesia (Federated States Of)"  ]<-"Micronesia (Federated States of)"   
precop.example$Entity[precop.example$Entity=="Thallande" ]<-"Thailand"    

                                   
#Further standardization
precop.example$Entity<-gsub("\\("," (",precop.example$Entity)
precop.example$Entity<-gsub("\\)",") ",precop.example$Entity)
precop.example$Entity<-gsub("\\’","'",precop.example$Entity)
precop.example$Entity<-gsub("\\( ","(",precop.example$Entity)
precop.example$Entity<-gsub(" \\)",")",precop.example$Entity)
precop.example$Entity<-str_squish(precop.example$Entity)
precop.example$Entity<-trimws(precop.example$Entity, which = "both")
precop.example$Entity[precop.example$Entity=="Iran (Islamic Republic of)"]<-"Iran"                                                 
precop.example$Entity[precop.example$Entity=="Rhanda"]<-"Rwanda"   
precop.example$Entity[precop.example$Entity=="Dominique"]<-"Dominica" 

#########################################################
#########step 5: Job title, Division, Affiliation########
#########################################################

#fix a few minor issues
precop.example$JobTitle<-gsub("\\-+$", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\,+$", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\.+$", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("^+\\,", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("^+\\.", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("^+\\‘", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("^+\\’", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("^+\\-", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\‘+$", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\’+$", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("^+\\¡", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\¡+$", "",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\("," (",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\)",") ",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\’","'",precop.example$JobTitle)
precop.example$JobTitle<-gsub("\\( ","(",precop.example$JobTitle)
precop.example$JobTitle<-gsub(" \\)",")",precop.example$JobTitle)

precop.example$Division<-gsub("\\-+$", "",precop.example$Division)
precop.example$Division<-gsub("\\,+$", "",precop.example$Division)
precop.example$Division<-gsub("\\.+$", "",precop.example$Division)
precop.example$Division<-gsub("^+\\,", "",precop.example$Division)
precop.example$Division<-gsub("^+\\.", "",precop.example$Division)
precop.example$Division<-gsub("^+\\‘", "",precop.example$Division)
precop.example$Division<-gsub("^+\\’", "",precop.example$Division)
precop.example$Division<-gsub("^+\\-", "",precop.example$Division)
precop.example$Division<-gsub("\\‘+$", "",precop.example$Division)
precop.example$Division<-gsub("\\’+$", "",precop.example$Division)
precop.example$Division<-gsub("^+\\¡", "",precop.example$Division)
precop.example$Division<-gsub("\\¡+$", "",precop.example$Division)
precop.example$Division<-gsub("\\("," (",precop.example$Division)
precop.example$Division<-gsub("\\)",") ",precop.example$Division)
precop.example$Division<-gsub("\\’","'",precop.example$Division)
precop.example$Division<-gsub("\\( ","(",precop.example$Division)
precop.example$Division<-gsub(" \\)",")",precop.example$Division)

precop.example$Affiliation<-gsub("\\-+$", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\,+$", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\.+$", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("^+\\,", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("^+\\.", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("^+\\‘", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("^+\\’", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("^+\\-", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\‘+$", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\’+$", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("^+\\¡", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\¡+$", "",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\("," (",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\)",") ",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\’","'",precop.example$Affiliation)
precop.example$Affiliation<-gsub("\\( ","(",precop.example$Affiliation)
precop.example$Affiliation<-gsub(" \\)",")",precop.example$Affiliation)

#fix one other potential issue (if Division is blank and Affiliation just says "Climate Change", always shift this to Division)
for(i in 1:nrow(precop.example)){
	if((is.na(precop.example$Division[i]) & !is.na(precop.example$Affiliation[i]))){
		if(precop.example$Affiliation[i]=="Climate Change"){
		precop.example$Division[i]<-"Climate Change"
		precop.example$Affiliation[i]<-NA
		} 
	}
}

#trim remaining leading and trailing whitepsace
precop.example$JobTitle<-str_squish(precop.example$JobTitle)
precop.example$Division<-str_squish(precop.example$Division)
precop.example$Affiliation<-str_squish(precop.example$Affiliation)
precop.example$JobTitle<-trimws(precop.example$JobTitle, which = "both")
precop.example$Division<-trimws(precop.example$Division, which = "both")
precop.example$Affiliation<-trimws(precop.example$Affiliation, which = "both")

#now combine
precop.full<-rbind(precop.full,precop.example)

}#close full loop

#add column names
colnames(precop.full)<-c("Group_Type","Delegation","Honorific","Person_Name","Job_Title","Division","Affiliation","Virtual","Year","Meeting","Location",    "Female","IGO","NGO","Observer","Party","IO" )

#fix location mentions
precop.full$Location<-gsub("\\(","",precop.full$Location)
precop.full$Location<-gsub("\\)","",precop.full$Location)
precop.full$Location<-gsub("Part","",precop.full$Location)
precop.full$Location<-str_squish(precop.full$Location)
precop.full$Location<-trimws(precop.full$Location, which = "both")

#set working directory
setwd("C:/Users/bagozzib/Desktop/Completed Excel Files/")

#now save file
write.csv(precop.full,"precops.cleaned.csv",row.names=FALSE)



  