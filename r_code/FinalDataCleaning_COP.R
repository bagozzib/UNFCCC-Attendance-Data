###Clean COP files and create variables

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
setwd("")


#######################################
#########Set-up Read-in Space##########
#######################################

#get all files
files<-list.files()

#create storage set
cop.full<-NULL

#loop through all files
for(i in 1:length(files)){

#assign current file
currentfile<-files[i]
print(currentfile)

#read in data 
cop.example <- read_excel(currentfile)

#get properties
properties<-unlist(strsplit(currentfile,"_",fixed=T))

######################################################
#########step 1: standardize retained columns#########
######################################################

if((as.numeric(properties[3])!=28) & (as.numeric(properties[3])!=29)){

#omit English-translated columns and other unnecessary columns 
#These vary from COP to COP, and code below is meant to be robust to this varition

#remove spaces from column names, which R does not always like
colnames(cop.example)<-gsub(" ","",colnames(cop.example))

#remove irrelevant columns, if they exist
if(ncol(cop.example[ , -which(names(cop.example) %in% c(colnames(cop.example)[grep("English",colnames(cop.example))],"Extras","Namedoubt"))])>0){
cop.example<-cop.example[ , -which(names(cop.example) %in% c(colnames(cop.example)[grep("English",colnames(cop.example))],"Extras","Namedoubt"))]
}
cop.example<-as.data.frame(cop.example)

#add virtual indicator
cop.example$Virtual<-0

#add overflow indicator
cop.example$Overflow<-0

#add COP and year variables
cop.example$Year<-properties[1]
cop.example$Meeting<-paste(properties[2],properties[3],sep=" ")

#location varaible
location<-paste(properties[4:length(properties)],collapse=" ")
location<-gsub("(1)","",location)
location<-gsub("\\.xlsx","",location)
location<-gsub("_new","",location)
location<-gsub("Part 1","",location)
location<-gsub("Part 2","",location)
cop.example$Location<-location


}


else{
#rename COP 28 & 29 columns
colnames(cop.example)<-c("Group","Entity","Title","Name","Job Title","Division","Affiliation","Virtual","Overflow")

#remove spaces from column names, which R does not always like
colnames(cop.example)<-gsub(" ","",colnames(cop.example))

#omit blank rows
cop.example<-subset(cop.example,!is.na(cop.example$Group))

#add COP and year variables
cop.example$Year<-properties[1]
cop.example$Meeting<-paste(properties[2],properties[3],sep=" ")

#location varaible
location<-paste(properties[4:length(properties)],collapse=" ")
location<-gsub("(1)","",location)
location<-gsub("\\.xlsx","",location)
location<-gsub("_new","",location)
location<-gsub("Part 1","",location)
location<-gsub("Part 2","",location)
cop.example$Location<-location

}

#########################################################
#########step 2: fix remaining name-title issues#########
#########################################################

#If name has "H.E." or "HE." or "Dr." or "DR." at the start, move this to instead appear at the start of the person's Title-entry
cop.example$Title<-ifelse(grepl("^+HE\\.",cop.example$Name),paste(cop.example$Title,"HE.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+HE\\.",cop.example$Name),gsub("^+HE\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+HE ",cop.example$Name),paste(cop.example$Title,"HE.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+HE ",cop.example$Name),gsub("^+HE ","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+H\\.E\\.",cop.example$Name),paste(cop.example$Title,"H.E.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+H\\.E\\.",cop.example$Name),gsub("^+H\\.E\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+HL\\.E\\.",cop.example$Name),paste(cop.example$Title,"H.E.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+HL\\.E\\.",cop.example$Name),gsub("^+HL\\.E\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+HLE\\.",cop.example$Name),paste(cop.example$Title,"H.E.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+HLE\\.",cop.example$Name),gsub("^+HLE\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+H\\.L\\.E\\.",cop.example$Name),paste(cop.example$Title,"H.E.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+H\\.L\\.E\\.",cop.example$Name),gsub("^+H\\.L\\.E\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+DR\\.",cop.example$Name),paste(cop.example$Title,"DR.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+DR\\.",cop.example$Name),gsub("^+DR\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(grepl("^+Dr\\.",cop.example$Name),paste(cop.example$Title,"Dr.",sep=" "),cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Dr\\.",cop.example$Name),gsub("^+Dr\\.","",cop.example$Name),cop.example$Name)

#If Title is blank and name starts with "Mr." or "Mrs.", etc. shift that to title.
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Mr\\.",cop.example$Name), "Mr.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Mr\\.",cop.example$Name),gsub("^+Mr\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Mrs\\.",cop.example$Name), "Mrs.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Mrs\\.",cop.example$Name),gsub("^+Mrs\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Sr\\.",cop.example$Name), "Sr.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Sr\\.",cop.example$Name),gsub("^+Sr\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Sra\\.",cop.example$Name), "Sra.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Sra\\.",cop.example$Name),gsub("^+Sra\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Mme\\.",cop.example$Name), "Mme.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Mme\\.",cop.example$Name),gsub("^+Mme\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+M\\.",cop.example$Name), "M.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+M\\.",cop.example$Name),gsub("^+M\\.","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Miss.",cop.example$Name), "Ms.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Miss\\.",cop.example$Name),gsub("^+Miss\\.","",cop.example$Name),cop.example$Name)

#If Title is blank and name starts with "Mr," or "Mrs,", etc. shift that to title.
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Mr\\,",cop.example$Name), "Mr.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Mr\\,",cop.example$Name),gsub("^+Mr\\,","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Mrs\\,",cop.example$Name), "Mrs.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Mrs\\,",cop.example$Name),gsub("^+Mrs\\,","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Sr\\,",cop.example$Name), "Sr.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Sr\\,",cop.example$Name),gsub("^+Sr\\,","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Sra\\,",cop.example$Name), "Sra.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Sra\\,",cop.example$Name),gsub("^+Sra\\,","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Mme\\,",cop.example$Name), "Mme.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Mme\\,",cop.example$Name),gsub("^+Mme\\,","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+M\\,",cop.example$Name), "M.",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+M\\,",cop.example$Name),gsub("^+M\\,","",cop.example$Name),cop.example$Name)
cop.example$Title<-ifelse(cop.example$Title=="" & grepl("^+Miss",cop.example$Name), "Ms",cop.example$Title)
cop.example$Name<-ifelse(grepl("^+Miss",cop.example$Name),gsub("^+Miss","",cop.example$Name),cop.example$Name)


#fix some remaining common OCR mistakes in Name column
cop.example$Name<-gsub(" \\!"," I",cop.example$Name)
cop.example$Name<-gsub(" \\]"," I",cop.example$Name)
cop.example$Name<-gsub(" \\["," I",cop.example$Name)
cop.example$Name<-gsub("^+\\!","I",cop.example$Name)
cop.example$Name<-gsub("^+\\]","I",cop.example$Name)
cop.example$Name<-gsub("^+\\[","I",cop.example$Name)
cop.example$Name<-gsub("\\!","l",cop.example$Name)
cop.example$Name<-gsub("\\]","l",cop.example$Name)
cop.example$Name<-gsub("\\[","l",cop.example$Name)
cop.example$Name<-gsub("- ","-",cop.example$Name)
cop.example$Name<-gsub(" -","-",cop.example$Name)
cop.example$Name<-gsub(" \\.",".",cop.example$Name)
cop.example$Name<-gsub(" 0"," O",cop.example$Name)
cop.example$Name<-gsub("^+0","O",cop.example$Name)
cop.example$Name<-gsub("0","o",cop.example$Name)
cop.example$Name<-gsub("\\$","S",cop.example$Name)
cop.example$Name<-gsub("^+\\.","",cop.example$Name)
cop.example$Name<-gsub("^+\\,","",cop.example$Name)

#trim remaining leading and trailing whitepsace entries from Name and Title columns
cop.example$Title<-trimws(cop.example$Title, which = "both")
cop.example$Name<-trimws(cop.example$Name, which = "both")

#Standardizing first name last name order. This is tricky!!
#some entries are "First LAST", others are "LAST, First". 
#But, some entries have a "First Middle-Initial Last" format where the middle initial has an incorrect "," instead of an ".". For example "John M, Smith"
#First I fix the latter problem below, then reverse remaining names.

#if name contains a single letter preceded by a space followed by a comma, as in " A,", replace the comma with a period
cop.example$Name<-ifelse(grepl(" [A-Z],",cop.example$Name),gsub(",",".",cop.example$Name),cop.example$Name)

#now actually standardize first and last name based on remaining commas, by swapping last-first in those cases
for(i in 1:nrow(cop.example)){
	if(grepl(",",cop.example$Name[i])){
	temp<-strsplit(cop.example$Name[i], ", ")
	cop.example$Name[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#fix any remaining entries that end with a period
cop.example$Name<-gsub("\\.+$","",cop.example$Name)

#remove some lingering titles that may be in Name
cop.example$Name<-gsub("^+Prof\\. ","",cop.example$Name)
cop.example$Name<-gsub("^+Dr\\. ","",cop.example$Name)
cop.example$Name<-gsub("^+The Honourable ","",cop.example$Name)  
cop.example$Name<-gsub("^+The Honorable ","",cop.example$Name)

#fix one remaining issue
cop.example$Name<-gsub("^+\\‘", " and ",cop.example$Name)
cop.example$Name<-gsub("^+\\’", " and ",cop.example$Name)
cop.example$Name<-gsub("\\‘+$", " and ",cop.example$Name)
cop.example$Name<-gsub("\\’+$", " and ",cop.example$Name)

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
cop.example$Name<-trimws(cop.example$Name, which = "both")

#standardize name data to use capitals only (and always) at the start of each unigram
cop.example$Name<-str_to_title(cop.example$Name) 

#fix a common OCR issue in titles
cop.example$Title<-gsub("Mlle","Mme",cop.example$Title)
cop.example$Title<-gsub("St\\.","Sr.",cop.example$Title)
cop.example$Title<-gsub("St","Sr",cop.example$Title)
cop.example$Title<-gsub("Sta\\.","Sra.",cop.example$Title)
cop.example$Title<-gsub("Sta","Sra",cop.example$Title)
cop.example$Title<-gsub("H\\.E\\.Ms\\.","H.E. Ms.",cop.example$Title)
cop.example$Title<-gsub("H\\.E\\.Ms","H.E. Ms",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.Dr\\.","S.E. Dr.",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.Dr","S.E. Dr",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.M\\.","S.E. M.",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.M","S.E. M",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.Mme\\.","S.E. Mme.",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.Mme","S.E. Mme",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.Sr\\.","S.E. Sr.",cop.example$Title)
cop.example$Title<-gsub("S\\.E\\.Sr","S.E. Sr",cop.example$Title)
cop.example$Title<-gsub("Mine","Mme",cop.example$Title)
cop.example$Title<-gsub("H\\. E\\. Mr\\.","H.E. Mr.",cop.example$Title)
cop.example$Title<-gsub("H\\.E\\.  Mr\\.","H.E. Mr.",cop.example$Title)
cop.example$Title<-gsub("M\\. H\\.E\\.","H.E. M.",cop.example$Title)
cop.example$Title<-gsub("M\\. S\\.E\\.","S.E. M.",cop.example$Title)
cop.example$Title<-gsub("Ms\\. H\\.E\\.","H.E. Ms.",cop.example$Title)
cop.example$Title<-gsub("Mrs\\. H\\.E\\.","H.E. Mrs.",cop.example$Title)
cop.example$Title<-gsub("Mr H\\.E\\.","H.E. Mr.",cop.example$Title)
cop.example$Title<-gsub("Sr\\. S\\.E\\.","S.E. Sr.",cop.example$Title)
cop.example$Title<-gsub("Sra\\. S\\.E\\.","S.E. Sra.",cop.example$Title)
cop.example$Title<-gsub("S E\\. Sr\\.","S.E. Sr.",cop.example$Title)
cop.example$Title<-gsub("S E\\. M\\.","S.E. M.",cop.example$Title)


#create female varaible 
#NOTE: there may be other variants in other pre-cop/cop years that we need to check...perhaps recover any remaining titles that aren't already listed below across all pre-cop/cops and send them to Daria and me to review.
cop.example$female<-NA
cop.example$female[cop.example$Title=="Sir"]<-0
cop.example$female[cop.example$Title=="H.E. Mr."]<-0
cop.example$female[cop.example$Title=="H.E.Mr."]<-0
cop.example$female[cop.example$Title=="H.E. Mr"]<-0
cop.example$female[cop.example$Title=="H.E. M."]<-0
cop.example$female[cop.example$Title=="H.E. Sr."]<-0
cop.example$female[cop.example$Title=="H.E. Ms."]<-1
cop.example$female[cop.example$Title=="H.E. Mrs."]<-1
cop.example$female[cop.example$Title=="HE. Mr."]<-0
cop.example$female[cop.example$Title=="H.E. Ms"]<-1
cop.example$female[cop.example$Title=="H.E Ms."]<-1
cop.example$female[cop.example$Title=="H.E Ms"]<-1
cop.example$female[cop.example$Title=="H.E.Ms."]<-1
cop.example$female[cop.example$Title=="H.H. Mr."]<-1
cop.example$female[cop.example$Title=="H.E. Msgr."]<-0
cop.example$female[cop.example$Title=="H.E. Msgr"]<-0
cop.example$female[cop.example$Title=="Msgr."]<-0
cop.example$female[cop.example$Title=="H.E. Archbishop"]<-0
cop.example$female[cop.example$Title=="His"]<-0
cop.example$female[cop.example$Title=="Rev."]<-0
cop.example$female[cop.example$Title=="Rev"]<-0
cop.example$female[cop.example$Title=="M."]<-0
cop.example$female[cop.example$Title=="M"]<-0
cop.example$female[cop.example$Title=="H.E. Dato’ M."]<-0
cop.example$female[cop.example$Title=="Mme."]<-1
cop.example$female[cop.example$Title=="Mme"]<-1
cop.example$female[cop.example$Title=="Mr."]<-0
cop.example$female[cop.example$Title=="Mr"]<-0
cop.example$female[cop.example$Title=="M. R."]<-0
cop.example$female[cop.example$Title=="Miss"]<-1
cop.example$female[cop.example$Title=="Ms."]<-1
cop.example$female[cop.example$Title=="Ms"]<-1
cop.example$female[cop.example$Title=="Mrs."]<-1
cop.example$female[cop.example$Title=="Mrs"]<-1
cop.example$female[cop.example$Title=="S.E. M."]<-0
cop.example$female[cop.example$Title=="S.E. M"]<-0
cop.example$female[cop.example$Title=="S.E. Ms"]<-0
cop.example$female[cop.example$Title=="S.E.M."]<-0
cop.example$female[cop.example$Title=="S.E. Mme."]<-1
cop.example$female[cop.example$Title=="S.E. Mme"]<-1
cop.example$female[cop.example$Title=="S.E  Mme"]<-1
cop.example$female[cop.example$Title=="S.E. Sr."]<-0
cop.example$female[cop.example$Title=="S.E. Sr"]<-0
cop.example$female[cop.example$Title=="S.E. Sra."]<-1
cop.example$female[cop.example$Title=="S.E. Sra"]<-1
cop.example$female[cop.example$Title=="S.E.Sra."]<-1
cop.example$female[cop.example$Title=="S.E.Sr."]<-0
cop.example$female[cop.example$Title=="S.E.Mme"]<-1
cop.example$female[cop.example$Title=="Sr."]<-0
cop.example$female[cop.example$Title=="Sr"]<-0
cop.example$female[cop.example$Title=="Sr. D."]<-0
cop.example$female[cop.example$Title=="Sra."]<-1
cop.example$female[cop.example$Title=="Sra"]<-1
cop.example$female[cop.example$Title=="Srta"]<-1
cop.example$female[cop.example$Title=="Srta."]<-1
cop.example$female[cop.example$Title=="Mr. H E."]<-0   
cop.example$female[cop.example$Title=="Mr. H.E."]<-0   
cop.example$female[cop.example$Title=="Mr. HE."]<-0   
cop.example$female[cop.example$Title=="Mr. HLE."]<-0       
cop.example$female[cop.example$Title=="Ms. HE."]<-0
cop.example$female[cop.example$Title=="S.E.Ms"]<-1
cop.example$female[cop.example$Title=="Fr."]<-1
cop.example$female[cop.example$Title=="H.E. Mr. Dr."]<-0
cop.example$female[cop.example$Title=="H.E. Mr. HE."]<-0
cop.example$female[cop.example$Title=="H.E. Ms. Dr."]<-1
cop.example$female[cop.example$Title=="M. DR."]<-0       
cop.example$female[cop.example$Title=="Mr. Dr."]<-0 
cop.example$female[cop.example$Title=="Ms. Dr."]<-1   
cop.example$female[cop.example$Title=="Ms. DR."]<-1
cop.example$female[cop.example$Title=="Baron"]<-0
cop.example$female[cop.example$Title=="Dame"]<-1
cop.example$female[cop.example$Title=="Lady"]<- 1 
cop.example$female[cop.example$Title=="Lord"]<-0 
cop.example$female[cop.example$Title=="Sr. Dr."]<-0
cop.example$female[cop.example$Title=="Rev. Dr."]<-0

#Standardize title a bit more
cop.example$Title<-gsub("\\.","",cop.example$Title)
cop.example$Title<-gsub("\\,","",cop.example$Title)
cop.example$Title<-gsub("SEM","SE M",cop.example$Title)
cop.example$Title<-gsub("HEMr","HE Mr",cop.example$Title)
cop.example$Title<-gsub("HEMrs","HE Mrs",cop.example$Title)
cop.example$Title<-gsub("MrHE","HE Mr",cop.example$Title)
cop.example$Title<-gsub("MrsHE","HE Mrs",cop.example$Title)
cop.example$Title<-gsub("H E","HE",cop.example$Title)

#Always place HE at the start of a title
for(i in 1:nrow(cop.example)){
	if(grepl(" HE+$",cop.example$Title[i])){
	temp<-strsplit(cop.example$Title[i], " ")
	cop.example$Title[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#Always place SE at the start of a title
for(i in 1:nrow(cop.example)){
	if(grepl(" SE+$",cop.example$Title[i])){
	temp<-strsplit(cop.example$Title[i], " ")
	cop.example$Title[i]<-paste(temp[[1]][2],temp[[1]][1],sep=" ")
	}
}

#trim remaining leading and trailing whitepsace entries from Name column that may have arisen from above steps
cop.example$Title<-trimws(cop.example$Title, which = "both")
cop.example$Name<-trimws(cop.example$Name, which = "both")
cop.example$Title<-str_squish(cop.example$Title)
cop.example$Name<-str_squish(cop.example$Name)

###########################################
#########step 3: fix group entries#########
###########################################

#standardize entries (there may be other variants in other COPs/Pre-COPs that we need to add here
cop.example$Group<-ifelse(cop.example$Group=="InterGovernmental Organizations","Intergovernmental organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="INTERGOVERNMENTAL ORGANIZATIONS","Intergovernmental organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="ORGANISATIONS INTERGOUVERNEMENTALES","Intergovernmental organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="NON-GOVERNMENTAL ORGANIZATIONS","Non-governmental organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="ORGANISATIONS NON-GOUVERNEMENTALES","Non-governmental organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Non-Governmental Organizations","Non-governmental organizations",cop.example$Group) 
cop.example$Group<-ifelse(cop.example$Group=="OBSERVER STATES","Observer States",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Observer states","Observer States",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters","Observer States",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters.","Observer States",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Observer states and entities having received a standing invitation to participate as observers in the sessions and the work of the General Assembly and maintaining permanent observer missions at Headquarters\\.","Observer States",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="PARTIES","Parties",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="REPRESENTATIVES","Parties",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Representatives","Parties",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="REPRESENTANTS","Parties",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="INSTITUTIONS SPECIALISEES ET AUTRES ORGANISATIONS","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Specialized Agencies and Related Organizations","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND RELATED ORGANIZATIONS","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="REPRESENTATIVES OF SPECIALIZED AGENCIES AND OTHER ORGANIZATIONS OF THE UNITED NATIONS SYSTEM","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Representatives of specialized agencies and related organizations","Specialized agencies and related organizations",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="REPRESENTATIVES OF UNITED NATIONS SECRETARIAT UNITS AND BODIES","United Nations Secretariat units and bodies",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="NATIONS UNIES","United Nations Secretariat units and bodies",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Representatives of United Nations Secretariat units and bodies","United Nations Secretariat units and bodies",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="UN Secretariat units and Bodies","United Nations Secretariat units and bodies",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="Representatives of United Nations secretariat units and bodies","United Nations Secretariat units and bodies",cop.example$Group)
cop.example$Group<-ifelse(cop.example$Group=="United Nations secretariat units and bodies","United Nations Secretariat units and bodies",cop.example$Group)                  
cop.example$Group<-ifelse(cop.example$Group=="United Nations secretariat units and related bodies","United Nations Secretariat units and bodies" ,cop.example$Group) 
cop.example$Group<-ifelse(cop.example$Group=="United Nations specialized agencies and related organizations","United Nations Secretariat units and bodies",cop.example$Group) 

#create binary vars
cop.example$IGO<-0
cop.example$IGO[cop.example$Group=="Intergovernmental organizations"]<-1
cop.example$NGO<-0
cop.example$NGO[cop.example$Group=="Non-governmental organizations"]<-1
cop.example$Observer<-0
cop.example$Observer[cop.example$Group=="Observer States"]<-1
cop.example$Party<-0
cop.example$Party[cop.example$Group=="Parties"]<-1
cop.example$IO<-0
cop.example$IO[cop.example$Group=="Specialized agencies and related organizations"]<-1
cop.example$IO[cop.example$Group=="United Nations Secretariat units and bodies"]<-1


###########################################
#########step 4: fix entity entries########
###########################################

#standardize capitalization format
cop.example$Entity<-str_to_title(cop.example$Entity) 

#not all years have countries spelled in a foreign language (usually French or Spanish) but some do. 
#other cases have misspellings or OCR problems
#Let's fix all of these manually here:
cop.example$Entity[cop.example$Entity=="Albanie"]<-"Albania"                                         
cop.example$Entity[cop.example$Entity=="Algerie"]<-"Algeria"                                             
cop.example$Entity[cop.example$Entity=="Allemagne"]<-"Germany" 
cop.example$Entity[cop.example$Entity=="Allemagne (Suite)"]<-"Germany" 
cop.example$Entity[cop.example$Entity=="Antigua-Et-Barbuda"]<-"Antigua And Barbuda"     
cop.example$Entity[cop.example$Entity=="Antigua Et Barbuda"]<-"Antigua And Barbuda" 
cop.example$Entity[cop.example$Entity=="Arabie Saoudite"]<-"Saudi Arabia"   
cop.example$Entity[cop.example$Entity=="Arabie Saoudite (Suite)"]<-"Saudi Arabia" 
cop.example$Entity[cop.example$Entity=="Argentine" ]<-"Argentina"                                          
cop.example$Entity[cop.example$Entity=="Armenie"]<-"Armenia"                                            
cop.example$Entity[cop.example$Entity=="Australie" ]<-"Australia"
cop.example$Entity[cop.example$Entity=="Australie (Suite)" ]<-"Australia"         
cop.example$Entity[cop.example$Entity=="Autriche"   ]<-"Austria"
cop.example$Entity[cop.example$Entity=="Autriche (Suite)"    ]<-"Austria"
cop.example$Entity[cop.example$Entity=="Azerbaidjan"    ]<-"Azerbaijan"
cop.example$Entity[cop.example$Entity=="Bahrein"  ]<-"Bahrain"                                         
cop.example$Entity[cop.example$Entity=="Bangladesh" ]<-"Bangladesh"                                       
cop.example$Entity[cop.example$Entity=="Barbade"  ]<-"Barbados"                                         
cop.example$Entity[cop.example$Entity=="Belgique"  ]<-"Belgium"                                          
cop.example$Entity[cop.example$Entity=="Belize" ]<-"Belize"                                            
cop.example$Entity[cop.example$Entity=="Benin" ]<-"Benin"                                             
cop.example$Entity[cop.example$Entity=="Bhoutan"   ]<-"Bhutan"  
cop.example$Entity[cop.example$Entity=="Bolivia (Plurinational State Of)"  ]<-"Bolivia"
cop.example$Entity[cop.example$Entity=="Bolivie"  ]<-"Bolivia"                                          
cop.example$Entity[cop.example$Entity=="Botswana"]<-"Bostwana"                                           
cop.example$Entity[cop.example$Entity=="Bresil" ]<-"Brazil"                                           
cop.example$Entity[cop.example$Entity=="Bulgarie"  ]<-"Bulgaria"                                         
cop.example$Entity[cop.example$Entity=="Burkina Faso"   ]<-"Burkina Faso"                                    
cop.example$Entity[cop.example$Entity=="Cambodge"  ]<-"Cambodia"                                         
cop.example$Entity[cop.example$Entity=="Cameroun"  ]<-"Cameroon"                                         
cop.example$Entity[cop.example$Entity=="Canada"   ]<-"Canada"  
cop.example$Entity[cop.example$Entity=="Canada (Suite)"   ]<-"Canada"                                    
cop.example$Entity[cop.example$Entity=="Cap-Vert"   ]<-"Cape Verde"                                          
cop.example$Entity[cop.example$Entity=="Chili"]<-"Chile"                                              
cop.example$Entity[cop.example$Entity=="Chine" ]<-"China" 
cop.example$Entity[cop.example$Entity=="Republique Populaire De Chine"]<-"China" 
cop.example$Entity[cop.example$Entity=="Chine (Suite)"    ]<-"China"   
cop.example$Entity[cop.example$Entity=="Colombie"  ]<-"Colombia"
cop.example$Entity[cop.example$Entity=="Columbia"]<-"Colombia"
cop.example$Entity[cop.example$Entity=="Communaute Europeenne" ]<-"European Community"                               
cop.example$Entity[cop.example$Entity=="Comores"  ]<-"Comoros"                                          
cop.example$Entity[cop.example$Entity=="Costa Rica"  ]<-"Costa Rica"  
cop.example$Entity[cop.example$Entity=="Costarica"  ]<-"Costa Rica"                
cop.example$Entity[cop.example$Entity=="Cote D' Ivoire" ]<-"Cote D'ivoire"
cop.example$Entity[cop.example$Entity=="Cote D’ Ivoire" ]<-"Cote D'ivoire"
cop.example$Entity[cop.example$Entity=="Cote D’Ivoire" ]<-"Cote D'ivoire"
cop.example$Entity[cop.example$Entity=="Côte D’ivoire" ]<-"Cote D'ivoire" 
cop.example$Entity[cop.example$Entity=="Côte D'ivoire" ]<-"Cote D'ivoire" 
cop.example$Entity[cop.example$Entity=="Côté D'ivoire"]<-"Cote D'ivoire" 
cop.example$Entity[cop.example$Entity=="Côte D’ Ivoire"]<-"Cote D'ivoire"
cop.example$Entity[cop.example$Entity=="Cote D’ivoire"]<-"Cote D'ivoire"
cop.example$Entity[cop.example$Entity=="Croatie" ]<-"Croatia"                                           
cop.example$Entity[cop.example$Entity=="Cuba"  ]<-"Cuba"       
cop.example$Entity[cop.example$Entity=="Chypre"  ]<-"Cyprus" 
cop.example$Entity[cop.example$Entity=="Danemark" ]<-"Denmark"                                          
cop.example$Entity[cop.example$Entity=="Djibouti"  ]<-"Djibouti"                                         
cop.example$Entity[cop.example$Entity=="Egypte"  ]<-"Egypt"                                           
cop.example$Entity[cop.example$Entity=="El Salvador"    ]<-"El Salvador"                                     
cop.example$Entity[cop.example$Entity=="Emirats Arabes Unis"    ]<-"United Arab Emirates"                           
cop.example$Entity[cop.example$Entity=="Equateur"  ]<-"Ecuador"                                         
cop.example$Entity[cop.example$Entity=="Erythree"  ]<-"Eritrea"                                         
cop.example$Entity[cop.example$Entity=="Espagne"   ]<-"Spain"                                         
cop.example$Entity[cop.example$Entity=="Estonie"   ]<-"Estonia"                                        
cop.example$Entity[cop.example$Entity=="Etats-Unis D' Amerique"    ]<-"United States Of America" 
cop.example$Entity[cop.example$Entity=="Etats-Unis D'amerique"  ]<-"United States Of America"                             
cop.example$Entity[cop.example$Entity=="Etats-Unis D'amerique (Suite)"]<-"United States Of America" 
cop.example$Entity[cop.example$Entity=="Etats-Unis D’amérique"]<-"United States Of America"                                
cop.example$Entity[cop.example$Entity=="Etats-Unis D’'Amérique"]<-"United States Of America" 
cop.example$Entity[cop.example$Entity=="Etats-Unis D’amerique" ]<-"United States Of America"                                    
cop.example$Entity[cop.example$Entity=="Etats-Unis D’amerique (Suite)"]<-"United States Of America"               
cop.example$Entity[cop.example$Entity=="Ethiopie"  ]<-"Ethiopia"                                          
cop.example$Entity[cop.example$Entity=="Federation De Russie"  ]<-"Russian Federation"                             
cop.example$Entity[cop.example$Entity=="Fidj1"    ]<-"Fiji"  
cop.example$Entity[cop.example$Entity=="Fidji"    ]<-"Fiji"                    
cop.example$Entity[cop.example$Entity=="Finlande"    ]<-"Finland"                                        
cop.example$Entity[cop.example$Entity=="France"    ]<-"France"
cop.example$Entity[cop.example$Entity=="France (Suite)"    ]<-"France"        
cop.example$Entity[cop.example$Entity=="Gambie"  ]<-"Gambia"                                            
cop.example$Entity[cop.example$Entity=="Georgie"    ]<-"Georgia"                                         
cop.example$Entity[cop.example$Entity=="Ghana"  ]<-"Ghana"      
cop.example$Entity[cop.example$Entity=="Grenade"  ]<-"Grenada"                
cop.example$Entity[cop.example$Entity=="Grece"   ]<-"Greece"                                           
cop.example$Entity[cop.example$Entity=="Guatemala"  ]<-"Guatemala"
cop.example$Entity[cop.example$Entity=="Guinee- Bissau" ]<-"Guinea-Bissau"  
cop.example$Entity[cop.example$Entity=="Guinee-Bissau"  ]<-"Guinea-Bissau"           
cop.example$Entity[cop.example$Entity=="Guinea-Bissau"   ]<-"Guinea-Bissau"                                  
cop.example$Entity[cop.example$Entity=="Guinee"     ]<-"Guinea" 
cop.example$Entity[cop.example$Entity=="Guinee Equatoriale"]<-"Equatorial Guinea"
cop.example$Entity[cop.example$Entity=="Guyane"   ]<-"Guyana"                                          
cop.example$Entity[cop.example$Entity=="Honduras"   ]<-"Honduras"                                          
cop.example$Entity[cop.example$Entity=="Hongrie"   ]<-"Hungary"
cop.example$Entity[cop.example$Entity=="Iles Cook"   ]<-"Cook Islands" 
cop.example$Entity[cop.example$Entity=="Iles Marshall"   ]<-"Marshall Islands"                                   
cop.example$Entity[cop.example$Entity=="Iles Salomon"  ]<-"Solomon Islands"                                      
cop.example$Entity[cop.example$Entity=="Inde"   ]<-"India"                                           
cop.example$Entity[cop.example$Entity=="Indonesie"   ]<-"Indonesia" 
cop.example$Entity[cop.example$Entity=="Indonesie (Suite)"  ]<-"Indonesia" 
cop.example$Entity[cop.example$Entity=="Indonesie ( Suite)"]<-"Indonesia" 
cop.example$Entity[cop.example$Entity=="Irlande"  ]<-"Ireland"                                          
cop.example$Entity[cop.example$Entity=="Islande"   ]<-"Iceland"                                          
cop.example$Entity[cop.example$Entity=="Italie"     ]<-"Italy"                                         
cop.example$Entity[cop.example$Entity=="Jamaique"  ]<-"Jamaica"                                          
cop.example$Entity[cop.example$Entity=="Japon"  ]<-"Japan"    
cop.example$Entity[cop.example$Entity=="Japon (Suite)"]<-"Japan"
cop.example$Entity[cop.example$Entity=="Japan (Suite)"]<-"Japan"
cop.example$Entity[cop.example$Entity=="Jordanie"  ]<-"Jordan"
cop.example$Entity[cop.example$Entity=="Joordan"  ]<-"Jordan"
cop.example$Entity[cop.example$Entity=="Kenya"  ]<-"Kenya"                                            
cop.example$Entity[cop.example$Entity=="Kiribati" ]<-"Kiribati"                                          
cop.example$Entity[cop.example$Entity=="Koweit"   ]<-"Kuwait"                                         
cop.example$Entity[cop.example$Entity=="Lesotho"   ]<-"Lesotho"                                           
cop.example$Entity[cop.example$Entity=="Lettonie"    ]<-"Latvia"                                       
cop.example$Entity[cop.example$Entity=="Liban"     ]<-"Lebanon"                                         
cop.example$Entity[cop.example$Entity=="Liechtenstein"  ]<-"Liechtenstein"                                  
cop.example$Entity[cop.example$Entity=="Lituanie"      ]<-"Lithuania"                                    
cop.example$Entity[cop.example$Entity=="Luxembourg"  ]<-"Luxembourg"                                        
cop.example$Entity[cop.example$Entity=="Malaisie"   ]<-"Malaysia"                                        
cop.example$Entity[cop.example$Entity=="Malawi"   ]<-"Malawi"                                         
cop.example$Entity[cop.example$Entity=="Maldives" ]<-"Maldives"                                           
cop.example$Entity[cop.example$Entity=="Mali"     ]<-"Mali"                                          
cop.example$Entity[cop.example$Entity=="Malte"    ]<-"Malta"                                          
cop.example$Entity[cop.example$Entity=="Maroc"   ]<-"Morocco"                                          
cop.example$Entity[cop.example$Entity=="Maurice" ]<-"Mauritius"                                           
cop.example$Entity[cop.example$Entity=="Mauritanie"  ]<-"Mauritania"                                       
cop.example$Entity[cop.example$Entity=="Mexique (Suite)"   ]<-"Mexico" 
cop.example$Entity[cop.example$Entity=="Mexique"   ]<-"Mexico"  
cop.example$Entity[cop.example$Entity=="Micronesie"    ]<-"Micronesia (Federated States Of)"                
cop.example$Entity[cop.example$Entity=="Micronesie (Etats Federes De)"    ]<-"Micronesia (Federated States Of)"                  
cop.example$Entity[cop.example$Entity=="Monaco"      ]<-"Monaco"                                         
cop.example$Entity[cop.example$Entity=="Mongolie"  ]<-"Mongolia"                                          
cop.example$Entity[cop.example$Entity=="Myanmar"  ]<-"Myanmar"                                        
cop.example$Entity[cop.example$Entity=="Namibie"    ]<-"Namibia"                                       
cop.example$Entity[cop.example$Entity=="Nepal"    ]<-"Nepal"                                           
cop.example$Entity[cop.example$Entity=="Nicaragua"    ]<-"Nicaragua"                                      
cop.example$Entity[cop.example$Entity=="Niger"      ]<-"Niger"                                        
cop.example$Entity[cop.example$Entity=="Nigeria"      ]<-"Nigeria"                                      
cop.example$Entity[cop.example$Entity=="Nioue"       ]<-"Niue"                                      
cop.example$Entity[cop.example$Entity=="Norvege"       ]<-"Norway"                                    
cop.example$Entity[cop.example$Entity=="Nouvelle-Zelande"  ]<-"New Zealand" 
cop.example$Entity[cop.example$Entity=="Nouvelle~Zelande"    ]<-"New Zealand"
cop.example$Entity[cop.example$Entity=="Nouvelle- Zelande"      ]<-"New Zealand"                    
cop.example$Entity[cop.example$Entity=="Oman"         ]<-"Oman" 
cop.example$Entity[cop.example$Entity=="Oman (Suite)"         ]<-"Oman"        
cop.example$Entity[cop.example$Entity=="Ouganda"    ]<-"Uganda"                                        
cop.example$Entity[cop.example$Entity=="Ouzbekistan"   ]<-"Uzbekistan"                                      
cop.example$Entity[cop.example$Entity=="Pakistan"    ]<-"Pakistan"                                         
cop.example$Entity[cop.example$Entity=="Panama"   ]<-"Panama"                                          
cop.example$Entity[cop.example$Entity=="Paraguay"  ]<-"Paraguay"   
cop.example$Entity[cop.example$Entity=="Pays-Bas (Suite)"]<-"Netherlands"    
cop.example$Entity[cop.example$Entity=="Pays-Bas" ]<-"Netherlands"                                          
cop.example$Entity[cop.example$Entity=="Perou" ]<-"Peru"       
cop.example$Entity[cop.example$Entity=="Perou (Suite)"]<-"Peru" 
cop.example$Entity[cop.example$Entity=="Philippines"  ]<-"Philippines"  
cop.example$Entity[cop.example$Entity=="Philippines (Suite)"    ]<-"Philippines"                    
cop.example$Entity[cop.example$Entity=="Pologne"    ]<-"Poland"                                        
cop.example$Entity[cop.example$Entity=="Portugal"    ]<-"Portugal"    
cop.example$Entity[cop.example$Entity=="Republique Socialiste Sovietique De Bielorussie"]<-"Belarus"
cop.example$Entity[cop.example$Entity=="Qatar"    ]<-"Qatar"                                          
cop.example$Entity[cop.example$Entity=="Republique Arabe Syrienne"]<-"Syrian Arab Republic"
cop.example$Entity[cop.example$Entity=="République Arabe Syrienne"]<-"Syrian Arab Republic"
cop.example$Entity[cop.example$Entity=="Republique Centrafricaine" ]<-"Central African Republic"                         
cop.example$Entity[cop.example$Entity=="Republique De Coree"   ]<-"Republic Of Korea"          
cop.example$Entity[cop.example$Entity=="Republique De Coree (Suite)"]<-"Republic Of Korea"     
cop.example$Entity[cop.example$Entity=="Republique De Moldova"   ]<-"Moldova, Republic Of"   
cop.example$Entity[cop.example$Entity=="Lao People’s Democratic Republic"   ]<-"Lao Peoples Democratic Republic" 
cop.example$Entity[cop.example$Entity=="Republique Democratique Populaire Lao"   ]<-"Lao Peoples Democratic Republic"  
cop.example$Entity[cop.example$Entity=="Lao People's Democratic Republic" ]<-"Lao Peoples Democratic Republic" 
cop.example$Entity[cop.example$Entity=="Democratic People’s Republic Of Korea" ]<-"Democratic People's Republic Of Korea"
cop.example$Entity[cop.example$Entity=="Democratic Peoples Republic Of Korea" ]<-"Democratic People's Republic Of Korea"        
cop.example$Entity[cop.example$Entity=="Republique Populaire Democratique De Coree" ]<-"Democratic People's Republic Of Korea"      
cop.example$Entity[cop.example$Entity=="Republique Tcheque"     ]<-"Czech Republic"  
cop.example$Entity[cop.example$Entity=="Czechia"     ]<-"Czech Republic"
cop.example$Entity[cop.example$Entity=="Republique-Unie De Tanzanite"]<-"United Republic Of Tanzania" 
cop.example$Entity[cop.example$Entity=="Republique-Unie De Tanzanie" ]<-"United Republic Of Tanzania"                        
cop.example$Entity[cop.example$Entity=="Roumanie"   ]<-"Romania"            
cop.example$Entity[cop.example$Entity=="Royaume-Uni De Grande-Bretagne Et D'irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
cop.example$Entity[cop.example$Entity=="Royaume-Uni De Grande Bretagne Et D Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland"
cop.example$Entity[cop.example$Entity=="Royaume-Uni De Grande Bretagne Et D' Irlande Du Nord"]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Entity[cop.example$Entity=="Royaume- Uni De Grande- Bretagne Et D’irlande Du Nord" ]<-"United Kingdom Of Great Britain And Northern Ireland"       
cop.example$Entity[cop.example$Entity=="Royaume-Uni De Grande- Bretagne Et D’irlande Du Nord (Suite)" ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Entity[cop.example$Entity=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord"     ]<-"United Kingdom Of Great Britain And Northern Ireland"     
cop.example$Entity[cop.example$Entity=="Royaume-Uni De Grande Bretagne Et D’irlande Du Nord (Suite)"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Entity[cop.example$Entity=="United Kingdom"  ]<-"United Kingdom Of Great Britain And Northern Ireland" 
cop.example$Entity[cop.example$Entity=="Saint-Kitts-Et-Nevis" ]<-"Saint Kitts And Nevis"                            
cop.example$Entity[cop.example$Entity=="Sainte-Lucie" ]<-"Saint Lucia"  
cop.example$Entity[cop.example$Entity=="Sao Tome-Et-Principe"    ]<-"Sao Tome and Principe"         
cop.example$Entity[cop.example$Entity=="Samoa" ]<-"Samoa"   
cop.example$Entity[cop.example$Entity=="Papouasie-Nouvelle-Guinee"     ]<-"Papua New Guinea" 
cop.example$Entity[cop.example$Entity=="Papouasie-Nouvelle- Guinee"     ]<-"Papua New Guinea" 
cop.example$Entity[cop.example$Entity=="Senegal"    ]<-"Senegal"                                         
cop.example$Entity[cop.example$Entity=="Sierra Leone"  ]<-"Sierra Leone"                                    
cop.example$Entity[cop.example$Entity=="Slovaquie"   ]<-"Slovak Republic"  
cop.example$Entity[cop.example$Entity=="Slovakia"   ]<-"Slovak Republic"
cop.example$Entity[cop.example$Entity=="Slovenie"  ]<-"Slovenia"                                         
cop.example$Entity[cop.example$Entity=="Soudan"  ]<-"Sudan"                                           
cop.example$Entity[cop.example$Entity=="Sri Lanka"  ]<-"Sri Lanka"
cop.example$Entity[cop.example$Entity=="Srilanka"   ]<-"Sri Lanka"
cop.example$Entity[cop.example$Entity=="Suede"   ]<-"Swedan" 
cop.example$Entity[cop.example$Entity=="Suede (Suite)"   ]<-"Swedan" 
cop.example$Entity[cop.example$Entity=="Sutsse"    ]<-"Switzerland"               
cop.example$Entity[cop.example$Entity=="Suisse" ]<-"Switzerland"                                           
cop.example$Entity[cop.example$Entity=="Tchad"  ]<-"Chad"                                             
cop.example$Entity[cop.example$Entity=="Thailand" ]<-"Thailand"  
cop.example$Entity[cop.example$Entity=="Thailande (Suite)" ]<-"Thailand"   
cop.example$Entity[cop.example$Entity=="Jamahiriya Arabe Libyenne"]<-"Libyan Arab Jamahiriya"       
cop.example$Entity[cop.example$Entity=="Libya"]<-"Libyan Arab Jamahiriya"
cop.example$Entity[cop.example$Entity=="Togo"   ]<-"Togo"                                            
cop.example$Entity[cop.example$Entity=="Trinite-Et-Tobago"   ]<-"Trinidad And Tobago"
cop.example$Entity[cop.example$Entity=="Trinite-Et- Tobago"  ]<-"Trinidad And Tobago"
cop.example$Entity[cop.example$Entity=="Tunisie"   ]<-"Tunisia"   
cop.example$Entity[cop.example$Entity=="Tchecoslovaquie" ] <-"Czechoslovakia"                                
cop.example$Entity[cop.example$Entity=="Thailande"    ]<-"Thailand" 
cop.example$Entity[cop.example$Entity=="Turkmenistan" ]<-"Turkmenistan"                                     
cop.example$Entity[cop.example$Entity=="Uruguay"     ]<-"Uruguay"                                      
cop.example$Entity[cop.example$Entity=="Vanuatu"    ]<-"Vanuatu"    
cop.example$Entity[cop.example$Entity=="Venezuela (Bolivarian Republic Of)"]<-"Venezuela"
cop.example$Entity[cop.example$Entity=="Venezuela"    ]<-"Venezuela" 
cop.example$Entity[cop.example$Entity=="Venezuela (Suite)"    ]<-"Venezuela" 
cop.example$Entity[cop.example$Entity=="Vietnam" ]<-"Viet Nam" 
cop.example$Entity[cop.example$Entity=="Viet Nam." ]<-"Viet Nam"                                          
cop.example$Entity[cop.example$Entity=="Yemen"    ]<-"Yemen"                                          
cop.example$Entity[cop.example$Entity=="Zaire"   ]<-"Democratic Republic of The Congo"                
cop.example$Entity[cop.example$Entity=="Zambie"  ]<-"Zambia"                                           
cop.example$Entity[cop.example$Entity=="Zimbabwe"]<-"Zimbabwe"
cop.example$Entity[cop.example$Entity=="Afrique Du Sud"    ]<-"South Africa"                                                                                                                     
cop.example$Entity[cop.example$Entity=="Gabon"   ]<-"Gabon"                                                                                                                              
cop.example$Entity[cop.example$Entity=="Haiti"       ]<-"Haiti"                                                                                                                           
cop.example$Entity[cop.example$Entity=="Tran (Islamic Republic Of)"  ]<-"Iran (Islamic Republic Of)" 
cop.example$Entity[cop.example$Entity=="Iran (Republique Islamique De)"  ]<-"Iran (Islamic Republic Of)"   
cop.example$Entity[cop.example$Entity=="Iran (Republique Islamique D’)"    ]<-"Iran (Islamic Republic Of)"                                  
cop.example$Entity[cop.example$Entity=="Iraq"     ]<-"Iraq"                                                                                                                              
cop.example$Entity[cop.example$Entity=="Israel"     ]<-"Israel"  
cop.example$Entity[cop.example$Entity=="The Former Yugoslay Republic Of Macedonia" ]<-"The Former Yugoslav Republic Of Macedonia" 
cop.example$Entity[cop.example$Entity=="North Macedonia"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                                            
cop.example$Entity[cop.example$Entity=="L'ex-Republique Yougoslave De Macedoi!"  ]<-"The Former Yugoslav Republic Of Macedonia"                                                                                               
cop.example$Entity[cop.example$Entity=="Madagascar"          ]<-"Madagascar"                                                                                                                  
cop.example$Entity[cop.example$Entity=="Republique Dominicaine"    ]<-"Domnican Republic"                                                                                                             
cop.example$Entity[cop.example$Entity=="Saint-Siege"    ]<-"Holy See" 
cop.example$Entity[cop.example$Entity=="Saint Siege"     ]<-"Holy See"                                                                                                           
cop.example$Entity[cop.example$Entity=="Singapour"   ]<-"Singapore"                                                                                                                          
cop.example$Entity[cop.example$Entity=="Swaziland"   ]<-"Swaziland"      
cop.example$Entity[cop.example$Entity=="Türkiye"   ]<-"Turkey"                                                    
cop.example$Entity[cop.example$Entity=="Turquie"     ]<-"Turkey"                                                                                                                           
cop.example$Entity[cop.example$Entity=="Ukrainian Soviet Socialist Republic"]<-"Ukraine" 
cop.example$Entity[cop.example$Entity=="Ukraine"]<-"Ukraine" 
cop.example$Entity[cop.example$Entity=="Union Des Republiques Socialistes Sovietiques" ]<-"Union of Soviet Socialist Republics"
cop.example$Entity[cop.example$Entity=="Yougoslavie" ]<-"Yugoslavia"
cop.example$Entity[cop.example$Entity=="Swedan" ]<-"Sweden"                                             
cop.example$Entity[cop.example$Entity=="Solomon Islands (Continued0" ]<-"Solomon Islands"                                            
cop.example$Entity[cop.example$Entity=="Serbia and Montenegro" ]<-"Serbia"                                            
cop.example$Entity[cop.example$Entity=="Seychelles (Continued0" ]<-"Seychelles"                                          
cop.example$Entity[cop.example$Entity=="Republic of Cabo Verde"]<-"Cape Verde"                                     
cop.example$Entity[cop.example$Entity=="Cote D Ivoire"]<-"Cote D'ivoire"                                         
cop.example$Entity[cop.example$Entity=="Côte D`Ivoire"]<-"Cote D'ivoire"                               
cop.example$Entity[cop.example$Entity=="Domnican Republic"]<-"Dominican Republic" 
cop.example$Entity[cop.example$Entity=="Eswatini"]<-"Swaziland" 
cop.example$Entity[cop.example$Entity=="Holysee" ]<-"Holy See" 
cop.example$Entity[cop.example$Entity=="Kazakstan"]<-"Kazakhstan"
cop.example$Entity[cop.example$Entity=="Moldova, Republic Of"]<- "Moldova"   
cop.example$Entity[cop.example$Entity=="Flj1" ]<-"Fiji"  
cop.example$Entity[cop.example$Entity=="Guiea"]<-"Guinea"                                           
cop.example$Entity[cop.example$Entity=="Georgla"]<-"Georgia"                                          
cop.example$Entity[cop.example$Entity=="State of Palestine"]<-"Palestine" 
cop.example$Entity[cop.example$Entity=="Nive"]<-"Niue"                                                        
cop.example$Entity[cop.example$Entity=="Cape Verde" ]<-"Cabo Verde" 

#fix a few minor over-capitalization issues
cop.example$Entity<-gsub(" Of ", " of ",cop.example$Entity)
cop.example$Entity<-gsub(" Of)", " of)",cop.example$Entity)
cop.example$Entity<-gsub(" And ", " and ",cop.example$Entity)
cop.example$Entity<-gsub(" For ", " for ",cop.example$Entity)

#fix one remaining issue
cop.example$Entity<-gsub("^+\\‘", " and ",cop.example$Entity)
cop.example$Entity<-gsub("^+\\’", " and ",cop.example$Entity)
cop.example$Entity<-gsub("\\‘+$", " and ",cop.example$Entity)
cop.example$Entity<-gsub("\\’+$", " and ",cop.example$Entity)

#fix a few more cases
cop.example$Entity[cop.example$Entity=="Iran(Islamic Republic of)" ]<-"Iran (Islamic Republic of)" 
cop.example$Entity[cop.example$Entity=="Netherlands (Kingdom of The)"]<-"Netherlands" 
cop.example$Entity[cop.example$Entity==" and Grenada"]<-"Grenada"                                         
cop.example$Entity[cop.example$Entity==" and Kenya"]<-"Kenya"   
cop.example$Group[cop.example$Entity=="Organisation Mondiale Du Commerce (Omc)"]<-"Specialized agencies and related organizations"  
cop.example$Entity[cop.example$Entity=="Republic of Moldova"]<- "Moldova" 
cop.example$Entity[cop.example$Entity=="Republic of Cabo Verde"]<- "Cabo Verde" 
cop.example$Entity[cop.example$Entity=="Serbia and Montenegro" ]<-"Serbia"  
cop.example$Entity[cop.example$Entity=="State of Palestine"]<-"Palestine" 
cop.example$Entity[cop.example$Entity=="Iran (Islamic Republic of)"]<-"Iran"                                                 
cop.example$Entity[cop.example$Entity=="Rhanda"]<-"Rwanda"   
cop.example$Entity[cop.example$Entity=="Dominique"]<-"Dominica" 


#Further standardization
cop.example$Entity<-gsub("\\("," (",cop.example$Entity)
cop.example$Entity<-gsub("\\)",") ",cop.example$Entity)
cop.example$Entity<-gsub("\\’","'",cop.example$Entity)
cop.example$Entity<-gsub("\\( ","(",cop.example$Entity)
cop.example$Entity<-gsub(" \\)",")",cop.example$Entity)
cop.example$Entity<-str_squish(cop.example$Entity)
cop.example$Entity<-trimws(cop.example$Entity, which = "both")

#########################################################
#########step 5: Job title, Division, Affiliation########
#########################################################

#fix a few minor issues
cop.example$JobTitle<-gsub("\\-+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\,+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\.+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("^+\\,", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("^+\\.", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("^+\\‘", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("^+\\’", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\‘+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\’+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\-+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("^+\\¡", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\¡+$", "",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\("," (",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\)",") ",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\’","'",cop.example$JobTitle)
cop.example$JobTitle<-gsub("\\( ","(",cop.example$JobTitle)
cop.example$JobTitle<-gsub(" \\)",")",cop.example$JobTitle)

cop.example$Division<-gsub("\\-+$", "",cop.example$Division)
cop.example$Division<-gsub("\\,+$", "",cop.example$Division)
cop.example$Division<-gsub("\\.+$", "",cop.example$Division)
cop.example$Division<-gsub("^+\\,", "",cop.example$Division)
cop.example$Division<-gsub("^+\\.", "",cop.example$Division)
cop.example$Division<-gsub("^+\\‘", "",cop.example$Division)
cop.example$Division<-gsub("^+\\’", "",cop.example$Division)
cop.example$Division<-gsub("\\‘+$", "",cop.example$Division)
cop.example$Division<-gsub("\\’+$", "",cop.example$Division)
cop.example$Division<-gsub("\\-+$", "",cop.example$Division)
cop.example$Division<-gsub("^+\\¡", "",cop.example$Division)
cop.example$Division<-gsub("\\¡+$", "",cop.example$Division)
cop.example$Division<-gsub("\\("," (",cop.example$Division)
cop.example$Division<-gsub("\\)",") ",cop.example$Division)
cop.example$Division<-gsub("\\’","'",cop.example$Division)
cop.example$Division<-gsub("\\( ","(",cop.example$Division)
cop.example$Division<-gsub(" \\)",")",cop.example$Division)

cop.example$Affiliation<-gsub("\\-+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\,+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\.+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("^+\\,", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("^+\\.", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("^+\\‘", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("^+\\’", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\‘+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\’+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\-+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("^+\\¡", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\¡+$", "",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\("," (",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\)",") ",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\’","'",cop.example$Affiliation)
cop.example$Affiliation<-gsub("\\( ","(",cop.example$Affiliation)
cop.example$Affiliation<-gsub(" \\)",")",cop.example$Affiliation)


#fix one other potential issue (if Division is blank and Affiliation just says "Climate Change", always shift this to Division)
for(i in 1:nrow(cop.example)){
	if((is.na(cop.example$Division[i]) & !is.na(cop.example$Affiliation[i]))){
		if(cop.example$Affiliation[i]=="Climate Change"){
		cop.example$Division[i]<-"Climate Change"
		cop.example$Affiliation[i]<-NA
		} 
	}
}

#trim remaining leading and trailing whitepsace
cop.example$JobTitle<-str_squish(cop.example$JobTitle)
cop.example$Division<-str_squish(cop.example$Division)
cop.example$Affiliation<-str_squish(cop.example$Affiliation)
cop.example$JobTitle<-trimws(cop.example$JobTitle, which = "both")
cop.example$Division<-trimws(cop.example$Division, which = "both")
cop.example$Affiliation<-trimws(cop.example$Affiliation, which = "both")

#now combine
cop.full<-rbind(cop.full,cop.example)

}#close full loop

#add column names
colnames(cop.full)<-c("Group_Type","Delegation","Honorific","Person_Name","Job_Title","Division","Affiliation","Virtual","Overflow","Year","Meeting","Location",    "Female","IGO","NGO","Observer","Party","IO" )

#fix location mentions
cop.full$Location<-gsub("\\(","",cop.full$Location)
cop.full$Location<-gsub("\\)","",cop.full$Location)
cop.full$Location<-gsub("new","",cop.full$Location)
cop.full$Location<-gsub("Part","",cop.full$Location)
cop.full$Location<-str_squish(cop.full$Location)
cop.full$Location<-trimws(cop.full$Location, which = "both")

#set working directory
setwd("")

#now save file
write.csv(cop.full,"cops.cleaned.csv",row.names=FALSE)



  