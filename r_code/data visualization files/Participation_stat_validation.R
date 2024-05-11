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
setwd("C:/Users/bagozzib/Desktop/Completed Excel Files/")

#read-in cops
cops<-read.csv("cops.cleaned.csv")

#set working directory
setwd("C:/Users/bagozzib/Desktop/Completed Excel Files/validation")

#read-in participation statistics
part_stats<-read.csv("Participation_Statistics.csv")

#omit virtual from both cases
part_stats<-subset(part_stats,part_stats$Location!="UAE (virtual)")
cops<-subset(cops,cops$Virtual!=1)

#fix issues for COP 6
#part_stats$Meeting<-ifelse((part_stats$Meeting=="COP 6" & part_stats$Location=="Hague"), "COP 6 - Hague",part_stats$Meeting)
part_stats$Meeting<-ifelse((part_stats$Meeting=="COP 6" & part_stats$Location=="Bonn"), "COP 6.5",part_stats$Meeting)
#cops$Meeting<-ifelse((cops$Meeting=="COP 6" & cops$Location=="Hague"), "COP 6 - Hague",part_stats$Meeting)
cops$Meeting<-ifelse((cops$Meeting=="COP 6" & cops$Location=="Bonn"), "COP 6.5",cops$Meeting)

#set up COP list
cop.list<-c("COP 1","COP 2", "COP 3", "COP 4", "COP 5","COP 6", "COP 6.5", "COP 7", "COP 8", "COP 9", "COP 10", "COP 11", "COP 12", "COP 13", "COP 14", "COP 15", "COP 16", "COP 17", "COP 18", "COP 19", "COP 20", "COP 21", "COP 22", "COP 23", "COP 24", "COP 25", "COP 26", "COP 27", "COP 28")
#cop.list<-c("COP 1","COP 2", "COP 3", "COP 4", "COP 5","COP 6", "COP 7", "COP 8", "COP 9", "COP 10", "COP 11", "COP 12", "COP 13", "COP 14", "COP 15", "COP 16", "COP 17", "COP 18", "COP 19", "COP 20", "COP 21", "COP 22", "COP 23", "COP 24", "COP 25", "COP 26", "COP 27", "COP 28")

##################################################################
#######################Group Count Comparisons####################
##################################################################

#set up blank storage object
group_table<-NULL

#loop through each COP
for(i in 1:length(cop.list)){

#Get total true groups for COP1
parties_true<-part_stats$Parties[part_stats$Meeting==cop.list[i]]
observers_true<-part_stats$Observer[part_stats$Meeting==cop.list[i]]
un_true<-part_stats$UN[part_stats$Meeting==cop.list[i]]
igo_true<-part_stats$IGO[part_stats$Meeting==cop.list[i]]
ngo_true<-part_stats$NGO[part_stats$Meeting==cop.list[i]]
total_true<-parties_true+observers_true+un_true+igo_true+ngo_true

#get total groups for coded COP1
parties_coder<-length(unique(cops$Delegation[(cops$Group_Type=="Parties" & cops$Meeting==cop.list[i])]))
observers_coder<-length(unique(cops$Delegation[(cops$Group_Type=="Observer States" & cops$Meeting==cop.list[i])]))
un_coder<-length(unique(cops$Delegation[((cops$Group_Type=="Specialized agencies and related organizations" | cops$Group_Type=="United Nations Secretariat units and bodies")  & cops$Meeting==cop.list[i])]))
igo_coder<-length(unique(cops$Delegation[(cops$Group_Type=="Intergovernmental organizations" & cops$Meeting==cop.list[i])]))
ngo_coder<-length(unique(cops$Delegation[(cops$Group_Type=="Non-governmental organizations" & cops$Meeting==cop.list[i])]))
total_coder<-parties_coder+observers_coder+un_coder+igo_coder+ngo_coder

#build comparison table 1
group_table_temp<-cbind(cop.list[i],parties_true,parties_coder,observers_true,observers_coder,un_true,un_coder,igo_true,igo_coder,ngo_true,ngo_coder,total_true,total_coder)

#combine with past COPs
group_table<-rbind(group_table,group_table_temp)

}
colnames(group_table)<-c("COP","True Party","Coded Parties","True Observers","Coded Observers","True UN","Coded UN","True IGO","Coded IGO","True NGO","Coded NGO","True Total","Coded True")

#save table
write.csv(group_table,"Group_Level_Comparison.csv",row.names=FALSE)

####################################################################
################Participant Count Comparisons#######################
####################################################################
#set up blank storage object
participant_table<-NULL

#loop through each COP
for(i in 1:length(cop.list)){

parties_true<-part_stats$Party_Participants[part_stats$Meeting==cop.list[i]]
observers_true<-part_stats$Observer_Participants[part_stats$Meeting==cop.list[i]]
un_true<-part_stats$UN_Participants[part_stats$Meeting==cop.list[i]]
igo_true<-part_stats$IGO_Participants[part_stats$Meeting==cop.list[i]]
ngo_true<-part_stats$NGO_Participants[part_stats$Meeting==cop.list[i]]
participant_true<-parties_true+observers_true+un_true+igo_true+ngo_true

#get total groups for coded COP1
parties_coder<-nrow(cops[(cops$Group_Type=="Parties" & cops$Meeting==cop.list[i]),])
observers_coder<-nrow(cops[(cops$Group_Type=="Observer States" & cops$Meeting==cop.list[i]),])
un_coder<-nrow(cops[((cops$Group_Type=="Specialized agencies and related organizations" | cops$Group_Type=="United Nations Secretariat units and bodies")  & cops$Meeting==cop.list[i]),])
igo_coder<-nrow(cops[(cops$Group_Type=="Intergovernmental organizations" & cops$Meeting==cop.list[i]),])
ngo_coder<-nrow(cops[(cops$Group_Type=="Non-governmental organizations" & cops$Meeting==cop.list[i]),])
participant_coder<-parties_coder+observers_coder+un_coder+igo_coder+ngo_coder

#build comparison table 1
participant_table_temp<-cbind(cop.list[i],parties_true,parties_coder,observers_true,observers_coder,un_true,un_coder,igo_true,igo_coder,ngo_true,ngo_coder,participant_true,participant_coder)

#combine with past COPs
participant_table<-rbind(participant_table,participant_table_temp)
}

colnames(participant_table)<-c("COP","True Party","Coded Parties","True Observers","Coded Observers","True UN","Coded UN","True IGO","Coded IGO","True NGO","Coded NGO","True Total","Coded True")

#save table
write.csv(participant_table,"Participant_Level_Comparison.csv",row.names=FALSE)



  