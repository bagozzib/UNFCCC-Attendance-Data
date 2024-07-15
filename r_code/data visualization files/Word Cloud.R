#This code makes a Wordcloud by Male vs Female for 2021 onward

##############
####Set Up####
##############

#clear memory
rm(list=ls())

#Load some necessary packages (required during each R-session)
library(tm)
library(wordcloud)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tidyverse)

#set seed
set.seed(50)

#set working directory
setwd("C:/Users/bagoz/OneDrive/Desktop/COPsNew/")  


################################################################
#######################Read In Data#############################
################################################################

#read in cleaned data
complete.data<-read.csv("cops.cleaned.translated.csv",header=TRUE, row.names=NULL,fileEncoding = "UTF-8")

#subset
complete.data<-subset(complete.data,!is.na(complete.data$Female))
complete.data<-subset(complete.data,complete.data$Year>=2021)

#format variables
complete.data$gender<-ifelse(complete.data$Female==1,"Female","Male")

#Apply corpus()
docs <- corpus(complete.data, text_field = 'Job_Title')
docs
summary(docs)


############################
####Wordcloud by Gender#####
############################

#1. Subset corpus to only cases with non NA on Female
#2. Convert to a DTM and remove stopwords and punctuation 
#3. Retain only terms that appear at least 15 times in the remaining corpus
dfm_gw <- corpus_subset(docs, !is.na(Female)) %>% 
    dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 15, verbose = FALSE)

#####################################################
####Comparison Wordcloud For Multiple Genders#####
#####################################################

#Make Wordcloud
png("comparison.png", units="in", width=5, height=5, res=300)
corpus_subset(docs, 
              gender %in% c("Male", "Female")) %>%
    tokens(remove_punct = TRUE) %>%
    tokens_remove(stopwords("english")) %>%
    tokens_remove(stopwords("spanish")) %>%
    tokens_remove(stopwords("french")) %>%
    dfm() %>%
    dfm_group(groups = gender) %>%
    dfm_trim(min_termfreq = 5, verbose = FALSE) %>%
    textplot_wordcloud(comparison = TRUE,color = c("orange","forestgreen"))
dev.off()

#The end
