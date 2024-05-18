#
#This R-script make a keyness plot by male vs female
#

##############
####Set Up####
##############

#clear memory
rm(list=ls())

#Load some necessary packages
library(tm)
library(wordcloud)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(tidyverse)

#set seed
set.seed(50)

#set working directory
setwd(".../applications/")  


################################################################
#######################Read In Data#############################
################################################################

#read in cleaned data
complete.data<-read.csv("cops.cleaned.translated.csv",header=TRUE, row.names=NULL,encoding="latin1")

#subset to only male/female respondents and to only COPs taking place in 2021 or later
complete.data<-subset(complete.data,!is.na(complete.data$Female))
complete.data<-subset(complete.data,complete.data$Year>=2021)

#format gender variable
complete.data$gender<-ifelse(complete.data$Female==1,"Female","Male")

#Apply corpus() to our object
docs <- corpus(complete.data, text_field = 'Job_Title')


##########################################################################
#################################Keyness##################################
##########################################################################

# compare job titles across genders by chi^2
dfmat1 <- corpus_subset(docs, 
              gender %in% c("Male", "Female")) %>%
    tokens(remove_punct = TRUE) %>%
    tokens_remove(stopwords("english")) %>%
    tokens_remove(stopwords("spanish")) %>%
    tokens_remove(stopwords("french")) %>%
    dfm() %>%
    dfm_group(groups = gender) %>%
    dfm_trim(min_termfreq = 5, verbose = FALSE)
tstat1 <- textstat_keyness(dfmat1, target = "Female")

#make plot
textplot_keyness(tstat1, margin = 0.2, n = 10)

#The End
