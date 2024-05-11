

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
setwd("C:/Users/bagozzib/Desktop/applications/")  


################################################################
#######################Read In Data#############################
################################################################

#read in cleaned data
complete.data<-read.csv("C:/Users/bagozzib/Desktop/applications/cops.cleaned.translated.csv",header=TRUE, row.names=NULL)

#subset
complete.data<-subset(complete.data,!is.na(complete.data$Female))
complete.data<-subset(complete.data,complete.data$Year>=2021)

#format variables
complete.data$gender<-ifelse(complete.data$Female==1,"Female","Male")

#Apply corpus() to our object, but need to also tell it which column in this object is text
docs <- corpus(complete.data, text_field = 'Job_Title')
docs
summary(docs)


############################
####Wordcloud by Gender#####
############################

#Use forward pipes to:
#1. Subset our corpus to only speeches with female/male gender
#2. Converts to a DTM and removes stopwords and punctuation 
#3. Retain only terms that appear at least 15 times in the remaining corpus
dfm_gw <- corpus_subset(docs, !is.na(Female)) %>% 
    dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
    dfm_trim(min_termfreq = 15, verbose = FALSE)

#The above commands give us some warnings, mainly becuase I'm skipping over some intermediate steps for now


#####################################################
####Comparison Wordcloud For Multiple Genders#####
#####################################################

#Extend this approach to compare multiple presidents' wordclouds,
#now rolling together our data processing steps and our plotting steps all into a single
#chained command via forward pipes. This single command does:
#1. Subsets our docs to only include speeches from GW, Jefferson, and James Madison,
#2. Removes punctuation
#3. Removes English stopwords
#4. Converts to a DTM
#5. Creates three separate subets of our DTM (for each president)
#6. Retains only terms appearing at least five times in each set
#7. Plots the word cloud...whew!
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


##########################################################################
#################################Keyness##################################
##########################################################################

# compare Trump speeches to other Presidents by chi^2
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
textplot_keyness(tstat1, margin = 0.2, n = 10)

#The End...
