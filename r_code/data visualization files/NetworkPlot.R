

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
library(dplyr)
library(ggraph)
library(igraph)
library(reshape2)

#set seed
set.seed(50)


#set working directory
setwd("C:/Users/bagozzib/Desktop/applications/") 

################################################################
#######################Read In Data#############################
################################################################

#read in cleaned data
cops.data<-read.csv("C:/Users/bagozzib/Desktop/applications/cops.cleaned.translated.csv",header=TRUE, row.names=NULL)
precops.data<-read.csv("C:/Users/bagozzib/Desktop/applications/precops.cleaned.translated.csv",header=TRUE, row.names=NULL)

#combine
complete.data<-rbind(cops.data,precops.data)

#subset to just countries
complete.data<-subset(complete.data,(complete.data$Party==1 | complete.data$Observer==1))
complete.data<-subset(complete.data,(!is.na(complete.data$Affiliation)))

#group affiliations by country
complete.data<-complete.data %>%
  group_by(Delegation) %>%
  summarize(Affiliation = paste(Affiliation, collapse = ", "))
  
#convert to corpus
divcorp<- corpus(complete.data, text_field = 'Affiliation')

#convert to DFM, removing punctuation and stopwords
dfmdiv<-divcorp %>%
    tokens(remove_punct=TRUE) %>%
    tokens_remove(stopwords("english")) %>%
    tokens_remove(stopwords("spanish")) %>%
    tokens_remove(stopwords("french")) %>%
    dfm() 

#get similarity above a chosen threshold
tstatdiv <- textstat_simil(dfmdiv, method = "cosine", margin = "documents")

#convert to matrix
tstatdiv<-as.matrix(tstatdiv)
tstatdiv[is.nan(tstatdiv)] <-0
tstatdiv[(tstatdiv<.75)] <-0
tstatdiv[(tstatdiv>=.75)] <-1
tstatdiv<-as.matrix(tstatdiv)
rownames(tstatdiv)<-complete.data$Delegation
colnames(tstatdiv)<-complete.data$Delegation
divadj <- graph.adjacency(tstatdiv, mode="undirected", weighted=NULL,diag=F)

set.seed(5)
plot(delete.vertices(simplify(divadj), degree(divadj)==0),rescale=FALSE, , ylim=c(-1,3.75),xlim=c(-7,7), vertex.shape = "none",edge.color="darkgreen",vertex.label.cex =.75) 
