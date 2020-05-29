#load some libraries
library(tidytext)
library(textfeatures)
library(dplyr)
library(ggplot2)

#adds paragraph numbers and speech names
speech_meta<-function(X,Y){
  Q<-mutate(X, "paragraph"=1:dim(X)[1])
  R<-mutate(Q, "speech"=Y)
}