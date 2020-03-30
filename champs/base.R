#quality libraries
library(tidyr)
library(stringr)
library(dplyr)
library(rsample)
library(textrecipes)
library(parsnip)
library(ggplot2)

#so in the base directory I loaded a really old tournment
#these should get the datasets from your local github source
`results` <- read.csv("~/Documents/GitHub/team/champs/2011NCAA.csv", stringsAsFactors=FALSE)
`teams` <- read.csv("~/Documents/GitHub/team/champs/2011NCAATEAMS.csv", stringsAsFactors=FALSE)

#notice that I stored these in really nice names

View(results) 
View(teams)

#first list of tasks to do..
#1. clean the results from the games
#2. clean the team names in the teams so that NCAA is used to indicate that they made the touranment
#3. structure the results so that the winning team is ALWAYS on the left and the loser is on the right
#4. more advanced things to think about, how to we model each game, do we run a model on who wins and loses?

#Simon here. This code creates T_factor which is a true/false list depending on if NCAA was in a schools name and cleans NCAA from the team names

bb_T<-str_detect(`teams`$School, "NCAA")
T_factor<-as.factor(bb_T)

View(T_factor)

#clean the touranemnt indicator from the names
clean_school<-str_replace_all(`teams`$School, "NCAA", "")%>%
  str_trim(side=c("both"))

View(clean_school)

#This is a data frame with cleaned school names and the T-factor showing if they got in the tournament or not

Usher<-bind_cols(data.frame(T_factor), data.frame(clean_school))
View(Usher)

bey<-bind_cols(Usher, teams)
View(bey)

colnames(bey)[2]<-"Schl" 

jayz<-left_join(results, bey)



#filter for that which is not blank
drake<-jayz%>%
  filter(GmNumber!="")

dim(drake)
dim(results)

#now to make the ID column work again
colnames(bey)[2]<-"Opp"

bieber<-left_join(results, bey, by="Opp")
dim(bieber)

#so this should combine the two datasets
rhi<-bind_cols(jayz, bieber)












