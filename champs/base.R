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
View(bieber)


#so this should combine the two datasets
rhi<-bind_cols(jayz, bieber)

View(rhi)

#select out what we actually need
results%>%
  select(Schl, Opp)


#Simon here, I wrote some code to get winners and losers sorted. I don't know if I did it correctly, but I put it below


#Determine if the team in the Schl column won or lost

bb_D<-str_detect(rhi$Result., "W")
D_factor<-as.factor(bb_D)

#Make column a data frame

D.frame <- as.data.frame(D_factor)

View(D.frame)

#create a shared column between the true/false data frame and rhi

D.frame2 <- cbind(D.frame, "GmNumber"=1:nrow(D.frame)) 

View(D.frame2)

rhi3<-rhi

View(rhi3)

#bind the data frames by the reference column 

WonGames <-bind_cols(data.frame(D.frame2), data.frame(rhi3))

View(WonGames)

#Swapping Schl and Opp column data if the team on the right won, so all winners are on the left and all losers are on the right

TransGames <- transform(WonGames, Schl = ifelse(D_factor == 'FALSE', Opp, Schl), Opp = ifelse(D_factor == 'FALSE', Schl, Opp))

View(TransGames)

#select out school, opp, T_factor, and D_factor

Results <- select(TransGames, Schl, Opp, D_factor, T_factor)

View(Results)

#This gives a data frame with winners, losers, and if the winner was the expected team to win

colnames(Results)[1]<-"Winner" 
colnames(Results)[2]<-"Loser"
colnames(Results)[3]<-"Winner Expected"










