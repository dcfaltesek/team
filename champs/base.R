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
`results` <- read.csv("~/Documents/GitHub/team/champs/results_2.csv", stringsAsFactors=FALSE)
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
colnames(bey)[2]<-"Schl"
solange<-bey
colnames(solange)[2]<-"Opp"
#bey is all the team data

#what do we see
anti_join(results, bey)
anti_join(results, solange)

results$Schl
results$Opp


yachty<-inner_join(results, solange)
wayne<-inner_join(results, bey)

wayne2<-wayne%>%
  select(c(Schl, X, Result, Rk, SOS, TOV_pct, FT.FGA, OP_3PAr))

yachty2<-yachty%>%
  select(c(Opp, X, Result, Rk, SOS, TOV_pct, FT.FGA, OP_3PAr))

fugees<-full_join(wayne2, yachty2, by="X")

fugees<-fugees%>%
  filter(Rk.x != "NA")%>%
  filter(Rk.y != "NA")

colnames(fugees)[3]<-"result"

bb_D<-data.frame(fugees$X, binary_result=as.factor(str_detect(fugees$Result, "W")))
neptunes<-bind_cols(fugees, bb_D)

data_split <- neptunes %>%
  initial_split(prop = .5)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)


library(textrecipes)
rec <- recipe(binary_result ~ TOV_pct.x + TOV_pct.y + FT.FGA.x + FT.FGA.y + Rk.x + Rk.y, data = training_data) %>%
  #three deep ngrams
  prep() 

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

#randomforest is called from parsnip
library(parsnip)
#accuracy is from forecast
library(forecast)
Combi2<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(binary_result ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$binary_result)

ggplot(Combi2, aes(.pred_class, truth))+geom_jitter()


Combi3<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(binary_result ~ ., data = train_data) %>%
  predict(new_data = neptunes) %>%
  mutate(truth = neptunes$binary_result)

ggplot(Combi3, aes(.pred_class, truth))+geom_jitter()

plurnt<-bind_cols(Combi3, neptunes)
View(plurnt)

