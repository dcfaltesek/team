#a clean file for composing our code...

#first we need to get the games really clean
library(lubridate)
library(dplyr)
library(stringr)
year<-mdy(haley_games$Date)

results<-select(haley_games, c(Schl,Opp,Result))%>%
  mutate(year=year(year))%>%
  mutate(outcome=str_count(Result, "W")) 
  
View(results)

A1<-results%>%
  filter(year==2019)
A2<-kobe%>%
  filter(Year==19)
A1<-A1%>%
  mutate(Year = year -2000)
flurn<-inner_join(A1,A2, by = "Schl")
colnames(flurn)[1]<-"team1"
colnames(flurn)[2]<-"Schl"
result19<-inner_join(flurn, A2, by = "Schl")

blackstreet<-bind_rows(result11, result12, result13, result14, result15, result16, result17, result18, result19)

View(blackstreet)

write.csv(blackstreet, "blackstreet.csv", row.names = FALSE)

library(rsample)

#START HERE!

blackstreet2<-data.frame(blackstreet, outcome=as.factor(blackstreet$outcome))
data_split <- blackstreet2 %>%
  initial_split(prop = .8)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)


library(textrecipes)
rec <- recipe(outcome.1 ~ FT.x + FT.y, data = training_data) %>%
  #three deep ngrams
  prep() 

library(parsnip)
train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

#randomforest is called from parsnip
library(parsnip)
#accuracy is from forecast
library(forecast)
Combi2<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(outcome.1 ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$outcome.1)

library(ggplot2)
ggplot(Combi2, aes(.pred_class, truth))+geom_jitter()


#BAKE the new games joined with the same data


Combi3<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(outcome.1 ~ ., data = train_data) %>%
#put the new stuff in here
  predict(new_data = NEWSTUFF) 










