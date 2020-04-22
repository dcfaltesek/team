#with simons clean and super-sensible data (GOOD JORB!)
library(dplyr)
library(rsample)

fugees<-blackstreet_fixed%>%
  mutate(T1FGA = T1FreeThrowAttpt/T1FieldGoalAttpt*T1FreeThrowPct)%>%
  mutate(T2FGA = T2FreeThrowAttpt/T2FieldGoalAttpt*T2FreeThrowPct)%>%
  mutate(T1R3G = T1FieldGoalAttpt3P/T1FieldGoalAttpt2P*T1FieldGoals3PPct)%>%
  mutate(T2R3G = T2FieldGoalAttpt3P/T2FieldGoalAttpt2P*T2FieldGoals3PPct)%>%
  mutate(T1Z= T1PointsScored/mean(T1PointsScored))%>%
  mutate(T2Z= T2PointsScored/mean(T2PointsScored))%>%
  mutate(scorebalance=T1Z-T2Z)

 
firstround<-InputData...Round.of.64
predict20A<-firstround%>%
  mutate(T1FGA = T1FreeThrowAttpt/T1FieldGoalAttpt*T1FreeThrowPct)%>%
  mutate(T2FGA = T2FreeThrowAttpt/T2FieldGoalAttpt*T2FreeThrowPct)%>%
  mutate(T1R3G = T1FieldGoalAttpt3P/T1FieldGoalAttpt2P*T1FieldGoals3PPct)%>%
  mutate(T2R3G = T2FieldGoalAttpt3P/T2FieldGoalAttpt2P*T2FieldGoals3PPct)%>%
  mutate(T1Z= T1PointsScored/mean(T1PointsScored))%>%
  mutate(T2Z= T2PointsScored/mean(T2PointsScored))%>%
  mutate(scorebalance=T1Z-T2Z)

#split the data into development and validation
data_split <- fugees %>%
  initial_split(prop = .8)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)


library(textrecipes)
#our final recipe
rec <- recipe(outcome ~ T1WinLossRatio + T2WinLossRatio + T1FGA + T2FGA + T1R3G + T2R3G + T1Z + T2Z + scorebalance, data = training_data) %>%
  #three deep ngrams
  prep() 


train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)
all_data<-bake(rec, new_data=fugees)
one_off<-bake(rec,new_data = predict20A)

library(parsnip)
#critial analytic 80/20 split, accuracy module
Combi2<-rand_forest("classification", trees=5000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth=val_data$outcome)

library(ggplot2)
#
ggplot(Combi2, aes(.pred_class, truth))+geom_jitter()

BackstreetBoyz<-data.frame(Combi2, validation_data)

NSYNC<-BackstreetBoyz%>%
  mutate(accurate = as.numeric(.pred_class)-as.numeric(truth))

minus<-NSYNC%>%
  filter(accurate==-1)%>%
  sample_frac(.25)
View(minus)


plus<-NSYNC%>%
  filter(accurate==1)%>%
  sample_frac(.25)
View(plus)

#pretty plot of the validation phase
ggplot(NSYNC, aes(.pred_class, truth, colour=accurate))+geom_jitter()+ggtitle("Predictions vs Truths")+labs(x="predictions", y="actual results")

NSYNC%>%
  count(accurate)

#some fun plots
ggplot(firstround, aes(T1GamesPlayed,T1PointsScored))+geom_text_repel(aes(label=Team1))
ggplot(firstround, aes(T2GamesPlayed,T2PointsScored))+geom_text_repel(aes(label=Team2))

write.csv(NSYNC, "NSYNC.csv", row.names = FALSE)

#this code makes new predictions
one_off<-bake(rec,new_data = predict20A)

Combi3<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = train_data) %>%
  predict(new_data = one_off)

X<-data.frame(Combi3, predict20A)
View(X)
