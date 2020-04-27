#develop the new model based on corset
library(dplyr)
head(coreset)
warrenhill<-coreset%>%
  mutate(outcome = as.factor(outcome))%>%
  mutate(team1ft = (FTA.x*FT..x)--(mean(FTA.x*FT..x)/sd(FTA.x*FT..x)))%>%
  mutate(team2ft = (FTA.y*FT..y)-(mean(FTA.y*FT..y)/sd(FTA.y*FT..y)))%>%
  mutate(team1differential = ((Tm..x - mean(Opp..x))/sd(Tm..x)))%>%
  mutate(team2differential = ((Tm..y - mean(Opp..y))/sd(Tm..y)))%>%
  mutate(team13pt = ((X3PA.x * X3P..x)-(mean(X3PA.x) * mean(X3P..x)))/sd(X3PA.x * X3P..x))%>%
  mutate(team23pt = ((X3PA.y * X3P..y)-(mean(X3PA.y) * mean(X3P..y)))/sd(X3PA.y * X3P..y))%>%
  mutate(team1sos = SOS.x-mean(SOS.x)/sd(SOS.x))%>%
  mutate(team2sos = SOS.y-mean(SOS.y)/sd(SOS.y))%>%
  mutate(team1rb = (TRB.x-mean(TRB.x))/sd(TRB.x))%>%
  mutate(team2rb = (TRB.y-mean(TRB.y))/sd(TRB.y))%>%
  mutate(team1to = (TOV.x-mean(TRB.x))/sd(TRB.x))%>%
  mutate(team2to = (TOV.y-mean(TOV.y))/sd(TOV.y))
  


najee<-warrenhill%>%
  select(team1, team2, Result, outcome, Year.x, team1sos, team2sos, team1differential, team2differential, team1ft, team2ft,
         team13pt, team23pt, team1rb, team2rb, team1to, team2to, W.L..x, W.L..y)


#split the data into development and validation
library(rsample)
data_split <- najee %>%
  initial_split(prop = .8)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)


library(textrecipes)
#our final recipe






#this is our four factors special blend
rec <- recipe(outcome ~ team1sos + team2sos + team1differential + team2differential + team1ft + team2ft + team13pt + team23pt + team1rb + team2rb + team1to + team2to + W.L..x + W.L..y, data = training_data) %>%
  #three deep ngrams
  prep()  


#processing step
train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

library(parsnip)
#critial analytic 80/20 split, accuracy module
Combi2<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth=val_data$outcome)


library(ggplot2)
ggplot(Combi2, aes(.pred_class, truth))+geom_jitter()

#to validate the validation, we go with the brilliant artist behind Sued, Marion Meadows
marionmeadows<-data.frame(Combi2,validation_data, accurate = as.numeric(Combi2$truth)-as.numeric(Combi2$.pred_class))

#get a sense of what is up
marionmeadows%>%
  count(accurate)

View(marionmeadows)


#at each import cycle go ahead and rebake the new
new_data<-bake(rec, new_data=albright)
#predictor
Combi2<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = new_data)

resultX<-data.frame(Combi2, albright)
View(resultX)








#ultrasimple no basketball data
rec <- recipe(outcome ~ team1sos + team2sos + W.L..x + W.L..y, data = training_data) %>%
  #three deep ngrams
  prep()  


#processing step
train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

library(parsnip)
#critial analytic 80/20 split, accuracy module
Combi2<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth=val_data$outcome)


library(ggplot2)
ggplot(Combi2, aes(.pred_class, truth))+geom_jitter()

#to validate the validation, we go with the brilliant artist behind Sued, Marion Meadows
marionmeadows<-data.frame(Combi2,validation_data, accurate = as.numeric(Combi2$truth)-as.numeric(Combi2$.pred_class))

#get a sense of what is up
marionmeadows%>%
  count(accurate)

View(marionmeadows)


#at each import cycle go ahead and rebake the new
new_data<-bake(rec, new_data=albright)
#predictor
Combi2<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = new_data)

resultX<-data.frame(Combi2, albright)
View(resultX)





#Big and raw
#reimported corset with outcome as a factor
data_split <- coreset %>%
  initial_split(prop = .8)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

#processing step
train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)


rec <- recipe(outcome ~  SOS.y + SOS.x + TRB.x + TRB.y + Tm..x + Opp..x +Tm..y + Opp..y + TOV.x +TOV.y + L.x + L.y, data = training_data) %>%
  #three deep ngrams
  prep()  


#processing step
train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

library(parsnip)
#critial analytic 80/20 split, accuracy module
Combi2<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth=val_data$outcome)


library(ggplot2)
ggplot(Combi2, aes(.pred_class, truth))+geom_jitter()

#to validate the validation, we go with the brilliant artist behind Sued, Marion Meadows
marionmeadows<-data.frame(Combi2,validation_data, accurate = as.numeric(Combi2$truth)-as.numeric(Combi2$.pred_class))

#get a sense of what is up
marionmeadows%>%
  count(accurate)

View(marionmeadows)


#at each import cycle go ahead and rebake the new
#notice that the new data is just pure boneyjames
new_data<-bake(rec, new_data=boneyjames)
#predictor
Combi2<-rand_forest("classification", trees=10000) %>%
  set_engine("randomForest") %>%
  fit(outcome ~ ., data = test_data) %>%
  predict(new_data = new_data)

resultX<-data.frame(Combi2, boneyjames)
View(resultX)

                                               
                                                    
                                                    
