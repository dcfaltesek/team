#NCAA basketball 2

#import the data
bigbasketball <- read.csv("~/Desktop/bigbasketball.csv", stringsAsFactors=FALSE)
View(bigbasketball)
`#clean the names
#detect if the team was in the touranment, store as factor

library(stringr)
bb_T<-str_detect(bigbasketball$School, "NCAA")
T_factor<-as.factor(bb_T)

#clean the touranemnt indicator from the names
clean_school<-str_replace_all(bigbasketball$School, "NCAA", "")%>%
  str_trim(side=c("both"))

#create a new datafram with all the orignal data, tourney factor, win factor, and cleam names
clean_basketball<-data.frame(bigbasketball, T_factor, Z=as.factor(bigbasketball$W),clean_school, RBRT=bigbasketball$TRB/bigbasketball$G, TOVRT=bigbasketball$TOV/bigbasketball$G, FTRT=bigbasketball$FTA/bigbasketball$G, ASSRT=bigbasketball$AST/bigbasketball$G,
                             four_factor_shooting=(((bigbasketball$FG.*2)+(bigbasketball$X3P.*3)+(bigbasketball$FT.))*.4), four_factor_tov=((bigbasketball$TOV/bigbasketball$G)*.25), four_factor_rebound=((bigbasketball$TRB/bigbasketball$G)*.2), 
                             four_factor_free=(((FTRT=bigbasketball$FTA/bigbasketball$G)*bigbasketball$FT.)*.15))
                          
#remove the last row in baseR
cleaner_basketball<-clean_basketball[1:(dim(clean_basketball)-1),]

View(cleaner_basketball)
#add conference data
conferences <- read.csv("~/conferences.csv")
library(dplyr)
clean_w_conf<-inner_join(cleaner_basketball, conferences, by ="clean_school")

library(rsample)
data_split <- clean_w_conf %>%
  initial_split(prop = .8)

#break into two sets
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
#recipe for this is a combination of simple rating syste, strength of schedule
#outcome is WINS, predictors are shot percentage, three point percentage, free throws, and turnovers
rec <- recipe(Z ~ SRS + AST + SOS +TOV, data = training_data) %>%
  prep() 

train_data<-juice(rec)
val_data <- bake(rec, new_data = validation_data)

library(parsnip)
#run the random forest
is5<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(Z ~ ., data = train_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth = val_data$Z)

library(ggplot2)
#now plot that
ggplot(is5, aes(as.numeric(.pred_class), as.numeric(truth)), colour=Conference)+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))

#cor coefficient between the factor levels 
cor.test(as.numeric(is5$.pred_class), as.numeric(is5$truth))

#rerun the model with all the data
is6<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(Z ~ ., data = train_data) %>%
  predict(new_data = clean_w_conf)%>%
  mutate(truth = clean_w_conf$Z)

#combine complete predictions with the original data
totality<-bind_cols(clean_w_conf, is6)
ggplot(totality, aes(as.numeric(.pred_class), as.numeric(truth)), colour=Conference)+geom_text(label=clean_w_conf$clean_school)+theme_classic()+theme(axis.text.x = element_text(angle = 45))
cor.test(as.numeric(totality$truth), as.numeric(totality$.pred_class))

View(totality)
summaries<-totality%>%
  mutate(differential=as.numeric(.pred_class)-as.numeric(truth))%>%
  group_by(Conference)%>%
  summarize(mean(differential), sd(differential))

View(summaries)



#DEAN OLIVERS MODEL
rec <- recipe(Z ~ four_factor_shooting + four_factor_tov + four_factor_rebound + four_factor_free, data = training_data) %>%
  prep() 

train_data<-juice(rec)
val_data <- bake(rec, new_data = validation_data)

library(parsnip)
#run the random forest
is5<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(Z ~ ., data = train_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth = val_data$Z)


is5<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(Z ~ ., data = train_data) %>%
  predict(new_data = val_data)%>%
  mutate(truth = val_data$Z)

ggplot(is5, aes(as.numeric(.pred_class), as.numeric(truth)), colour=Conference)+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))

#cor coefficient between the factor levels 
cor.test(as.numeric(is5$.pred_class), as.numeric(is5$truth))






#now for naive bayes

