#rebuilt example

library(tidyverse)
library(tidytext)
library(caret)
library(textdata)

#we went ahead and got 1k tweets about star trek and billie eilish

trek2<-trek%>%
  select(text, created_at, is_retweet)%>%
  mutate(category="trek")

billie2<-billie%>%
  select(text, created_at, is_retweet)%>%
  mutate(category="billie")

mldata<-bind_rows(trek2, billie2)
#for refernce, lets give all these an ID number
mldata<-mutate(mldata, id=1:2000)
mldata<-data.frame(mldata)

#now a test using a histogram as the original did
library(ggplot2)

ggplot(mldata, aes(category))+geom_bar()
ggplot(mldata, aes(id, category))+geom_jitter()


#get some stopwords
library(tm)
stoppy<-stopwords()
stoppy2<-data.frame(word=stoppy)

#now lets do some regular old text cleaning stuff
library(tidytext)
m2<-mldata%>%
  unnest_tokens(word, text)%>%
  anti_join(stoppy2)%>%
  count(id, word, sort = TRUE)

categorypass<-mldata%>%
  select(id, category)


m2<-left_join(m2, categorypass)

words_10 <- m2 %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 10) %>%
  select(word)


data_dtm <- m2 %>%
  right_join(words_10, by = "word") %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)


meta <- tibble(id = as.numeric(dimnames(data_dtm)[[1]])) %>%
  left_join(m2[!duplicated(m2$id), ], by = "id")

set.seed(1234)
trainIndex <- createDataPartition(meta$category, p = 0.8, list = FALSE, times = 1)

data_df_train <- data_dtm[trainIndex, ] %>% as.matrix() %>% as.data.frame()
data_df_test <- data_dtm[-trainIndex, ] %>% as.matrix() %>% as.data.frame()

response_train <- meta$category[trainIndex]

#missing tweets


#now modeling stuff
trctrl <- trainControl(method = "none")


svm_mod <- train(x = data_df_train,
                 y = as.factor(response_train),
                 method = "svmLinearWeights2",
                 trControl = trctrl,
                 tuneGrid = data.frame(cost = 1, 
                                       Loss = 0, 
                                       weight = 1))

svm_pred <- predict(svm_mod,
                    newdata = data_df_test)

svm_cm <- confusionMatrix(svm_pred, as.factor(meta[-trainIndex, ]$category))
svm_cm
