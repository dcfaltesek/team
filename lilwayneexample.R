#this will get us the data we need
get_discography("Lil Wayne")

S<-.Last.value

S$lyrics
library(reshape2)
S2<-unnest(S, lyrics)
View(S2)

#this function will get us album level lyrics from a thing we called on spotifyr
song_level_lyrics_spotify<-function(x){
  Z<-unite(x, identifier, c(track_n, line, duration_ms))
  ZZ<-select(Z, c(identifier, lyric, album_name, track_name))
  Q<-spread(ZZ, identifier, lyric)
  unite(Q, song_lyric, -c(track_name, album_name), na.rm=TRUE)
}
S3<-song_level_lyrics_spotify(S2)
View(S3)

library(rsample)
data_split <- S3 %>%
  initial_split(prop = .8, strata = album_name)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec <- recipe(album_name ~ song_lyric, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 1)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 500) %>%
  #tfidf method
  step_tfidf(song_lyric) %>%
  step_upsample(album_name) %>%
  prep() 

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

#randomforest is called from parsnip
library(parsnip)
#accuracy is from forecast
library(forecast)
is4<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(album_name ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$album_name)

library(ggplot2)
ggplot(is4, aes(truth, .pred_class))+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))



#this will get us the data we need
RH<-get_discography("Red Hot Chili Peppers")

library(reshape2)
RH2<-unnest(RH, lyrics)


#this function will get us album level lyrics from a thing we called on spotifyr
song_level_lyrics_spotify<-function(x){
  Z<-unite(x, identifier, c(track_n, line, duration_ms))
  ZZ<-select(Z, c(identifier, lyric, album_name, track_name))
  Q<-spread(ZZ, identifier, lyric)
  unite(Q, song_lyric, -c(track_name, album_name), na.rm=TRUE)
}
RH3<-song_level_lyrics_spotify(RH2)


library(rsample)
data_split <- RH3 %>%
  initial_split(prop = .6, strata = album_name)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec <- recipe(album_name ~ song_lyric, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 2)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 200) %>%
  #tfidf method
  step_tfidf(song_lyric) %>%
  step_upsample(album_name) %>%
  prep() 

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)

#randomforest is called from parsnip
library(parsnip)
#accuracy is from forecast
library(forecast)
is5<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(album_name ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$album_name)

library(ggplot2)
ggplot(is5, aes(truth, .pred_class))+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))

#putting it all together...
Combi<-bind_rows(S3, RH3)

data_split <- Combi %>%
  initial_split(prop = .6, strata = album_name)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec <- recipe(album_name ~ song_lyric, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 2)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 200) %>%
  #tfidf method
  step_tfidf(song_lyric) %>%
  step_upsample(album_name) %>%
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
  fit(album_name ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$album_name)

library(ggplot2)
ggplot(Combi2, aes(truth, .pred_class))+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))

View(Combi2)
filter(Combi2, truth!=.pred_class)
library(dplyr)
wayne_albums<-levels(as.factor(S2$album_name))
red_albums<-levels(as.factor(RH2$album_name))

wayne_albums<-data.frame(wayne_albums, "name"="Wayne")
red_albums<-data.frame(red_albums, "name"="Red Hot")
colnames(red_albums)[1]<-"truth"
colnames(wayne_albums)[1]<-"truth"
zorn<-bind_rows(wayne_albums, red_albums)
wayne_albums2<-data.frame(wayne_albums, "name"="Wayne")
red_albums2<-data.frame(red_albums, "name"="Red Hot")
colnames(red_albums2)[1]<-".pred_class"
colnames(wayne_albums2)[1]<-".pred_class"
zlurm<-bind_rows(red_albums2, wayne_albums2)

zort<-inner_join(Combi2, zlurm, by=".pred_class")
ggplot(zort, aes(.pred_class, truth, colour=name.1))+geom_jitter()+facet_wrap(~name)


