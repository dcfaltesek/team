#this is an example of how to sucessfully pull a qualifier

#access data source, lyrics genius
#besure to use your spotify credentials
library(spotifyr)
LW<-get_discography("Lil Wayne")

#prepare the lyric data

#notice that the lyrics are here are nested


#here are a few libraries that can do the job
library(tidyr)
library(dplyr)
#tgo on ahead and unnest those
LW2<-unnest(LW, lyrics, album_images)


#this function will be very nice 
song_level_lyrics_spotify<-function(x){
  Z<-unite(x, identifier, c(track_n, line, duration_ms))
  ZZ<-select(Z, c(identifier, lyric, album_name, track_name))
  Q<-spread(ZZ, identifier, lyric)
  unite(Q, song_lyric, -c(track_name, album_name), na.rm=TRUE)
}

TSX<-spotify_other_information(TS)
RHX<-spotify_other_information(RH)
LWX<-spotify_other_information(LW)
VPX<-spotify_other_information(VP)
KWX<-spotify_other_information(KW)
UX<-spotify_other_information(U)

staind<-bind_rows(TSX, RHX, LWX, VPX, KWX, UX)
flurn<-inner_join(withusher, staind)


View()

#now that we have our full lyrics, lets break this up for sampling
library(rsample)
data_split <- flurn %>%
  initial_split(prop = .5, strata = album_name)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec <- recipe(name ~ song_lyric + tempo + key+ speechiness + liveness, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 2)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 500) %>%
  #tfidf method
  #here is the key to actually locking on
  step_tfidf(song_lyric) %>%
  step_upsample(name) %>%
  prep() 

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
test_data <- bake(rec, new_data = training_data)
oasis_data<-bake(rec, new_data=flurn)

#randomforest is called from parsnip
library(parsnip)
#accuracy is from forecast
library(forecast)
is4<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(name ~ ., data = train_data) %>%
  predict(new_data = oasis_data) %>%
  mutate(truth = flurn$name)

library(ggplot2)
ggplot(is4, aes(truth, .pred_class, colour=truth))+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ggtitle("Random Forest with TF/IDF, Key, Tempo, Speechiness, and Liveness")+labs(x="Artist", y="Predicted Artist", key="Artist")



#this will get us the data we need
RH<-get_discography("Red Hot Chili Peppers")

#Lets redo this with Red Hot Chilli Peppers Lyrics

#unnest the lyrics
RH2<-unnest(RH, lyrics)

#use our custom function to make a nice long dataframe wth that
RH3<-song_level_lyrics_spotify(RH2)

data_split <- RH3 %>%
  initial_split(prop = .8, strata = album_name)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec2 <- recipe(album_name ~ song_lyric, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 2)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 200) %>%
  #tfidf method
  step_tfidf(song_lyric) %>%
  step_upsample(album_name) %>%
  prep() 

train_data <- juice(rec2)
val_data <- bake(rec2, new_data = validation_data)
test_data <- bake(rec2, new_data = training_data)

#randomforest is called from parsnip
library(parsnip)
#accuracy is from forecast
library(forecast)
is5<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(album_name ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$album_name)

#this is pretty ok, but not as tight as lil wayne
library(ggplot2)
ggplot(is5, aes(truth, .pred_class))+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))

#putting it all together...
Combi<-bind_rows(LW3, RH3) 

data_split <- Combi %>%
  initial_split(prop = .8, strata = album_name)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec <- recipe(album_name ~ song_lyric, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 2)) %>%
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
Combi2<-rand_forest("classification") %>%
  set_engine("randomForest") %>%
  fit(album_name ~ ., data = train_data) %>%
  predict(new_data = val_data) %>%
  mutate(truth = val_data$album_name)

library(ggplot2)
ggplot(Combi2, aes(truth, .pred_class))+geom_jitter()+theme_classic()+theme(axis.text.x = element_text(angle = 45))

#these lines of code will let us see if it effectively resorted the albums
filter(Combi2, truth!=.pred_class)
wayne_albums<-levels(as.factor(LW2$album_name))
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

#graphic for if it did effectively sort RHCP and 
ggplot(zort, aes(.pred_class, truth, colour=name.1))+geom_jitter()+facet_wrap(~name)


