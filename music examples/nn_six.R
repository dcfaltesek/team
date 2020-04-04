#libraries we need aside from spotifyR

library(dplyr)
library(tidyr)
library(keras)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(ggrepel)

#get more artists
TS<-get_discography("Taylor Swift")
FF<-get_discography("Foo Fighters")
RH<-get_discography("Red Hot Chili Peppers")
LW<-get_discography("Lil Wayne")
VP<-get_discography("Vulfpeck")
KW<-get_discography("Kanye West")
U<-get_discography("Usher")


TS2<-unnest(TS, lyrics)
FF2<-unnest(FF, lyrics)
RH2<-unnest(RH, lyrics)
LW2<-unnest(LW, lyrics)
VP2<-unnest(VP, lyrics)
KW2<-unnest(KW, lyrics)
U2<-unnest(U, lyrics)


TS3<-song_level_lyrics_spotify(TS2)%>%
  mutate(name="Taylor Swift")

RH3<-song_level_lyrics_spotify(RH2)%>%
  mutate(name="Chili Peppers")

FF3<-song_level_lyrics_spotify(FF2)%>%
  mutate(name="Foo Fighters")

LW3<-song_level_lyrics_spotify(LW2)%>%
  mutate(name="Lil Wayne")

VP3<-song_level_lyrics_spotify(VP2)%>%
  mutate(name="Vulfpeck")

KW3<-song_level_lyrics_spotify(KW2)%>%
  mutate(name="Kanye West")

U3<-song_level_lyrics_spotify(U2)%>%
  mutate(name="Usher")

#in this formulary, the foos are omited, you can sub in any six artists
six_artists<-bind_rows(TS3, RH3, LW3, VP3, KW3, U3)

#to load the usher example 

six_id<-1:dim(six_artists)[1]

six_long<-data.frame(six_id, six_artists)
View(four_long)

six_wide<-six_long%>%
  pivot_wider(
    names_from = c(name),
    #these become a UNIQUE KEY COMBINATION NAME AND DATE
    values_from = -c(six_id, album_name, track_name, song_lyric),)

six_wide_detect<-six_wide%>%
  select(5:10)

f_w_d2<-replace(six_wide_detect, is.na(six_wide_detect), 0)
f_w_d3<-replace(f_w_d2, f_w_d2 !=0, 1)
f_w_d3[, 1:6] <- sapply(f_w_d3[, 1:6], as.numeric)

A<-six_wide%>%
  select(1:4)

six_ready<-data.frame(A, f_w_d3)

library(rsample)
data_split <- six_ready%>%
  initial_split(prop = .5)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)


## define vocab size (this is parameter to play with)
vocab_size = 50000

tokenizer <- text_tokenizer(num_words = vocab_size) %>% 
  fit_text_tokenizer(training_data$song_lyric)

library(keras)
training_seq <- texts_to_sequences(tokenizer, training_data$song_lyric)
valid_seq <- texts_to_sequences(tokenizer, validation_data$song_lyric)
all_seq<-texts_to_sequences(tokenizer, six_ready$song_lyric)

#lilwayne song 4 in words
training_data$song_lyric[4]
#lilwayne song 4 in numbers
training_seq[4]


## calculate training comments lengths
song_length <- training_seq %>% 
  map(~ str_split(.x, pattern = " ", simplify = TRUE)) %>% 
  map_int(length)

## plot comments length distribution
data_frame(song_length = song_length)%>% 
  ggplot(aes(song_length))+
  geom_histogram(binwidth = 20)+
  theme_minimal()+
  ggtitle("Training data song length distribution")


## define max_len
max_len = 1500

## pad sequence
x_train <- pad_sequences(training_seq, maxlen = max_len, padding = "post")
x_test <- pad_sequences(valid_seq, maxlen = max_len, padding = "post")
x_all <- pad_sequences(all_seq, maxlen = max_len, padding = "post")

## extract targets columns and convert to matrix
y_train <- training_data %>% 
  select(5:10) %>% 
  as.matrix()

## define embedding size
emd_size = 64

## define model
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = vocab_size, output_dim = emd_size) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 6, activation = "hard_sigmoid")

summary(model)

model %>% compile(
  optimizer = 'adagrad',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

history <- model %>% 
  fit(x_train,
      y_train,
      epochs = 500,
      batch_size = 16,
      validation_split = 0.05,
      verbose = 0) 


plot(history)+
  theme_minimal()+
  ggtitle("Loss and Accuracy Curves")

#now for the fun part
predicted_prob <- predict_proba(model, x_all) 
predicted_classes <- predict_classes(model, x_all) 
View(predicted_classes)


## join ids and predictions
y_test <- as_data_frame(predicted_prob)
names(y_test) <- names(training_data)[5:10] ## labels names
y_test <- add_column(y_test, six_id = six_ready$six_id, .before = 1) 
y_complete<-inner_join(y_test, six_long, by="six_id")
View(y_complete)

ggplot(y_complete, aes(Foo.Fighters,Chili.Peppers, colour=name))+geom_text_repel(aes(label=track_name))


#joiner
y_test2 <- as_data_frame(predicted_classes)
value<-c(0,1,2,3,4,5)
pred_names<-c("Taylor Swift", "Red Hot Chili Peppers", "Lil Wayne", "Vulfpeck", "Kanye West", "Usher")
class_joiner<-data.frame(value, pred_names)

y_test2 <- as_data_frame(predicted_classes)
names(y_test2) <- names(training_data)[5:10] ## labels names
y_test2 <- add_column(y_test2, six_id = six_ready$six_id, .before = 1) 
y_complete2<-inner_join(y_test2, six_long, by="six_id")
y_class<-inner_join(y_complete2, class_joiner)

ggplot(y_class, aes(name,pred_names, colour=name))+geom_jitter()



View(y_class)

#evaluation routine
res_eval2<-y_class%>%
  filter(name != pred_names)

dim(res_eval)[1]
dim(res_eval2)[1]
