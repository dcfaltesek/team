#neural net attempt to sort all the 
library(dplyr)
library(tidyr)
library(keras)
library(ggplot2)

#create an id variable
combi_id<-1:382
#add that to thedata
id_lyrics<-data.frame(combi_id, Combi)
#pivot it, because neural nets seem to like wide data
lyrics_pre<-id_lyrics%>%
  pivot_wider(
    names_from = album_name,
    #these become a UNIQUE KEY COMBINATION NAME AND DATE
    values_from = -c(song_lyric, combi_id),
    values_fill = 
  )
#shift to numeric 0/1 categories
LP<-replace(lyrics_pre, is.na(lyrics_pre), 0)
LP2<-replace(LP, LP !=0, 1)
song_data<-select(LP2, -c(combi_id, song_lyric))

#produce a datafram which has the infomration
data_columns<-select(LP, c(combi_id, song_lyric))
View(song_data)

#go on ahead and make it numeric
song_intermediate1<-select(song_data, 1:20)
song_intermediate1[, 1:20] <- sapply(song_intermediate1[, 1:20], as.numeric)

#filter the NAs
LP_final<-bind_cols(data_columns, song_intermediate1)
LP_final<-LP_final%>%
  filter(song_lyric != "")


#NOW WE HAVE THE DATASET

#now mirror the structure of the keras call that made all this work
#the core dataset is really LP_final
data_split <- LP_final%>%
  initial_split(prop = .8)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)


## define vocab size (this is parameter to play with)
vocab_size = 20000

tokenizer <- text_tokenizer(num_words = vocab_size) %>% 
  fit_text_tokenizer(training_data$song_lyric)


training_seq <- texts_to_sequences(tokenizer, training_data$song_lyric)
valid_seq <- texts_to_sequences(tokenizer, validation_data$song_lyric)

#lilwayne song 4 in words
training_data$song_lyric[4]
#lilwayne song 4 in numbers
train_seq[4]


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
max_len = 1000

## pad sequence
x_train <- pad_sequences(training_seq, maxlen = max_len, padding = "post")
x_test <- pad_sequences(valid_seq, maxlen = max_len, padding = "post")

## extract targets columns and convert to matrix
y_train <- training_data %>% 
  select(3:22) %>% 
  as.matrix()

## define embedding size
emd_size = 64

## define model
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = vocab_size, output_dim = emd_size) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 20, activation = "sigmoid")

summary(model)

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

history <- model %>% 
  fit(x_train,
      y_train,
      epochs = 200,
      batch_size = 64,
      validation_split = 0.05,
      verbose = 0) 




plot(history)+
  theme_minimal()+
  ggtitle("Loss and Accuracy Curves")

#now for the fun part
predicted_prob <- predict_proba(model, x_test) 
predicted_classes <- predict_classes(model, x_test) 
View(predicted_classes)


## join ids and predictions
y_test <- as_data_frame(predicted_prob)
names(y_test) <- names(training_data)[3:22] ## labels names
y_test <- add_column(y_test, combi_id = validation_data$combi_id, .before = 1) 
y_complete<-inner_join(y_test, id_lyrics, by="combi_id")
View(y_complete)


View(y_test)

View(validation_data)
       