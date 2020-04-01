#this is the third file in the lil wayne series, it will deploy a binary classifier using a neural network to distinguish the artists

#first, rename the colmn in Zlurm so we can join it to combi
colnames(zlurm)[1]<-"album_name"
CombiX<-inner_join(Combi, zlurm)

#produce a new array of ID numbers, this code produces a vector of numbers from A:B where A is 1 and B is the total length of Combi
combi_id<-1:dim(Combi)[1]
#produce a dataframe where the id is the left colmn and the data is on the right
id_lyricsX<-data.frame(combi_id, CombiX)

#remove an extraneous column 
id_lyricsX<-select(id_lyricsX, -name.1)

#pivot that new dataset to wide
lyrics_preX<-id_lyricsX%>%
  pivot_wider(
    #the names that we will retain are those of the ARTISTS in this case RHCP and lil wayne
    names_from = name,
    #the values are from NOT the lyric, the ID, the album, or the track
    values_from = -c(song_lyric, combi_id, album_name, track_name),
  )

#this produces a wide dataset where the last two columns indicate either that the song is from Wayne or Red hot

#we can encode the artists by removing those last two columns
artist_encode<-select(lyrics_preX, 5:6)

#if it is NA, replace it with 0
ae2<-replace(artist_encode, is.na(artist_encode), 0)

#if it is NOT 0, replace with 1
ae2<-replace(ae2, ae2 !=0, 1)

#HERE IS A POINT WHERE A LOT OF PEOPLE HAVE PROBLEMS
#the entire result matrix needs to be numeric
#tensor based neural networks deal exclusviely with clusters of numbers
ae2[, 1:2] <- sapply(ae2[, 1:2], as.numeric)

#now we can rejoin these datasets
album_data<-select(lyrics_preX, 1:4)
two_classes<-data.frame(album_data, ae2)

#NOW WE HAVE THE DATASET

#now mirror the structure of the keras call that made all this work
#the core dataset is really LP_final
data_split <- two_classes%>%
  initial_split(prop = .5)

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
max_len = 2000

## pad sequence
x_train <- pad_sequences(training_seq, maxlen = max_len, padding = "post")
x_test <- pad_sequences(valid_seq, maxlen = max_len, padding = "post")

## extract targets columns and convert to matrix
y_train <- training_data %>% 
  select(5:6) %>% 
  as.matrix()

## define embedding size
emd_size = 64

## define model - the last dense layer should be equal to the number of categories in your output
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = vocab_size, output_dim = emd_size) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 2, activation = "sigmoid")

summary(model)

#model compile assumptions
model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

#model size, you can mess with the epochs especially
history <- model %>% 
  fit(x_train,
      y_train,
      epochs = 200,
      batch_size = 64,
      validation_split = 0.05,
      verbose = 0) 

#accuracy plotting
plot(history)+
  theme_minimal()+
  ggtitle("Loss and Accuracy Curves")

#now for the fun part
predicted_prob <- predict_proba(model, x_test) 
predicted_classes <- predict_classes(model, x_test) 
View(predicted_prob)


## join ids and predictions
y_test <- as_data_frame(predicted_prob)
names(y_test) <- names(training_data)[5:6] ## labels names
y_test <- add_column(y_test, combi_id = validation_data$combi_id, .before = 1) 
y_complete<-inner_join(y_test, id_lyricsX, by="combi_id")
View(y_complete)

library(ggrepel)
ggplot(y_complete, aes(Wayne, Red.Hot, colour=name))+geom_text_repel(aes(label=track_name))

