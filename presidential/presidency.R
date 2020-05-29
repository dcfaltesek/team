library(tidytext)
library(textfeatures)
library(dplyr)
library(ggplot2)

#adds paragraph numbers and speech names
speech_meta<-function(X,Y){
  Q<-mutate(X, "paragraph"=1:dim(X)[1])
  R<-mutate(Q, "speech"=Y)
}

dim(X2009_naval_commencement)

#my three speeches
bush1<-speech_meta(bush_9_27_01, "O'Hare")
bush2<-speech_meta(bush_11_08_01, "Atlanta")
bush3<-speech_meta(bush_11_10_01, "United Nations")

obama1<-speech_meta(Obama_2015_State_on_the_Union, "SOTU_15")
obama2<-speech_meta(Obama_2015_Selma_Speech, "Selma")

obama3<-speech_meta(X2009_naval_commencement, "Naval")
obama4<-speech_meta(X2009_sotu, "09 state of the union")

#combne those
bush<-bind_rows(bush1, bush2, bush3)
bush<-bind_rows(obama1, obama2)

bush<-bind_rows(obama3, obama4)

#break it down by speech and word
bush_words<-bush%>%
  unnest_tokens(word, text)%>%
  count(speech, word, sort = TRUE)

total_words <- bush_words %>% 
  group_by(speech) %>% 
  summarize(total = sum(n))

paragraph_words <- left_join(bush_words, total_words, by="speech")

View(total_words)

freq_by_rank <- paragraph_words %>% 
  group_by(speech) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = speech)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
View(freq_by_rank)

#by speech information 
Zprime<-paragraph_words%>%
  bind_tf_idf(word, speech,n)
View(Zprime)

write.csv(Zprime, "Simons_Speeches.csv", row.names = FALSE)


words<-Zprime%>%
  filter(n>5)
View(words)

agapure_words<-bush%>%
  unnest_tokens(word, text)%>%
  count(word, sort = TRUE)
View(pure_words)

bush_bigrams<-bush%>%
  unnest_tokens(word, text, token = "ngrams", n=3)%>%
  count(word, sort = TRUE)
bush_bigrams
View(bush_bigrams)

A<-get_sentiments("afinn")
B<-get_sentiments("nrc")
C<-get_sentiments("bing")
D<-get_sentiments("loughran")

dim(A)
dim(B)
dim(C)
dim(D)

head(A)
head(B)
head(C)
head(D)


