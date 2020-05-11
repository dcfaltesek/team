library(tidytext)
library(textfeatures)
library(dplyr)
library(ggplot2)

speech_meta<-function(X,Y){
  Q<-mutate(X, "paragraph"=1:dim(X)[1])
  R<-mutate(Q, "speech"=Y)
}

bush1<-speech_meta(bush_9_27_01, "O'Hare")
bush2<-speech_meta(bush_11_08_01, "Atlanta")
bush3<-speech_meta(bush_11_10_01, "United Nations")

bush<-bind_rows(bush1, bush2, bush3)

bush_words<-bush%>%
  unnest_tokens(word, text)%>%
  count(word, sort = TRUE)

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

pure_words<-bush%>%
  unnest_tokens(word, text)%>%
  count(word, sort = TRUE)
View(pure_words)

bush_bigrams<-bush%>%
  unnest_tokens(word, text, token = "ngrams", n=3)%>%
  count(word, sort = TRUE)
bush_bigrams

