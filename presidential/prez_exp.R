#load some libraries
library(tidytext)
library(textfeatures)
library(dplyr)
library(ggplot2)

#adds paragraph numbers and speech names
reference<-1:66

farewell_2<-data.frame(farewell, reference)


farewell_words<-farewell_2%>%
  unnest_tokens(word, paragraph)%>%
  count(reference, word, sort = TRUE)

#all words are counted by paragraph, ref is paragraph number, n is count

total_words <- farewell_words %>% 
  group_by(reference) %>% 
  summarize(total = sum(n))

paragraph_words <- left_join(farewell_words, total_words, by="reference")

freq_by_rank <- paragraph_words %>% 
  group_by(reference) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = reference)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
View(freq_by_rank)

Zprime<-paragraph_words%>%
  bind_tf_idf(word, reference,n)
View(Zprime)

A<-get_sentiments("afinn")

Afinn<-inner_join(farewell_words, A)
prezz<-inner_join(farewell_words, pressy)

prezz_final<-prezz%>%
  group_by(reference)%>%
  summarize(sum(score), sd(score), mean(score))

Afinn_final<-Afinn%>%
  group_by(reference)%>%
  summarize(sum(value), sd(value), mean(value))


