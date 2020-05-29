#load some libraries
library(tidytext)
library(textfeatures)
library(dplyr)
library(ggplot2)

#adds paragraph numbers and speech names
reference<-1:66

#this dataframe has the paragraph numbers
farewell_2<-data.frame(farewell, reference)

#breaks apart the pargraphs by word and then counts them by pargraph 
farewell_words<-farewell_2%>%
  unnest_tokens(word, paragraph)%>%
  count(reference, word, sort = TRUE)

#all words are counted by paragraph, ref is paragraph number, n is count
total_words <- farewell_words %>% 
  group_by(reference) %>% 
  summarize(total = sum(n))

#join these back to paragraph words, this is key for some measures
paragraph_words <- left_join(farewell_words, total_words, by="reference")

#get a frequency by rank
freq_by_rank <- paragraph_words %>% 
  group_by(reference) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

#frequency by rank 2 which is by the whole document
freq_by_rank2 <- paragraph_words %>% 
  group_by(word)%>%
  summarize(totes=sum(n))%>%
  arrange(desc(totes))%>%
  mutate("rank"=row_number(), "term frequency"= totes/sum(totes))

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = reference)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
View(freq_by_rank)

freq_by_rank2 %>% 
  ggplot(aes(rank, `term frequency`, color = reference)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


Zprime<-paragraph_words%>%
  bind_tf_idf(word, reference,n)
View(Zprime)

A<-get_sentiments("afinn")
B<-get_sentiments("nrc")
C<-get_sentiments("bing")
D<-get_sentiments("loughran")


Afinn<-inner_join(farewell_words, A)

pressy%>%
  group_by(word)%>%
  summarize(mean(score))


prezz<-inner_join(farewell_words, pressy)

prezz_final<-prezz%>%
  group_by(reference)%>%
  summarize(sum(score), sd(score), mean(score))

Afinn_final<-Afinn%>%
  group_by(reference)%>%
  summarize(sum(value), sd(value), mean(value))

#code that makes the joins work
View(prezz_final)
View(Afinn_final)
colnames(Afinn_final)[3]<-"sd(score)"
colnames(Afinn_final)[4]<-"mean(score)"
Afinn_final<-mutate(Afinn_final, type="Afinn")
prezz_final<-mutate(prezz_final, type="Pressy")


comp<-bind_rows(Afinn_final, prezz_final)
ggplot(comp, aes(reference, 'mean(score)', color=type))+geom_jitter()

#this makes the result sheet
hulk<-left_join(Afinn_final, prezz_final, by="reference")
View(hulk)

comparative<-data.frame(balance=hulk$`mean(score).x`-hulk$`mean(score).y`, var=hulk$`sd(score).x`-hulk$`mean(score).y`)
View(comparative)

