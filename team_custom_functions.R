#this function will take any dataframe from rtweet and render our prefered base structure
address_network<-function(x){
  f_load<-x$text
  address_pattern<-"@[[:graph:]]+"
  f_internal<-stringr::str_extract_all(f_load, address_pattern)
  f_internal
  f_list<-reshape2::melt(f_internal)
  f_passing<-data.frame(f_list)
  f_prime<-dim(x)[1]
  f_prime2<-1:f_prime
  f_rugged<-data.frame(x$text, f_prime2, x$created_at, x$screen_name)
  f_rugged
  colnames(f_passing)[2]<-"f_prime2"
  f_final<-dplyr::inner_join(f_rugged, f_passing, by="f_prime2")
  f_final
}  

#this function renders the prepared network as an edgelist, you can then use it in gephi
format_network<-function(x){
  f_source<-x$x.screen_name
  f_target<-stringr::str_replace_all(x$value, "@", "")
  data.frame(SOURCE=f_source, TARGET=f_target, tweet_number=x$f_prime2)
}


#produces a DFM of a twitter corpus
topic_corpus<-function(x){
  f_7<-quanteda::corpus(x$text)
  f_8<-quanteda::dfm(f_7)
}

kyle_dfm<-topic_corpus(kyle)

#be sure to store the result
some_storage_var<-topic_corpus(yourcorpus)

#load your dfm object as X, number of topics as Y, number of terms z
#the result 
topic_blast<-function(x, y, z){
  p_1<-quanteda::stopwords("english")
  internal_X <- quanteda::dfm(kyle_dfm, remove = p_1)
  internal_X2 <- quanteda::dfm_trim(internal_X, min_termfreq = 4, max_docfreq = 10)
  set.seed(100)
  if (require(topicmodels)) {
    internal_X3 <- topicmodels::LDA(quanteda::convert(internal_X2, to = "topicmodels"), k = y)
    topic_terms<-topicmodels::get_terms(internal_X3, z)
  }
  internal_X4<-topicmodels::topics(internal_X3)
  internal_X5<-data.frame(internal_X4)
  internal_X6<-row.names(internal_X5)
  internal_X7<-stringr::str_replace_all(internal_X6, "text", "")
  topic_assignments<-data.frame(doc_number=internal_X7, doc_topic=internal_X4)
  returns<-list(topic_terms=topic_terms, topic_assignments=topic_assignments)
}

#example
stuff<-topic_blast(kyle_dfm, 5, 5)





