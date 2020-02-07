#this function will take any dataframe from rtweet and render our prefered base structure
address_network<-function(x){
  #takes the text of the tweets and stores as vector
  f_load<-x$text
  #loads the REGEX pattern of a tweet
  address_pattern<-"@[[:graph:]]+"
  #extracts all things matching the address pattern of a tweet
  f_internal<-stringr::str_extract_all(f_load, address_pattern)
  f_internal
  #wide data to long
  f_list<-reshape2::melt(f_internal)
  #render that as a dataframe passing 
  f_passing<-data.frame(f_list)
  #use the dimension of the orignal data reference vector
  f_prime<-dim(x)[1]
  #attach the refernce vector
  f_prime2<-1:f_prime
  #create intermediate
  f_rugged<-data.frame(x$text, f_prime2, x$created_at, x$screen_name)
  f_rugged
  #renames for joining
  colnames(f_passing)[2]<-"f_prime2"
  f_final<-dplyr::inner_join(f_rugged, f_passing, by="f_prime2")
  f_final
}  

WU_parsed<-address_network(WUDR)


#this function renders the prepared network as an edgelist, you can then use it in gephi
format_network<-function(x){
  #exrtracts the screennames
  f_source<-x$x.screen_name
  #scrapes all surplus at symbols
  f_target<-stringr::str_replace_all(x$value, "@", "")
  #produces a dataframe that is an edgelist
  data.frame(SOURCE=f_source, TARGET=f_target, tweet_number=x$f_prime2)
}

WU_net<-format_network(WU_parsed)


#produces a DFM of a twitter corpus
topic_corpus<-function(x){
  #feed it the oringal datafarme
  #extracts the texts
  f_7<-quanteda::corpus(x$text)
  #document frequency matrix
  f_8<-quanteda::dfm(f_7)
}



WUDR_dfm<-topic_corpus(WUDR)

#be sure to store the result
some_storage_var<-topic_corpus(yourcorpus)

#load your dfm object as X, number of topics as Y, number of terms z
#the result 
topic_blast<-function(x, y, z){
  #gets stopwords
  p_1<-quanteda::stopwords("english")
  #reprocess as internal x
  internal_X <- quanteda::dfm(x, remove = p_1)
  #trims it, hyper parameters
  internal_X2 <- quanteda::dfm_trim(internal_X, min_termfreq = 4, max_docfreq = 10)
  #consistent results - WARNING THIS IS CHEATING!
  set.seed(100)
  #runs LDA
  if (require(topicmodels)) {
    internal_X3 <- topicmodels::LDA(quanteda::convert(internal_X2, to = "topicmodels"), k = y)
    topic_terms<-topicmodels::get_terms(internal_X3, z)
  }
  #gets the topics
  internal_X4<-topicmodels::topics(internal_X3)
  internal_X5<-data.frame(internal_X4)
  #extracts the row names
  internal_X6<-row.names(internal_X5)
  #cleans them
  internal_X7<-stringr::str_replace_all(internal_X6, "text", "")
  #has tweet numbers well as the topics
  topic_assignments<-data.frame(doc_number=internal_X7, doc_topic=internal_X4)
  returns<-list(topic_terms=topic_terms, topic_assignments=topic_assignments)
}

#example
stuff<-topic_blast(WUDR_dfm, 5, 5)

stuff$topic_terms
stuff$topic_assignments




