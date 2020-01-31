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

format_network<-function(x){
  f_source<-x$x.screen_name
  f_target<-stringr::str_replace_all(x$value, "@", "")
  data.frame(SOURCE=f_source, TARGET=f_target, tweet_number=x$f_prime2)
}

