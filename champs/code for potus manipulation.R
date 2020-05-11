#upload 

Z1<-address_network(plan_export)
Z2<-format_network(Z1)

write.csv(Z2, "Z2.csv", row.names = FALSE)

library(ggplot2)
ggplot(plan_export, aes(created_at, favorite_count, colour=is_retweet))+geom_jitter()

library(tweetbotornot)

install.packages("tweetbotornot")
devtools::install_github("mkearney/tweetbotornot")
library(dplyr)
Z4<-Z2%>%
  count(TARGET)%>%
  arrange(desc(n))

sum(Z4[1:50,2])

T7<-filter(plan_export, mentions_screen_name=="realDonaldTrump")
library(ggplot2)
library(lubridate)

ggplot(plan_export, aes(hour(created_at)))+geom_bar()+facet_grid(~day(created_at))

turb<-plan_export%>%
  filter(day(created_at)<6 & day(created_at)>2)%>%
  count(hour(created_at))
View(turb)

z9<-plan_export%>%
  filter(day(created_at)==5)
zurn<-address_network(z9)
zurp<-format_network(zurn)
write.csv(zurp, "zurp.csv", row.names = FALSE)


z10<-plan_export%>%
  filter(day(created_at)==5)%>%
  filter(hour(created_at)>19)
  
turz<-address_network(z10)
jasonmraz<-format_network(turz)
write.csv(jasonmraz, "mraz.csv", row.names = FALSE)



tweetbotornot("LindaGrace6390")

solur<-thatnight%>%
  filter(MOD==186)%>%
  filter(EIGEN==0)


sample_borst<-sample_n(solur, 120)
tweetbotornot(snurd$NODE)
murk<-.Last.value
View(murk)

