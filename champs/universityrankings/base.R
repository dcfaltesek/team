#clean by removing the names of all the rows that 

library(dplyr)
rankings2 <- read_excel("~/Desktop/rankings2.xlsx")

#run all this again as a subroutine with the 19 data
rankings_19 <- read_excel("~/Desktop/rankings_19.xlsx")

View(rankings2)


#create a dataframe where the ranks are numeric so we can actually do some joining
rankings3<-data.frame(as.numeric(rankings_18$Rank), rankings_19)

#rename the first column so the computer can use it for an id
colnames(rankings3)[1]<-"Rank2"

#now innerjoin referent to the actaul data
library(tidyr)

clean_ranks<-drop_na(rankings3, Rank)
View(clean_ranks)

#now get a clean list of where the institutions are

joins<-rankings3%>%
  filter(is.na(Rank))

joins2<-joins%>%
  filter(Name != "Explore")%>%
  filter(Name != "Country/Region")

View(joins2)
View(clean_ranks)

clean_ranks2<-clean_ranks[1:1258,]

#now bind the columns
full_18<-bind_cols(clean_ranks2, joins2)

A<-mutate(full_19, Year="2019")
B<-mutate(full_20, Year="2020")
C<-mutate(full_18, Year="2018")

D<-bind_rows(A, B, C)

E<-D%>%
  select(Rank2, Name, Name1, Year)

library(ggplot2)
library(GGally)

C%>%
  filter(Rank2<41)%>%
  ggplot(aes(as.integer(Year), as.integer(Rank), colour=Name1))+geom_jitter()


ggparcoord(E, columns = 1:3, groupColumn=4)

E%>%
  filter(as.integer(Rank2<31) & as.integer(Rank2>0))%>%
  ggplot(aes(Year, Rank2, colour=Name))+geom_text(aes(label=Name))





  
