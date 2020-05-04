#new model
library(dplyr)
#filter blackstreet to get the last three years where games were played



#davekoz is the last three years
davekoz<-nodoubt%>%
  filter(Year.x > 16)
#eugegroove isjust the useful columns from davekoz
eugegroove<-select(davekoz, c(team1, team2, Result, outcome, Year.x))

#clean teamdata
#cleans an extraneous "NCAA" from team data
library(stringr)
clean_school<-str_replace_all(teamdata17team1, "NCAA", "")%>%
  str_trim(side=c("both"))

#now reattach the cleaned school names
#this is named for Gato Barberi, listen to him. 
gato17<-data.frame(teamz=clean_school, teamdata17)
#remove the fake team1
gato17<-select(gato17, -team1)
#reinstall team one but now clean
colnames(gato17)[1]<-"team1"


#attach the relevant portions of eugegroove
eugegroove17<-filter(eugegroove, Year.x==17)
eugegroove18<-filter(eugegroove, Year.x==18)
eugegroove19<-filter(eugegroove, Year.x==19)

#testing subroutine to find errors in the db names
anti_join()


#attach stage one - REPLAC


kennyg<-inner_join(eugegroove17, gato17)
colnames(kennyg)[1]<-"placeholder"
colnames(kennyg)[2]<-"team1"
#RENAME FINAL PRODUCT
groverwashington<-inner_join(kennyg, gato17, by="team1")
colnames(groverwashington)[2]<-"team2"
colnames(groverwashington)[1]<-"team1"

frame1<-groverwashington


kennyg<-inner_join(eugegroove18, gato17)
colnames(kennyg)[1]<-"placeholder"
colnames(kennyg)[2]<-"team1"
#RENAME FINAL PRODUCT
groverwashington<-inner_join(kennyg, gato17, by="team1")
colnames(groverwashington)[2]<-"team2"
colnames(groverwashington)[1]<-"team1"

frame2<-groverwashington


kennyg<-inner_join(eugegroove19, gato17)
colnames(kennyg)[1]<-"placeholder"
colnames(kennyg)[2]<-"team1"
#RENAME FINAL PRODUCT
groverwashington<-inner_join(kennyg, gato17, by="team1")
colnames(groverwashington)[2]<-"team2"
colnames(groverwashington)[1]<-"team1"

frame3<-groverwashington
coreset<-bind_rows(frame1,frame2,frame3)
write.csv(coreset, "coreset.csv", row.names = FALSE)


#ADD LAYER of data for current year
colnames(teamdata20)[2]<-"team1"
#UPDATE THIS LINE
saxpack<-inner_join(round2, teamdata20, by="team1")
colnames(saxpack)[1]<-"placeholder"
colnames(saxpack)[2]<-"team1"
boneyjames<-inner_join(saxpack, teamdata20, by="team1")
colnames(boneyjames)[2]<-"team2"
colnames(boneyjames)[1]<-"team1"


kashiwa<-boneyjames%>%
  mutate(team1ft = (FTA.x*FT..x)--(mean(FTA.x*FT..x)/sd(FTA.x*FT..x)))%>%
  mutate(team2ft = (FTA.y*FT..y)-(mean(FTA.y*FT..y)/sd(FTA.y*FT..y)))%>%
  mutate(team1differential = ((Tm..x - mean(Opp..x))/sd(Tm..x)))%>%
  mutate(team2differential = ((Tm..y - mean(Opp..y))/sd(Tm..y)))%>%
  mutate(team13pt = ((X3PA.x * X3P..x)-(mean(X3PA.x) * mean(X3P..x)))/sd(X3PA.x * X3P..x))%>%
  mutate(team23pt = ((X3PA.y * X3P..y)-(mean(X3PA.y) * mean(X3P..y)))/sd(X3PA.y * X3P..y))%>%
  mutate(team1sos = SOS.x-mean(SOS.x)/sd(SOS.x))%>%
  mutate(team2sos = SOS.y-mean(SOS.y)/sd(SOS.y))%>%
  mutate(team1rb = (TRB.x-mean(TRB.x))/sd(TRB.x))%>%
  mutate(team2rb = (TRB.y-mean(TRB.y))/sd(TRB.y))%>%
  mutate(team1to = (TOV.x-mean(TRB.x))/sd(TRB.x))%>%
  mutate(team2to = (TOV.y-mean(TOV.y))/sd(TOV.y))

albright<-kashiwa%>%
  select(team1, team2, team1sos, team2sos, team1differential, team2differential, team1ft, team2ft,
         team13pt, team23pt, team1rb, team2rb, team1to, team2to, W.L..x, W.L..y)

write.csv(albright, "albright.csv", row.names=FALSE)
#albrightIS READY FOR PROCESSING
