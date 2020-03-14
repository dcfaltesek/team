#quality libraries
library(tidyr)
library(stringr)
library(dplyr)
library(rsample)
library(textrecipes)
library(parsnip)
library(ggplot2)

#so in the base directory I loaded a really old tournment
#these should get the datasets from your local github source
`results` <- read.csv("~/Documents/GitHub/team/champs/2011NCAA.csv", stringsAsFactors=FALSE)
`teams` <- read.csv("~/Documents/GitHub/team/champs/2011NCAATEAMS.csv", stringsAsFactors=FALSE)

#notice that I stored these in really nice names

View(results)
View(teams)

#first list of tasks to do..
#1. clean the results from the games
#2. clean the team names in the teams so that NCAA is used to indicate that they made the touranment
#3. structure the results so that the winning team is ALWAYS on the left and the loser is on the right
#4. more advanced things to think about, how to we model each game, do we run a model on who wins and loses?










