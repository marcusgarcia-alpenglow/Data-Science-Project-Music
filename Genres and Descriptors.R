library(tidyverse)
library(dplyr)

setwd("~/School Classses/Fall 2022/Intro to Data Science/Project/Data Science Project")
musicData <- read.csv("rym_clean1.csv")

# Firstly, we will use the data set to determine which descriptors are often associated with which genres

##Provided a genre give a mood and vice versa or provide an artist/album 
##Year of birth album finder
##Best Rated

#selects only necessary columns for genre and descriptors

musicdf <- musicData %>%
  select(primary_genres, secondary_genres, descriptors)


#splits data set up at commas

singleVarMusic <- musicdf %>% 
  mutate(primary_genres = strsplit(as.character(primary_genres), ", ")) %>%
  unnest(primary_genres) %>% 
  mutate(secondary_genres = strsplit(as.character(secondary_genres), ", ")) %>%
  unnest(secondary_genres) %>%
  mutate(descriptors = strsplit(as.character(descriptors), ", ")) %>%
  unnest(descriptors)


#Summarizes and counts how many times each descriptor happened in a genre

summariseDf <- summarise(group_by(singleVarMusic,primary_genres,descriptors),count =n())


summary(lm(primary_genres ~1, data = summariseDf))

