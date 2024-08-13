library(tidyverse)

setwd("~/School Classses/Fall 2022/Intro to Data Science/Project/Data Science Project")
musicData <- read.csv("rym_clean1.csv")

musicdf2 <- musicData %>%
  select(position, release_date, primary_genres)

cleanMusic <- musicdf2 %>% 
  mutate(primary_genres = strsplit(as.character(primary_genres), ", "))  %>% 
  unnest(primary_genres)

cleanMusic$release_date <- format(as.Date(cleanMusic$release_date),"%Y")

dateDf <- Reduce(rbind,
                 by(cleanMusic,
                    cleanMusic["release_date"],
                    head,
                    n=50))


countedDf <- dateDf %>% 
  count(release_date, primary_genres) %>% 
  group_by(release_date) %>% 
  top_n(2, n)


countedDf <- dateDf %>% 
  count(release_date, primary_genres) %>% 
  group_by(release_date) %>% 
  top_n(1, n)

subset(countedDf, release_date == "2020")


countedDf <- dateDf %>% 
  count(release_date, primary_genres) %>% 
  group_by(release_date) %>% 
  top_n(2, n)

subset(countedDf, release_date == "2020")


genre = list("Singer-Songwriter", "Alternative Rock")

genreTrack <- dateDf[(dateDf$primary_genres==genre),]
genreTrack$release_date <- as.Date(genreTrack$release_date, "%Y")
ggplot(genreTrack, aes(release_date, fill = primary_genres)) +
  geom_density(alpha = 0.2) +
  scale_x_date()


genreTrack <- dateDf
genreTrack$release_date <- as.Date(genreTrack$release_date, "%Y")
ggplot(genreTrack, aes(release_date, fill = release_date)) +
  geom_density(alpha = 0.2) +
  scale_x_date()
