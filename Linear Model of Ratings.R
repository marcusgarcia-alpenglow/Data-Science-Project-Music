library(tidyverse)
library(dplyr)

setwd("~/School Classses/Fall 2022/Intro to Data Science/Project/Data Science Project")
musicData <- read.csv("rym_clean1.csv")

#Linear Model

RatingMusic <- musicData %>% 
  select(avg_rating, rating_count)

#Calculating the Time Difference

timeDiff <- difftime(Sys.Date(), musicData$release_date)
timeDiffYears <- timeDiff/365

#First Model

lmMusic <- lm(avg_rating ~ timeDiffYears, data = RatingMusic)
summary(lmMusic)

#Changing the model to include rating count

transRatingMusic = RatingMusic %>%
  transmute(
    rating = avg_rating,
    sqrt_rating_count = sqrt(rating_count),
  )

lmtransMusic <- lm(rating ~ timeDiffYears + `sqrt_rating_count`, data = transRatingMusic)


summary(lmtransMusic)
plot(lmtransMusic$residuals)



