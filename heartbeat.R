library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

#Read in the dataset
heartbeats <- fread("https://raw.githubusercontent.com/zonination/datasets/master/file.csv")

#Examine the structure of the data
str(heartbeats)

#Change the variable names at the beginning to avoid issues later with spaces and units
names(heartbeats)[1:4] <- c("animal","mass", "bpm", "longevity")

#Convert the mass variable from grams to kilograms
#heartbeats$mass <- heartbeats$mass /1000

str(heartbeats)

ggplot(heartbeats, aes(x = mass, y = bpm, size = longevity, color = animal)) + geom_point(alpha = 0.8) + 
  scale_x_log10() + scale_y_log10() + scale_size(name = "Animal")

