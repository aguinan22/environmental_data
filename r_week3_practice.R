#Require here to locate bird and habitat data files
require(here)

#Require psych to make pair plots pretty
require(psych)

#Import data
habitat_data <- read.csv(here("data","hab.sta.csv"))
bird_data <- read.csv(here("data","bird.sta.csv"))

#Check to see what data consists of
head(habitat_data)
head(bird_data)

#Check total abundance range of Wilson's Warbler to decide breaks for histogram
range(bird_data$WIWA)

#Plot histogram of Wilson's Warbler abundance
hist(bird_data[,"WIWA"], xlab = "Wilson's Warbler Abundance", breaks = 0:7 - 0.5, main = "Wilson's Warbler Frequency Histogram")

#Bust out a bunch of basal area data for the pair plots
pairs.panels(habitat_data[,c("ba.con","ba.hard","ba.snag")])