#Load in data from Tony's URL
library(curl)
f <- curl("https://raw.githubusercontent.com/difiore/ada-datasets/main/data-wrangling.csv")
d <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(d)

#Set Variables
BSD <- d$Body_mass_male_mean/d$Body_mass_female_mean
BSD
sex_ratio <- d$AdultFemale/d$AdultMales
sex_ratio
DI <- d$DayLength_km/(pi * d$HomeRange_km2)
DI

#plot the data
library(ggplot2)
library(dplyr)
#Movement and day length overall
ggplot(data = d, mapping = aes(x = Move, y = DayLength_km)) +
  geom_point() +
  ggtitle("Movement and Day Range Length Overall")

#Movement and day length by family
ggplot(data = d, mapping = aes(x = Move, y = DayLength_km)) +
  geom_point() +
  ggtitle("Movement and Day Range Length by Family") +
  facet_wrap(~Family)

#Group size and day range length overall
ggplot(data = d, mapping = aes(x = MeanGroupSize, y = DayLength_km)) +
  geom_point() +
  ggtitle("Group Size and Day Range Length Overall")

#Group size and day range by family
ggplot(data = d, mapping = aes(x = MeanGroupSize, y = DayLength_km)) +
  geom_point() +
  ggtitle("Group Size and Day Range Length by Family") +
  facet_wrap(~Family)


#Canine Dimorphism and Body Dimorphism Overall
ggplot(data = d, mapping = aes(x = BSD, y = Canine_Dimorphism)) +
  geom_point() +
  ggtitle("Body Size and Canine Size Dimorphism Overall")

#Canine Dimorphism and Body Dimorphism by Familt
ggplot(data = d, mapping = aes(x = BSD, y = Canine_Dimorphism)) +
  geom_point() +
  ggtitle("Body Size and Canine Size Dimorphism by Family") +
  facet_wrap(~Family)

#This code will create a new variable called diet_strategy
#which stores the diet of each species
#we need {tidyverse} to use the mutate function
library(tidyverse)
d <- d %>%
  mutate(d, diet_strategy = ifelse(Fruit >= 50, "frugivore", ifelse(Leaves >= 50,
  "folivore", ifelse(Fruit < 50 & Leaves < 50, "omnivore", NA))))

#now we are going to use box plots to plot 
ggplot(data = d, mapping = aes(x = diet_strategy, y = MeanGroupSize)) +
  geom_boxplot() +
  ggtitle("Diet Strategy and Mean Group Size")

#Here I will use tools from the {dplyr} package to create new variables in the data
#This variable will be a concatenation of the Genus and Species variable
library(dplyr)
d <- d %>%
  s <- mutate(d, Binomial = paste(Genus, Species, sep = " ")) %>%
  select(s, Binomial, Family, Brain_Size_Species_Mean, Body_mass_male_mean) %>%
  group_by(s, Family) %>%
  summarize(s, mean(Brain_Size_Species_Mean, na.rm = TRUE), mean(Body_mass_male_mean, na.rm = TRUE))




