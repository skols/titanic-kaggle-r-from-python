# Load packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes) # theme_few()
library(scales) # dollar_format() function
library(mice) # imputation
library(randomForest) # classification algorithm

# Import data
train <- read.csv("train.csv", stringsAsFactors=F)
test <- read.csv("test.csv", stringsAsFactors=F)

combined <- bind_rows(train, test)

# Show column names
names(train)

# Preview the data
head(train)
tail(train)

# Examine data
glimpse(train)
glimpse(test)

# Look at summary data
summary(test)
