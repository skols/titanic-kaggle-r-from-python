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

# Analyze by describing data
# Show column names
names(train)

# Preview the data
head(train)
tail(train)

# Examine data
glimpse(train)
glimpse(test)

# Look at summary data
summary(train)
summary(test)

# Analyze by pivoting features
# Pclass
train %>% 
  group_by(Pclass) %>% 
  summarize(mean_S=mean(Survived)) %>% 
  arrange(desc(mean_S))

# Sex
train %>% 
  group_by(Sex) %>% 
  summarize(mean_S=mean(Survived)) %>% 
  arrange(desc(mean_S))

# SibSp
train %>% 
  group_by(SibSp) %>% 
  summarize(mean_S=mean(Survived)) %>% 
  arrange(desc(mean_S))

# Parch
train %>% 
  group_by(Parch) %>% 
  summarize(mean_S=mean(Survived)) %>% 
  arrange(desc(mean_S))

# Analyze by visualizing data
