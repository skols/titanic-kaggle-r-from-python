# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
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
ggplot(train, aes(Age)) +
  geom_histogram(bins=20) +
  facet_grid(. ~ Survived)
# Observations:
  # Infants (Age <=4) had high survival rate.
  # Oldest passengers (Age = 80) survived.
  # Large number of 15-25 year olds did not survive.
  # Most passengers are in 15-35 age range.

ggplot(train, aes(Age)) +
  geom_histogram(bins=20) +
  facet_grid(Pclass ~ Survived)
# Observations.
  # Pclass=3 had most passengers, however most did not survive.
  # Infant passengers in Pclass=2 and Pclass=3 mostly survived.
  # Most passengers in Pclass=1 survived.
  # Pclass varies in terms of Age distribution of passengers.

ggplot(train, aes(x=factor(Pclass), fill=Sex)) +
  geom_bar() +
  facet_grid(Survived ~ Embarked)
# Observations.
  # Female passengers had much better survival rate than males.
  # Exception in Embarked=C where males had higher survival rate. This could be a correlation
    # between Pclass and Embarked and in turn Pclass and Survived, not necessarily direct
    # correlation between Embarked and Survived.
  # Males had better survival rate in Pclass=3 when compared with Pclass=2 for C and Q ports.
  # Ports of embarkation have varying survival rates for Pclass=3 and among male passengers.

y_axis_labels <- min(train[, "Fare"]):max(train[, "Fare"])
ggplot(train, aes(Sex, Fare)) +
  geom_bar() +
  scale_y_continuous(labels=y_axis_labels, breaks=y_axis_labels) +
  facet_grid(Embarked ~ Survived)

# Wrangle data
dim(train)
dim(test)
dim(combined)

train <- train %>% 
  select(-c(Ticket, Cabin))

test <- test %>% 
  select(-c(Ticket, Cabin))

combined <- combined %>% 
  select(-c(Ticket, Cabin))

dim(train)
dim(test)
dim(combined)

# Combine titles with very low cell counts to a "rare" title
rare_title <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer",
                "Lady", "Major", "Rev", "Sir", "the Countess")

# Create a function to add Title to dataframes
titles <- function(df) {
  df$Title <- gsub('(.*, )|(\\..*)', '', df$Name)
  
  # Reassign mlle, ms, and mme with common names
  df$Title[df$Title == "Mlle"] <- "Miss"
  df$Title[df$Title == "Ms"] <- "Miss"
  df$Title[df$Title == "Mme"] <- "Mrs"
  df$Title[df$Title %in% rare_title] <- "Rare"
  
  return(df)

}

combined <- titles(combined)
train <- titles(train)
test <- titles(test)

train %>%
  group_by(Title) %>% 
  summarize(mean_S = mean(Survived))

# Convert Title from categorical to ordinal
train$Title <- factor(train$Title, levels = c("Mr", "Miss", "Mrs", "Master", "Rare"))
train$Title <- as.integer(train$Title)
head(train)

test$Title <- factor(test$Title, levels = c("Mr", "Miss", "Mrs", "Master", "Rare"))
test$Title <- as.integer(test$Title)

# Name can now be safely dropped and PassengerId isn't needed, so it can also be dropped
train <- train %>% 
  select(-c(Name, PassengerId))

test <- test %>% 
  select(-c(Name, PassengerId))

combined <- bind_rows(train, test)

dim(train)
dim(test)
dim(combined)

# Convert Sex to numerical values where female=1 and male=0
train$Sex <- factor(train$Sex, levels = c("female", "male")) # female=1, male=2
train$Sex <- as.integer(train$Sex)
train$Sex <- gsub(2, 0, train$Sex) # replacing 2 with 0
head(train)

test$Sex <- factor(test$Sex, levels = c("female", "male"))
test$Sex <- as.integer(test$Sex)
test$Sex <- gsub(2, 0, test$Sex)

combined <- bind_rows(train, test)
head(combined)

# Completing a numerical continous feature - estimating and completing
# features with missing or null values
# Doing for Age first
ggplot(train, aes(Age)) +
  geom_histogram(alpha=0.5, bins=20) +
  facet_grid(Pclass ~ Sex)

guess_ages <- matrix(c(0, 0, 0), nrow=2, ncol=3)

# i is Sex (0, 1), j is Pclass (1, 2, 3)
for (i in c(0:1)) {
  for (j in c(1:3)) {
    guess_df <- train[((train$Sex == i) & (train$Pclass == j)), "Age"]
    age_guess <- median(guess_df, na.rm=TRUE)
    # guess_ages[i+1, j] = integer(age_guess/0.5 + 0.5) * 0.5
  }
}
