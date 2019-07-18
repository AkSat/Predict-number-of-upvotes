library(dplyr)
library(caret)
library(ggplot2)
library(car)

library(dataPreparation)

# ID	-  Question ID
# Tag	- Anonymised tags representing question category
# Reputation	- Reputation score of question author
# Answers	- Number of times question has been answered
# Username	- Anonymised user id of question author
# Views	- Number of times question has been viewed
# Upvotes	(Target) - Number of upvotes for the question

setwd("F:/Data Contests/AnalyticsVidya/Completed/Predict Number of Upvotes")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)
str(test)

test$Upvotes <- NA


train1 <- train[,-c(1,5)]
test1 <- test[,-c(1,5)]


names(train1)


str(train1)
summary(train1)


summary(train1$Answers)
summary(test1$Answers)


plot(train1$Answers,train1$Upvotes)



# Non-linear behaviour of independent variables ============================

qqPlot(train1$Reputation)

qqPlot(train1$Views)

unique(train1$Answers)

qqPlot(train1$Answers)

# ====================== Binning of Answers variable 
# such that the questions answered on an average less than 3 times 
# will receive less no. of votes

train1$Answers1 <- ifelse(train1$Answers<=3,1,0)
test1$Answers1 <- ifelse(test1$Answers<=3,1,0)


names(train1)

train1 <- train1[,-3]
test1 <- test1[,-3]



# ===================== Converting tag variable (qualitative) into quantitative

train1$Tag1 <- as.numeric(train1$Tag)
train1 <- train1[,-1]


test1$Tag1 <- as.numeric(test1$Tag)
test1 <- test1[,-1]

names(train1)

qqPlot(train1$Tag1)

# ================= Partitioning of data

set.seed(100)
sampleind <- sample(nrow(train1),0.75 * nrow(train1),replace = FALSE)
trains <- train1[sampleind,]
tests <- train1[-sampleind,]

summary(trains$Answers)


# =============== scaling of variables using mean and standard deviation formula

names(trains)
scales <- build_scales(dataSet = trains, 
                       cols = c("Reputation","Views","Tag1"),
                       verbose = TRUE)

trains <- fastScale(dataSet = trains, scales = scales, verbose = TRUE,
                    way = "scale")

mod12 <- lm(Upvotes ~ . ,data = trains)
summary(mod12)



names(test1)

scales <- build_scales(dataSet = test1, 
                       cols = c("Reputation","Views","Tag1"),
                       verbose = TRUE)

test1 <- fastScale(dataSet = test1, scales = scales, verbose = TRUE,
                   way = "scale")



# ====================== Modelling
# using the polynomial regression because the independent variables 
# exhibit non-linear behavior

model_poly <- lm(Upvotes ~ poly(cbind(Reputation,Views,Tag1, Answers1),
                                degree = 4,raw = TRUE),
                 data = trains)
summary(model_poly)


prediction_poly <- predict(model_poly, newdata = test1)
summary(prediction_poly)



solution <- data.frame(ID=test$ID,Upvotes=prediction_poly)
write.csv(file = "submissions14poly.csv",x=solution,row.names = FALSE)




