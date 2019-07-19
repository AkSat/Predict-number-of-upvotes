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


names(train)
train1 <- train[,-c(1,5)]
test1 <- test[,-c(1,5)]


names(train1)
str(train1)
summary(train1)


summary(train1$Answers)
summary(test1$Answers)

quantile(train1$Answers,p=c(1:100)/100)
quantile(train1$Answers,p=c(900:1000)/1000)


plot(train1$Answers,train1$Upvotes)



# Non-linear behaviour of independent variables ============================

qqPlot(train1$Reputation)

qqPlot(train1$Views)

qqPlot(train1$Answers)

names(train1)







# Factor Analysis

fact1 <- factanal(scale(train1[,-1]),rotation = "varimax",factors = 1)
fact1

#.......no correlations







# ====================== Feature engineering of Answers variable 
# such that the questions answered on an average less than 3 times 
# will receive less no. of votes

summary(train1$Answers)  # .................... median : 3

train1$Answers1 <- ifelse(train1$Answers<=3,1,0)
test1$Answers1 <- ifelse(test1$Answers<=3,1,0)


names(train1)

train1 <- train1[,-3]
test1 <- test1[,-3]



# ===================== Converting tag variable (qualitative) into quantitative

unique(train1$Tag)
# a c r j p s h o i x


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



hist(train1$Reputation,probability = TRUE,xlim = c(-400000,400000))
hist(train1$Views,probability = TRUE,xlim = c(-1000000,1000000))
hist(train1$Tag1,probability = TRUE,xlim = c(-50,50))
# ........ above all not normal, hence scaling is required



# =============== scaling of variables using mean and standard deviation formula

names(trains)

# scaling method that gives mean and std. deviation

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



prediction <- predict(mod12, newdata = test1)


#Assumption checks
#check for : Residuals should be normally distributed 
hist(mod12$residuals)
qqPlot(mod12$residuals) #.......Non normal behaviour observed

#Multicollinearity Check
vif(mod12)
#.....here all the IV's have VIF < 10, hence no multicollinearity

#Constant  variance check
plot(mod12$fitted.values,mod12$residuals) 
#...pattern is observed (funnelling effect) .... thus heteroscedasticity



# Check for Good-fit
predictedVals <- mod12$fitted.values
actualVals <- trains$Upvotes

newdat <- data.frame(predictedVals,actualVals)

nrow(newdat)

p <- ggplot(newdat,aes(x = row(newdat)[,2],y = predictedVals))

p <- ggplot(newdat,aes(x = 1:nrow(newdat),y = predictedVals))

p + geom_line(colour = "blue") + geom_line(data = newdat,aes(y=actualVals),colour = "green")

# ... Not a good fitted graph


mse <- mean((mod12$residuals)^2)

rmse1 <- mse^0.5

# RMSE : 3083.507



# ====================== Modelling
# using the polynomial regression because the independent variables 
# exhibit non-linear behavior

model_poly <- lm(Upvotes ~ poly(cbind(Reputation,Views,Tag1, Answers1),
                                degree = 4,raw = TRUE),
                 data = trains)
summary(model_poly)

mse_poly <- mean((model_poly$residuals)^2)

rmse1_poly <- mse_poly^0.5


# RMSE : 970.515

#Assumption checks
#check for : Residuals should be normally distributed 
hist(model_poly$residuals)
qqPlot(model_poly$residuals) #.......normalality distribution is seen

#Constant  variance check
plot(model_poly$fitted.values,model_poly$residuals) 



# Check for Good-fit
predictedVals <- model_poly$fitted.values
actualVals <- trains$Upvotes

newdat <- data.frame(predictedVals,actualVals)

nrow(newdat)

p <- ggplot(newdat,aes(x = row(newdat)[,2],y = predictedVals))

p <- ggplot(newdat,aes(x = 1:nrow(newdat),y = predictedVals))

p + geom_line(colour = "blue") + geom_line(data = newdat,aes(y=actualVals),colour = "green")

# ... Better fitted graph


prediction_poly <- predict(model_poly, newdata = test1)
summary(prediction_poly)



solution <- data.frame(ID=test$ID,Upvotes=prediction_poly)
write.csv(file = "submissions14poly.csv",x=solution,row.names = FALSE)




