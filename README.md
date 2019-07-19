# Predict-number-of-upvotes
Help a StackOverflow like platform to identify the best question authors by predicting the upvote count of questions asked.

Problem Statement
An online question and answer platform has hired you as a data scientist to identify the best question authors on the platform. This identification will bring more insight into increasing the user engagement. Given the tag of the question, number of views received, number of answers, username and reputation of the question author, the problem requires you to predict the upvote count that the question will receive.

Link : https://datahack.analyticsvidhya.com/contest/enigma-codefest-machine-learning-1/

In the training dataset we see non-linear behaviour in the variables Reputation,Views and Answers.
Factoral Analysis is performed to check if there's any correlation between variables.

On an average the questions are answered not more than 3 times.
Thus,feature engineering of Answers variable is done such that the questions answered on an average less than 3 times will be voted as difficult (1) else easy (0).

The Tag variables is converted into numeric vector.

As was mentioned earlier about the non-linearity in variables, we scale the variables Reputation,Views and Tag using z-score normalization.

On partitioning the data 75:25 and thereafter applying linear regression we get RMSE value of 3083.507.
Also, residuals are not normally distributed and heteroscedasticity is observed, i.e. no random chance variation.

We therefore use the polynomial regression because the independent variables exhibit non-linear behavior. We attain and RMSE value of 970.515.

