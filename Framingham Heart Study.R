setwd("/Users/ahmedissaoui/Documents/Ahmed/Classes/IEOR 242/HW/HW2/")

library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
install.packages("lda")
library(lda)


# importing data
My_Data <- read.csv(file="framingham.csv", header=TRUE, sep=",")

# set a seed to always have the same split
set.seed(234)

# use sample.split to split the data set
split = sample.split(My_Data$TenYearCHD, SplitRatio = 0.7)

My_Data.train <- filter(My_Data, split == TRUE)
My_Data.test  <- filter(My_Data, split == FALSE)

# how many people had CHD within 10 years
table(My_Data.train$TenYearCHD)

###############################################################################
####                            Question A (i & ii)                            ####
###############################################################################

# Fit logistic regression model


mod <- glm(TenYearCHD ~ ., data = My_Data.train, family = "binomial")
summary(mod)

male, age, cigsPerDay, sysBP, glucose

# Look at the correlation between the best features (having the lowest p-value)
ggscatmat(My_Data.train, columns = c(2,3,8,13,14), alpha = 0.8)

# Predictions on the test set
predTest = predict(mod, newdata = My_Data.test, type = "response")

#Summary of model probabilities on the test set
summary(predTest)

###############################################################################
####                            Question A (iii)                            ####
###############################################################################

# Lets create a confusion matrix with a threshhold of probability = 0.5
table(My_Data.test$TenYearCHD, predTest > 0.15)

###############################################################################
####                            Question A (iv)                            ####
###############################################################################

# What is the accuracy of this model? 
cat("The accuracy of the logitc regression model is = ", (606+119)/nrow(My_Data.test))

# What is the TPR?
cat("The True Positive Rate =", 119 / (119 + 48))

# What is the FPR?
cat("The False Positive Rate =", 324 / (606 + 324))

###############################################################################
####                            Question A (vi)                            ####
###############################################################################


#Baseline model: predict that no one will have the disease 

# Accuracy of baseline on training:
table(My_Data.train$TenYearCHD)
cat("Accuracy of baseline on training =",2171/(2171+390))


# Accuracy of baseline on testing:
table(My_Data.test$TenYearCHD)
cat("Accuracy of baseline on testing =",930/(930+167))

###############################################################################
####                            Question A (vii)                            ####
###############################################################################

# Prediction for the new observation example
new_obs <- data.frame(male = 0,
                      age = 51,
                      education = "College",
                      currentSmoker = 1,
                      cigsPerDay = 20,
                      BPMeds = 0,
                      prevalentStroke = 0,
                      prevalentHyp = 1,
                      diabetes = 0,
                      totChol = 220,
                      sysBP = 140,
                      diaBP = 100,
                      BMI = 31,
                      heartRate = 59,
                      glucose = 78)

predict(mod, newdata = new_obs, type = "response")


###############################################################################
####                            Question B                                 ####
###############################################################################


# ROC curves
rocr.log.pred <- prediction(predTest, My_Data.test$TenYearCHD)
logPerformance <-performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0,1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)

###############################################################################
####                            Question C                                 ####
###############################################################################


# using LDA: 
require("MASS")
LdaModel <- lda(TenYearCHD ~ ., data = My_Data.train)

predTestLDA <- predict(LdaModel, newdata = My_Data.test)
predTestLDA_probs <- predTestLDA$posterior[,2]

table(My_Data.test$TenYearCHD, predTestLDA_probs >0.15)

# ROC curves
rocr.lda.pred <- prediction(predTestLDA_probs, My_Data.test$TenYearCHD)
ldaPerformance <-performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE)
abline(0,1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)



# comparing the two ROC curves on the same plot

plot(logPerformance, col = "blue")
plot(ldaPerformance, col ="red", add = TRUE)
abline(0,1)

