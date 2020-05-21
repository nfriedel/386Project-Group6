covid19.health.data <- read.csv("Full.Dataset.Revised.csv")
View(covid19.health.data)
summary(covid19.health.data)

covid.hd<-covid19.health.data
covid.hd[covid.hd == 0.0000]<-NA  
View(covid.hd)

covid.hd$Mortality.Rate[is.na(covid.hd$Mortality.Rate)] = 0 # make sure dependent variable data that is 0, isn't changed to NA
covid.hd$Developing.Country.Binary[is.na(covid.hd$Developing.Country.Binary)] = 0
covid.hd$Undernourished[is.na(covid.hd$Undernourished)] = 0
covid.hd$Confirmed[is.na(covid.hd$Confirmed)] = 0
covid.hd$Deaths[is.na(covid.hd$Deaths)] = 0
covid.hd$Obesity[is.na(covid.hd$Obesity)] = 0

summary(covid.hd)    # check which columns have an unreasonable amount of NA values or very low Medians/Max's


covid.hd$Aquatic.Products..Other_fat<-NULL         # 152 NA's
covid.hd$Aquatic.Products..Other_kcal<-NULL        # 154 NA's
covid.hd$Aquatic.Products..Other_kg<-NULL          # 92 NA's
covid.hd$Aquatic.Products..Other_protein<-NULL     # 134 NA's

covid.hd$Sugar.Crops_fat<-NULL                     # 146 NA's
covid.hd$Sugar.Crops_kcal<-NULL                    # 138 NA's
covid.hd$Sugar.Crops_kg<-NULL                      # 130 NA's
covid.hd$Sugar.Crops_protein<-NULL                 # 137 NA's

covid.hd$Sugar.Sweeteners_fat<-NULL              # 149 NA's
covid.hd$Sugar.Sweeteners_protein<-NULL          # 95 NA's

covid.hd$Miscellaneous_fat<-NULL                   # Mean = 0.059, Max = 0.45 and 24 NA
covid.hd$Miscellaneous_kcal<-NULL                  # Mean = 0.164, Max = 1.18 and 24 NA
covid.hd$Miscellaneous_kg<-NULL                    # Mean = 0.41, Max = 3.66 and 15 NA
covid.hd$Miscellaneous_protein<-NULL               # Mean = 0.184, Max = 1.13 and 19 NA

covid.hd$Offals_fat<-NULL                          # Mean = 0.154 and Max = 0.73
covid.hd$Offals_kcal<-NULL                         # Mean = 0.145 and Max = 0.8
covid.hd$Offals_kg<-NULL                           # Mean = 0.196 and Max = 1.23

covid.hd$Treenuts_kcal<-NULL                       # Mean = 0.314, Max = 1.42 and 21 NA
covid.hd$Treenuts_kg<-NULL                         # Mean = 0.131, Max = 0.76 and 11 NA
covid.hd$Treenuts_protein<-NULL                    # Mean = 0.283, Max = 1.89 and 16 NA

covid.hd$Spices_fat<-NULL                          # Mean = 0.297, Max = 2.69 and 12 NA
covid.hd$Spices_kcal<-NULL                         # Mean = 0.201, Max = 1.22 and 19 NA
covid.hd$Spices_kg<-NULL                           # Mean = 0.091 and Max = 0.663
covid.hd$Spices_protein<-NULL                      # Mean = 0.249, Max = 1.86 and 11 NA

covid.hd$Alcoholic.Beverages_fat<-NULL             # 154 NA's
covid.hd$Animal.fats_kg<-NULL                      # Mean = 0.233 and Max = 1.36
covid.hd$Stimulants_kg<-NULL                       # Mean = 0.21 and Max = 1.28
covid.hd$Alcoholic.Beverages_protein<-NULL         # Mean = 0.295, Max = 1.37 and 14 NA
covid.hd$Animal.fats_protein<-NULL                 # Mean = 0.115, Max = 0.98 and 7 NA
covid.hd$Vegetable.Oils_protein<-NULL              # Mean = 0.027, Max = 0.114 and 35 NA

covid.hd$X<-NULL       
covid.hd$X.1<-NULL  
covid.hd$X.2<-NULL
covid.hd$X.3<-NULL
covid.hd$X.4<-NULL
covid.hd$X.5<-NULL
covid.hd$X.6<-NULL

covid.hd <- covid.hd[-c(156, 157), ]                # Removes NA rows

summary(covid.hd)

# ----------------------Linear Regression Analysis---------------------#

# Model1 - all fat data #
Model1<-lm(Mortality.Rate ~ Animal.Products_fat+Animal.fats_fat+Cereals_fat+Eggs_fat+Fish.Seafood_fat+
             Fruits_fat+Meat_fat+Milk_fat+Oilcrops_fat+Pulses_fat+Starchy.Roots_fat+Stimulants_fat+
             Treenuts_fat+Vegetal.Products_fat+Vegetable.Oils_fat+Vegetables_fat, covid.hd)
summary(Model1)

# Model2 - all kcal data #
Model2<-lm(Mortality.Rate ~ Alcoholic.Beverages_kcal+Animal.fats_kcal+Cereals._kcal+Eggs_kcal+
             Fish.Seafood_kcal+Fruits_kcal+Meat_kcal+Milk_kcal+Oilcrops_kcal+Pulses_kcal+Starchy.Roots_kcal+
             Stimulants_kcal+Sugar.Sweeteners_kcal+Vegetal.Products_kcal+Vegetable.Oils_kcal+
             Vegetables_kcal, covid.hd)
summary(Model2)

# Model3 - all kg data #
Model3<-lm(Mortality.Rate ~ Alcoholic.Beverages_kg+Animal.Products_kg+Cereals_kg+Eggs_kg+
             Fish.Seafood_kg+Fruits_kg+Meat_kg+Milk_kg+Oilcrops_kg+Pulses_kg+Starchy.Roots_kg+
             Sugar.Sweeteners_kg+Vegetable.Oils_kg+Vegetables_kg+Vegetal.Products_kg, covid.hd)
summary(Model3)

# Model4 - all protein data #
Model4<-lm(Mortality.Rate ~ Animal.Products_protein+Cereals_protein+Eggs_protein+Fish.Seafood_protein+
             Fruits_protein+Meat_protein+Milk_protein+Offals_protein+Oilcrops_protein+Pulses_protein+
             Starchy.Roots_protein+Stimulants_protein+Vegetal.Products_protein+Vegetables_protein, covid.hd)
summary(Model4)

# Model5 - non-nutritional data #
Model5<-lm(Mortality.Rate ~ GDP.Per.Capita+Developing.Country.Binary+Obesity+Undernourished+Population, covid.hd)
summary(Model5)

# Model6 - complete regression analysis #
Model6<-lm(Mortality.Rate ~ GDP.Per.Capita+Developing.Country.Binary+Undernourished+Population
           +Animal.Products_kcal+Fruits_kcal+Sugar.Sweeteners_kcal+Vegetable.Oils_kcal
           +Vegetal.Products_kcal, covid.hd)
summary(Model6)

confint(Model6)

plot(Model6$residuals)               # scatterplot of residuals
abline(0,0,col='blue', lwd = 3) 
hist(Model6$residuals, prob = TRUE)  # histogram of residuals
library(tseries)
jarque.bera.test(Model6$residuals)   # null hypothesis: data distribution is normal


#####################
#Partioning the Data#
#####################

library(caret)

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.hd$Mortality.Rate, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-covid.hd[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.hd[-inTrain,]

###########################
#Prediction and Validation#
###########################

model.prediction <- lm(Mortality.Rate ~ GDP.Per.Capita+Developing.Country.Binary+Undernourished+Population
                       +Animal.Products_kcal+Fruits_kcal+Sugar.Sweeteners_kcal+Vegetable.Oils_kcal
                       +Vegetal.Products_kcal, Training)
summary(model.prediction)

#Evaluate Model7 on the Test Partition to Compute the Out-of-Sample Predictions
predictions<-predict(model.prediction, Testing)
View(predictions)

predictions[is.na(predictions)] = 0

##Calculate Root Mean Square Prediction Error on Test Data: The Out-of-Sample Error Measure
RMSE=sqrt(sum((predictions-Testing$Mortality.Rate)^2)/(length(Testing$Mortality.Rate-8)))
RMSE #report root mean squared error (E_out) using the out-of-sample testing data

## Out of Sample Error of 4.08 ##

# --------------------------Classification Task-------------------------#

#transforms the Developing Country variable into a new factor (categorical) variable
covid.hd$Developing.Country.Binary <- factor(covid.hd$Developing.Country.Binary)
View(covid.hd)

#####################
#Partioning the Data#
#####################

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.hd$Developing.Country.Binary, p=.70, list = FALSE) 

#stores these rows in a training set
Training<-covid.hd[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.hd[-inTrain,]  


#####################
#LOGISTIC REGRESSION#
#####################

M_LOG<-glm(Developing.Country.Binary ~ GDP.Per.Capita+Mortality.Rate, data = Training, family = "binomial")
summary(M_LOG)

exp(cbind(M_LOG$coefficients, confint(M_LOG)))

#builds the confusion matrix to look at accuracy on training data
confusionMatrix(table(predict(M_LOG, Training, type="response") >= 0.4,
                      Training$Developing.Country.Binary == 1))
## Accuracy of 0.9533 ##

#builds the confusion matrix to look at accuracy on testing data
confusionMatrix(table(predict(M_LOG, Testing, type="response") >= 0.4,
                      Testing$Developing.Country.Binary == 1))
## Accuracy of 0.9348 ##

