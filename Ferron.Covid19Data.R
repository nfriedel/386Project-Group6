covid19.health.data <- read.csv("Full.Dataset.Revised.csv")
View(covid19.health.data)
summary(covid19.health.data)

covid.hd<-covid19.health.data
covid.hd$Alcoholic.Beverages_fat[covid.hd$Alcoholic.Beverages_fat == 0.0000]<-NA
View(covid.hd)
covid.hd[covid.hd == 0.0000]<-NA  

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
#------------------------------------------Linear Regression--------------------------------------------------#

################################  
####Linear Regression Models####
################################

# Model1 - kcal data #
Model2<-lm(Mortality.Rate ~ Alcoholic.Beverages_kcal+Animal.fats_kcal+Cereals._kcal+Eggs_kcal+
             Fish.Seafood_kcal+Fruits_kcal+Meat_kcal+Milk_kcal+Oilcrops_kcal+Pulses_kcal+Starchy.Roots_kcal+
             Stimulants_kcal+Sugar.Sweeteners_kcal+Vegetal.Products_kcal+Vegetable.Oils_kcal+
             Vegetables_kcal, covid.hd)
summary(Model1)

#Residual standard error: 4.418 on 150 degrees of freedom
#Multiple R-squared:  0.04028,	Adjusted R-squared:  0.01469 
#F-statistic: 1.574 on 4 and 150 DF,  p-value: 0.1842

#Model2 - kg data #
Model2<-lm(Mortality.Rate ~ Alcoholic.Beverages_kg+Animal.Products_kg+Cereals_kg+Eggs_kg+
             Fish.Seafood_kg+Fruits_kg+Meat_kg+Milk_kg+Oilcrops_kg+Pulses_kg+Starchy.Roots_kg+
             Sugar.Sweeteners_kg+Vegetable.Oils_kg+Vegetables_kg+Vegetal.Products_kg, covid.hd)
summary(Model2)

#Residual standard error: 4.437 on 136 degrees of freedom#
#(3 observations deleted due to missingness)#
#Multiple R-squared:  0.1143,	Adjusted R-squared:  0.01656 #
#F-statistic: 1.169 on 15 and 136 DF,  p-value: 0.3028#

# Model3 - non-nutritional data #
Model3<-lm(Mortality.Rate ~ GDP.Per.Capita+Developing.Country.Binary+Obesity+Undernourished+Population, covid.hd)
summary(Model3)

#Residual standard error: 4.408 on 147 degrees of freedom#
#(2 observations deleted due to missingness)#
#Multiple R-squared:  0.06046,	Adjusted R-squared:  0.02851 #
#F-statistic: 1.892 on 5 and 147 DF,  p-value: 0.09909#

#Model4- kcal and kg filtered#
Model4<-lm(Mortality.Rate ~Animal.fats_kcal+Milk_kcal+Oilcrops_kcal+Starchy.Roots_kcal+
             Stimulants_kcal+Sugar.Sweeteners_kcal+Alcoholic.Beverages_kg+Milk_kg+Oilcrops_kg+Starchy.Roots_kg+
             Sugar.Sweeteners_kg, covid.hd)
summary(Model4)

#Residual standard error: 4.399 on 137 degrees of freedom#
#(6 observations deleted due to missingness)#
#Multiple R-squared:  0.1076,	Adjusted R-squared:  0.03589#
#F-statistic: 1.501 on 11 and 137 DF,  p-value: 0.1378#

#Model5- final regression#
Model5<-lm(Mortality.Rate ~GDP.Per.Capita+Developing.Country.Binary+Obesity+Undernourished+Population+Animal.fats_kcal+Milk_kcal+Oilcrops_kcal+Starchy.Roots_kcal+
             Stimulants_kcal+Sugar.Sweeteners_kcal+Alcoholic.Beverages_kg+Milk_kg+Oilcrops_kg+Starchy.Roots_kg+
             Sugar.Sweeteners_kg, covid.hd)
summary(Model5)

#Residual standard error: 4.352 on 130 degrees of freedom#
#(8 observations deleted due to missingness)#
#Multiple R-squared:  0.1679,	Adjusted R-squared:  0.06554#
#F-statistic:  1.64 on 16 and 130 DF,  p-value: 0.06714#


plot(Model5$residuals)               # scatterplot of residuals
abline(0,0,col='blue', lwd = 3) 
hist(Model5$residuals, prob = TRUE)  # histogram of residuals
library(tseries)
jarque.bera.test(Model5$residuals)   # null hypothesis: data distribution is normal
#X-squared = 373.87, df = 2, p-value < 2.2e-16#

#######################
#Partitioning the Data#
#######################

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.hd$Obesity, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-covid.hd[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.hd[-inTrain,]


#############################
##Prediction and Validation##
#############################

model.prediction <- lm(Mortality.Rate ~GDP.Per.Capita+Developing.Country.Binary+Obesity+Undernourished+Population+Animal.fats_kcal+Milk_kcal+Oilcrops_kcal+Starchy.Roots_kcal+
                         Stimulants_kcal+Sugar.Sweeteners_kcal+Alcoholic.Beverages_kg+Milk_kg+Oilcrops_kg+Starchy.Roots_kg+
                         Sugar.Sweeteners_kg, Training)
summary(model.prediction)

#Residual standard error: 4.369 on 88 degrees of freedom
#(4 observations deleted due to missingness)
#Multiple R-squared:  0.2491,	Adjusted R-squared:  0.1126 
#F-statistic: 1.824 on 16 and 88 DF,  p-value: 0.04


predictions<-predict(model.prediction, Testing)
View(predictions)

RMSE=sqrt(sum((predictions-Testing$Mortality.Rate)^2)/(length(Testing$Mortality.Rate-7)))
View(RMSE)

predictions[is.na(predictions)]=0

## Out of Sample Error of 4.55 ##

#------------------------------------------Classification--------------------------------------------------#
#transforms the Developing Country variable into a new factor (categorical) variable
covid.hd$Developing.Country.Binary <- factor(covid.hd$Developing.Country.Binary)
View(covid.hd)


#######################
#Partitioning the Data#
#######################
#same as above#

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.hd$Obesity, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-covid.hd[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.hd[-inTrain,]



#####################
#LOGISTIC REGRESSION#
#####################

M_LOG<-glm(Developing.Country.Binary ~ GDP.Per.Capita+Mortality.Rate, data = Training, family = "binomial")
summary(M_LOG)

exp(cbind(M_LOG$coefficients, confint(M_LOG)))

confusionMatrix(table(predict(M_LOG, Testing, type="response") >= 0.4,
                      Testing$Developing.Country.Binary == 1))

#Accuracy : 0.9333         
#95% CI : (0.8173, 0.986)
#No Information Rate : 0.6889         
#P-Value [Acc > NIR] : 7.97e-05       

#Kappa : 0.8475         

#Mcnemar's Test P-Value : 1              

#            Sensitivity : 0.9286         
#            Specificity : 0.9355         
#         Pos Pred Value : 0.8667         
#         Neg Pred Value : 0.9667         
#             Prevalence : 0.3111         
#         Detection Rate : 0.2889         
#  Detection Prevalence : 0.3333         
#      Balanced Accuracy : 0.9320#

