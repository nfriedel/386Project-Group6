covid.hd <- read.csv("C:\\Users\\nickf\\Desktop\\School\\Spring 2020\\Big Data\\Full.Dataset.csv", header=TRUE)
View(covid.hd)

#---------------------------------Cleaning the Data-----------------------------------#

covid.hd[covid.hd == 0.0000]<-NA 
covid.hd$Mortality.Rate[is.na(covid.hd$Mortality.Rate)] = 0 #make sure dependent variable data that is 0, isn't changed to NA
covid.hd$Developing.Country.Binary[is.na(covid.hd$Developing.Country.Binary)] = 0
covid.hd$Undernourished[is.na(covid.hd$Undernourished)] = 0
covid.hd$Confirmed[is.na(covid.hd$Confirmed)] = 0
covid.hd$Deaths[is.na(covid.hd$Deaths)] = 0

covid.hd <- covid.hd[-c(117,135), ]   #Removes Republic of Moldova and Zimbabwe (NA for GDP per Cap)
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

#Variables to be used in Regression
covid.v <- covid.hd[1:153, c(2,5,25,26,31,32,34,36,46,50,51,55,64,65)]
View(covid.v)

names(covid.v)[1]<- "GDP.per.Cap"
names(covid.v)[2]<- "Developing.Dummy"
names(covid.v)[5]<- "Pop"
names(covid.v)[6]<- "m.rate"
names(covid.v)[7]<- "kcal.Animal"
names(covid.v)[8]<- "kcal.Cereal"
names(covid.v)[9]<- "kcal.Sugar"
names(covid.v)[10]<- "kg.Alcohol"
names(covid.v)[11]<- "kg.Animal"
names(covid.v)[12]<- "kg.Fruit"
names(covid.v)[13]<- "kg.Veg"
names(covid.v)[14]<- "protein.Animal"

na.omit(covid.v)


#----------------------------------------Regression Model--------------------------------#

#Model1 - No nutritional data
Model1 <- lm(m.rate~GDP.per.Cap+Developing.Dummy+Obesity+Undernourished+Pop, covid.v)
#Model2 - Country statistics with nutritional data
Model2 <- lm(m.rate~GDP.per.Cap+Developing.Dummy+Obesity+Undernourished+Pop+kcal.Sugar+
               kg.Veg+kcal.Cereal+kg.Animal+kg.Alcohol+kg.Fruit, covid.v)

View(Model1)
View(Model2)

##Model1 Analysis
summary(Model1)
plot(Model1$residuals)
abline(0,0,col='black', lwd = 4)
hist(Model1$residuals, prob = TRUE)
curve(dnorm(x, mean = mean(Model1$residuals), sd = sd(Model1$residuals)), col = "darkblue", lwd = 2, add=TRUE)
library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(Model1$residuals) #null hypothesis: data is distribution is normal

##Model2 Analysis
summary(Model2)
plot(Model2$residuals)
abline(0,0,col='black', lwd = 4)
hist(Model2$residuals, prob = TRUE)
curve(dnorm(x, mean = mean(Model2$residuals), sd = sd(Model2$residuals)), col = "darkblue", lwd = 2, add=TRUE)
library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(Model2$residuals) #null hypothesis: data is distribution is normal

##Partitioning Data##

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.v$m.rate, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-covid.v[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.v[-inTrain,]

##Prediction and Validation##
model.pred <- lm(m.rate~GDP.per.Cap+Developing.Dummy+Obesity+Undernourished+Pop+kcal.Sugar+
                   kg.Veg+kcal.Cereal+kg.Animal+kg.Alcohol+kg.Fruit, Training)

#EVALUATE M9 ON THE TEST PARTITION TO COMPUTE THE OUT-OF-SAMPLE PREDICTIONS
predictions<-predict(model.pred, Testing)
View(predictions) # view predictions for December

##CALCULATE ROOT MEAN SQUARE PREDICTION ERROR ON TEST DATA: THE OUT-OF-SAMPLE ERROR MEASURE
RMSE=sqrt(sum((predictions-Testing$m.rate)^2)/(length(Testing$m.rate-11)))
RMSE #report root mean squared error (E_out) using the out-of-sample testing data

##Out-of-Sample Error of 3.99

#--------------------------------Classification Model---------------------------------#

covid.v$Developing.Dummy <- factor(covid.v$Developing.Dummy) #convert to factor (categorical) variable
View(covid.v)

#####################
##PARTITIONING DATA##
#####################

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.v$Developing.Dummy, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-covid.v[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.v[-inTrain,]  

#####################
#LOGISTIC REGRESSION#
#####################

M_LOG<-glm(Developing.Dummy ~ GDP.per.Cap+Obesity+Undernourished+Pop+m.rate+kcal.Sugar+
             kg.Veg+kcal.Cereal+kg.Animal+kg.Alcohol+kg.Fruit, data = Training, family = "binomial")
summary(M_LOG)
exp(cbind(M_LOG$coefficients, confint(M_LOG)))
confusionMatrix(table(predict(M_LOG, Testing, type="response") >= 0.5,
                      Testing$Developing.Dummy == 1))
##Accuracy of .8864##
