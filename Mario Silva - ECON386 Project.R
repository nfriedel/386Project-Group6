Final_Data <- read_excel("~/School/Spring 2020/ECON 386/Final Project/Final Data.xls")
View(Final_Data)
covid.hd<-Final_Data

##########################Cleaning the Data##########################

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
covid.v <- covid.hd[1:69, c(1,2,3,5,6,7,8,10,13,14,15,16,17,18,19,20,21,22,23,24,25,28,31,32,33,34,35,38,39)]
View(covid.v)

names(covid.v)[1]<- "Country"
names(covid.v)[2]<- "GDP.per.Cap"
names(covid.v)[3]<- "Total.GDP"
names(covid.v)[4]<- "Developing.Dummy"
names(covid.v)[5]<- "Test"
names(covid.v)[6]<- "Test.per.pop"
names(covid.v)[7]<- "Test.per.1K"
names(covid.v)[8]<- "Animal.fat"
names(covid.v)[9]<- "Cereals.fat"
names(covid.v)[10]<- "Egg.fat"
names(covid.v)[11]<- "Fish.fat"
names(covid.v)[12]<- "Fruit.fat"
names(covid.v)[13]<- "Meat.fat"
names(covid.v)[14]<- "Milk.fat"
names(covid.v)[15]<- "Oil.fat"
names(covid.v)[16]<- "Pulses.fat"
names(covid.v)[17]<- "Starch.fat"
names(covid.v)[18]<- "Stim.fat"
names(covid.v)[19]<- "Sugar.fat"
names(covid.v)[20]<- "Nut.fat"
names(covid.v)[21]<- "Veg.fat"
names(covid.v)[22]<- "Obesity"
names(covid.v)[23]<- "Deaths"
names(covid.v)[24]<- "Recovered"
names(covid.v)[25]<- "Active"
names(covid.v)[26]<- "Pop"
names(covid.v)[27]<- "M.rate"

na.omit(covid.v)


##########################Regression Models##########################

#Model1 - Bad Fats
Model1 <- lm(M.rate~Total.GDP+GDP.per.Cap+Pop+Test+Developing.Dummy+Obesity+Deaths+Animal.fat+Meat.fat+Starch.fat+Oil.fat+Pulses.fat+Stim.fat, covid.v)
#Model2 - Good Fats
Model2 <- lm(M.rate~Total.GDP+GDP.per.Cap+Pop+Test+Developing.Dummy+Obesity+Deaths+Cereals.fat+Egg.fat+Fish.fat+Fruit.fat+Milk.fat+Nut.fat+Veg.fat, covid.v)

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



#########################PARTITIONING DATA##################################
set.seed(1234) 

inTrain <- createDataPartition(y=covid.v$Developing.Dummy, p=.70, list = FALSE) 

Training<-covid.v[inTrain,]  

Testing<-covid.d[-inTrain,]  

#########################Classification Task################################

covid.v$Developing.Dummy <- factor(covid.v$Developing.Dummy) #convert to factor (categorical) variable
View(covid.v)

#########################PARTITIONING DATA#################################

set.seed(1234) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=covid.v$Developing.Dummy, p=.70, list = FALSE) 

#stores these rows in the training set
Training<-covid.v[inTrain,]  

#stores all rows not in the training set in the test/validation set
Testing<-covid.v[-inTrain,]  

#########################LOGISTIC REGRESSION##############################

Mlog_reg<-glm(Developing.Dummy ~ Total.GDP+GDP.per.Cap+Pop+Test+M.rate+Obesity+Deaths+Animal.fat+Meat.fat+Starch.fat+Oil.fat+Pulses.fat+Stim.fat, data = Training, family = "binomial")
summary(log_reg)

exp(cbind(Mlog_reg$coefficients, confint(Mlog_reg)))

confusionMatrix(table(predict(Mlog_reg, Testing, type="response") >= 0.5,
                      Testing$Developing.Country.Binary == 1))

########Accuracy of 0.8864########

######################Prediction and Validation##########################

model.prediction <- lm(Developing.Dummy ~ Total.GDP+GDP.per.Cap+Pop+Test+M.rate+Obesity+Deaths+Animal.fat+Meat.fat+Starch.fat+Oil.fat+Pulses.fat+Stim.fat, Training)
summary(model.prediction)

#Evaluate Model1 on the Test Partition to Compute the Out-of-Sample Predictions
predictions<-predict(model.prediction, Testing)
View(predictions)

RMSE(predictions, Testing$M.Rate, na.rm=T)
RMSE

########Out of Sample Error of 11.36########
