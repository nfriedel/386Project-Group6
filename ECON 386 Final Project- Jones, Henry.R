#####ECON386 Final Project- Henry Jones#####
##COVID-19 Mortality Rate###
covid.hd <- ECON_386_Final_Project_Revised_Data_Set
View(covid.hd)

###Code Cleaning Chunk###
covid.hd[covid.hd == 0.0000]<-NA 

covid.hd$`Mortality Rate`[is.na(covid.hd$`Mortality Rate`)] = 0 #make sure dependent variable data that is 0, isn't changed to NA
covid.hd$`Developing Country Binary`[is.na(covid.hd$`Developing Country Binary`)] = 0

covid.hd$Undernourished[is.na(covid.hd$Undernourished)] = 0
covid.hd$Confirmed[is.na(covid.hd$Confirmed)] = 0
covid.hd$Deaths[is.na(covid.hd$Deaths)] = 0
covid.hd <- covid.hd[-c(117), ]

summary(covid.hd)    # check which columns have an unreasonable amount of NA values or very low Medians/Max's

#Remove Variables with excessive NA's or insufficient nuritional value
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

###Linear Regression Model###
View(covid.hd)
dim(covid.hd)
head(covid.hd)
summary(covid.hd)
covidregress<- lm(covid.hd$`Mortality Rate`~covid.hd$Obesity+covid.hd$Undernourished+covid.hd$`GDP Per Capita`+covid.hd$Meat_fat+covid.hd$`Alcoholic Beverages_kcal`+covid.hd$`Developing Country Binary`+covid.hd$`Starchy Roots_fat`)
summary(covidregress)
confint(covidregress)
plot(covidregress$residuals)
abline(0,0,col='black')
hist(covidregress$residuals)
summary(covidregress$residuals)
library(tseries)
jarque.bera.test(covidregress$residuals)

###Prediction with Training and Testing Data using the Developing Country Binary###
covidTrainingDeveloping<-subset(covid.hd, covid.hd$`Developing Country`=='Y')
View(covidTrainingDeveloping)
plot(covidTrainingDeveloping$`Mortality Rate`~covidTrainingDeveloping$Obesity)
plot(covidTrainingDeveloping$`Mortality Rate`~covidTrainingDeveloping$Undernourished)
plot(covidTrainingDeveloping$`Mortality Rate`~covidTrainingDeveloping$`Alcoholic Beverages_kcal`)
abline(covidregress$coefficients[1], covidregress$coefficients[2], col='blue', lwd=2) 
covidTestingDeveloped<-subset(covid.hd, covid.hd$`Developing Country`=='N')
View(covidTestingDeveloped)
plot(covidTestingDeveloped$`Mortality Rate`~covidTestingDeveloped$Obesity)
plot(covidTestingDeveloped$`Mortality Rate`~covidTestingDeveloped$Undernourished)
plot(covidTestingDeveloped$`Mortality Rate`~covidTestingDeveloped$`Alcoholic Beverages_kcal`)
abline(covidregress$coefficients[1], covidregress$coefficients[2], col='blue', lwd=2) 
predictions<-predict(covidregress, covidTestingDeveloped)
View(predictions)

###Logistic Regression with Prediction and Validation###
covidpredict<- lm(covid.hd$`Mortality Rate`~covid.hd$Obesity+covid.hd$Undernourished+covid.hd$`GDP Per Capita`+covid.hd$Meat_fat+covid.hd$`Alcoholic Beverages_kcal`+covid.hd$`Developing Country Binary`+covid.hd$`Starchy Roots_fat`, covidTrainingDeveloping)
predictions<- predict(covidpredict, covidTestingDeveloped)
View(predictions)
predictions[is.na(predictions)] = 0
RMSE=sqrt(sum((predictions-covidTestingDeveloped$`Mortality Rate`)^2)/(length(covidTestingDeveloped$`Mortality Rate`-11)))
View(RMSE)

t###Data Partitioning###
covidTrainingDeveloping<-subset(covid.hd, covid.hd$`Developing Country`=='Y')
covidTestingDeveloped<-subset(covid.hd, covid.hd$`Developing Country`=='N')

###Logistic Regression###
covidregress2<-glm(covid.hd$`Developing Country Binary` ~ covid.hd$Obesity+covid.hd$Undernourished +covid.hd$`Alcoholic Beverages_kcal`, data = covidTrainingDeveloping, family = "binomial")
summary(covidregress2)
signal<-predict(covidregress2, covid.hd)
pred_prob<-(1/(1+exp(-signal)))
View(pred_prob)
View(predict(covidregress2, covid.hd, type="response"))
exp(cbind(covidregress2$coefficients, confint(covidregress2)))
confusionMatrix(table(predict(covidregress2, covidTrainingDeveloping, type="response") >= 0.5, covidTrainingDeveloping$`Developing Country Binary` == 1))






















