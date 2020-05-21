#Setting working directory
setwd("C:/Users/suyom/Desktop/ECON 386 project")

getwd()
#Importing data
data<-read.csv("Full.Dataset.Revised.csv")
data
View(data)
summary(data)


#####Data cleaning
#Change variable data that is zero to NA
data[data == 0.0000]<-NA
#Make sure dependent variable data that is 0, isn't changed to NA
data$Developing.Country.Binary[is.na(data$Developing.Country.Binary)] = 0
data$Undernourished[is.na(data$Undernourished)] = 0
data$Confirmed[is.na(data$Confirmed)] = 0
data$Deaths[is.na(data$Deaths)] = 0
data$Obesity[is.na(data$Obesity)] = 0
data$Pulses_fat[is.na(data$Pulses_fat)] = 0

# check which columns have an unreasonable amount of NA values or very low Medians/Max's
summary(data)    

data$Aquatic.Products..Other_fat<-NULL         # 152 NA's
data$Aquatic.Products..Other_kcal<-NULL        # 154 NA's
data$Aquatic.Products..Other_kg<-NULL          # 92 NA's
data$Aquatic.Products..Other_protein<-NULL     # 134 NA's

data$Sugar.Crops_fat<-NULL                     # 146 NA's
data$Sugar.Crops_kcal<-NULL                    # 138 NA's
data$Sugar.Crops_kg<-NULL                      # 130 NA's
data$Sugar.Crops_protein<-NULL                 # 137 NA's

data$Sugar.Sweeteners_fat<-NULL              # 149 NA's
data$Sugar.Sweeteners_protein<-NULL          # 95 NA's

data$Miscellaneous_fat<-NULL                   # Mean = 0.059, Max = 0.45 and 24 NA
data$Miscellaneous_kcal<-NULL                  # Mean = 0.164, Max = 1.18 and 24 NA
data$Miscellaneous_kg<-NULL                    # Mean = 0.41, Max = 3.66 and 15 NA
data$Miscellaneous_protein<-NULL               # Mean = 0.184, Max = 1.13 and 19 NA

data$Offals_fat<-NULL                          # Mean = 0.154 and Max = 0.73
data$Offals_kcal<-NULL                         # Mean = 0.145 and Max = 0.8
data$Offals_kg<-NULL                           # Mean = 0.196 and Max = 1.23

data$Treenuts_kcal<-NULL                       # Mean = 0.314, Max = 1.42 and 21 NA
data$Treenuts_kg<-NULL                         # Mean = 0.131, Max = 0.76 and 11 NA
data$Treenuts_protein<-NULL                    # Mean = 0.283, Max = 1.89 and 16 NA

data$Spices_fat<-NULL                          # Mean = 0.297, Max = 2.69 and 12 NA
data$Spices_kcal<-NULL                         # Mean = 0.201, Max = 1.22 and 19 NA
data$Spices_kg<-NULL                           # Mean = 0.091 and Max = 0.663
data$Spices_protein<-NULL                      # Mean = 0.249, Max = 1.86 and 11 NA

data$Alcoholic.Beverages_fat<-NULL             # 154 NA's
data$Animal.fats_kg<-NULL                      # Mean = 0.233 and Max = 1.36
data$Stimulants_kg<-NULL                       # Mean = 0.21 and Max = 1.28
data$Alcoholic.Beverages_protein<-NULL         # Mean = 0.295, Max = 1.37 and 14 NA
data$Animal.fats_protein<-NULL                 # Mean = 0.115, Max = 0.98 and 7 NA
data$Vegetable.Oils_protein<-NULL              # Mean = 0.027, Max = 0.114 and 35 NA

View(data)

#' The First regression model: determine Mortality.Rate with animal consuming 
#' clean data to create a dataframe cotaining following information:
#' independent var: Mortality.Rate
#' dependent var: Animal.fats_kcal
#'                Animal.Products_fat
#'                Animal.fats_fat
#'                Animal.Products_kcal
#'                Animal.Products_kg
#'                Animal.Products_protein

animal_data_str = c("Mortality.Rate", "Animal.Products_fat", "Animal.fats_fat",
                   "Animal.Products_kcal", "Animal.Products_kg", "Animal.Products_protein")
animal_data = data[,animal_data_str ]

# cleaning NA data
na.omit(animal_data)

# Regression Model 1
animal_reg = lm(Mortality.Rate ~ Animal.Products_fat+Animal.fats_fat+
                Animal.Products_kcal+Animal.Products_kg+Animal.Products_protein, animal_data)


# Model 1 analysis
summary(animal_reg)
plot(animal_reg)
plot(animal_reg$residuals)
abline(0,0,col='black', lwd = 4)
hist(animal_reg$residuals, prob = TRUE)
curve(dnorm(x, mean = mean(animal_reg$residuals), sd = sd(animal_reg$residuals)), col = "red", lwd = 2, add=TRUE)
library(tseries) #loads "tseries" library - need to first install "tseries" package
#conducts a hypothesis test for normality called the Jarque-Bera test
jarque.bera.test(animal_reg$residuals) #null hypothesis: data is distribution is normal


#' The Second regression model: determine Mortality.Rate with consuming kcal
#' clean data to create a dataframe cotaining following information:
#' independent var: Mortality.Rate
#' dependent var: Animal.Products_kcal
#'                Cereals._kcal
#'                Fish.Seafood_kcal
#'                Meat_kcal
#'                Oilcrops_kcal
#'                Starchy.Roots_kcal
#'                Sugar.Sweeteners_kcal
#'                Vegetable.Oils_kcal
#'                Alcoholic.Beverages_kcal
#'                Animal.fats_kcal
#'                Eggs_kcal
#'                Fruits_kcal
#'                Milk_kcal
#'                Pulses_kcal
#'                Stimulants_kcal
#'                Vegetal.Products_kcal
#'                Vegetables_kcal

kcal_data_str = c("Animal.Products_kcal","Cereals._kcal","Fish.Seafood_kcal",
                  "Meat_kcal", "Oilcrops_kcal", "Starchy.Roots_kcal", "Sugar.Sweeteners_kcal",
                  "Vegetable.Oils_kcal", "Alcoholic.Beverages_kcal", "Animal.fats_kcal", "Eggs_kcal",
                  "Fruits_kcal", "Milk_kcal", "Pulses_kcal", "Stimulants_kcal", "Vegetal.Products_kcal"
                  , "Vegetables_kcal", "Mortality.Rate")
          
kcal_data = data[,kcal_data_str]

# cleaning NA data
na.omit(kcal_data)

# Regression Model 2

kcal_reg = lm(Mortality.Rate ~ Animal.Products_kcal+Cereals._kcal+Fish.Seafood_kcal+
                  Meat_kcal+Oilcrops_kcal+Starchy.Roots_kcal+Sugar.Sweeteners_kcal+
                  Vegetable.Oils_kcal+Alcoholic.Beverages_kcal+Animal.fats_kcal+Eggs_kcal+
                  Fruits_kcal+Milk_kcal+Pulses_kcal+Stimulants_kcal+Vegetal.Products_kcal+
                  Vegetables_kcal, kcal_data)
# Model 2 analysis
summary(kcal_reg)
plot(kcal_reg)
plot(kcal_reg$residuals)
abline(0,0,col='black', lwd = 4)
hist(kcal_reg$residuals, prob = TRUE)
curve(dnorm(x, mean = mean(kcal_reg$residuals), sd = sd(kcal_reg$residuals)), col = "green", lwd = 2, add=TRUE)
jarque.bera.test(kcal_reg$residuals) #null hypothesis: data is distribution is normal



#########################Classification Task################################

data$Developing.Country.Binary <- factor(data$Developing.Country.Binary)

View(data)

#########################PARTITIONING DATA##################################
set.seed(1234) 

inTrain <- createDataPartition(y=data$Developing.Country.Binary, p=.70, list = FALSE) 

Training<-data[inTrain,]  

Testing<-data[-inTrain,]  

#########################LOGISTIC REGRESSION################################
log_reg<-glm(Developing.Country.Binary ~ Animal.Products_fat+Animal.fats_fat+
               Animal.Products_kcal+Animal.Products_kg+Animal.Products_protein, data = Training, family = "binomial")
summary(log_reg)

exp(cbind(log_reg$coefficients, confint(log_reg)))

confusionMatrix(table(predict(log_reg, Testing, type="response") >= 0.5,
                      Testing$Developing.Country.Binary == 1))

#------ Accuracy : 0.8043 ------#

#########################Prediction and Validation##########################


model.prediction <- lm(Mortality.Rate ~ Animal.Products_fat+Animal.fats_fat+
                         Animal.Products_kcal+Animal.Products_kg+Animal.Products_protein, Training)
summary(model.prediction)

predictions<-predict(model.prediction, Testing)
View(predictions)

RMSE(predictions, Testing$Mortality.Rate, na.rm=T)
RMSE 
#------ Out of Sample Error of 5.407452 ------#