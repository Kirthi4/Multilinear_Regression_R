install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("DataExplorer")
install.packages("ggplot2")

library(ggplot2)
library(tidyverse) 
library(funModeling) 
library(Hmisc)
library(DataExplorer)

#Non-airline Revenue
setwd('C:\\Users\\Kirti Nagori\\OneDrive\\Desktop\\Stats for BA\\HW5')
getwd()

revenue = read.csv('DEN 2012 - Jun 17-1 (1).csv')
revenue

colnames(revenue)

summary(revenue)
glimpse(revenue)
profiling_num(revenue)

summary(revenue$Concession)
summary(revenue$Parking)
summary(revenue$Rental.Car)
summary(revenue$Ground)

#Pairs plot
pairs(revenue)


#Graphs for all the 4 non-airline revenue for all the years
revenue$Month <- factor(revenue$Month,levels=month.abb)

ggplot2::
  ggplot(revenue, aes(revenue$Month, revenue$Concession)) + geom_bar(stat = "identity", fill = "blue")
ggplot(revenue, aes(revenue$Month, revenue$Parking)) + geom_bar(stat = "identity", fill = "blue")
ggplot(revenue, aes(revenue$Month, revenue$Rental.Car)) + geom_bar(stat = "identity", fill = "blue")
ggplot(revenue, aes(revenue$Month, revenue$Ground)) + geom_bar(stat = "identity", fill = "blue")


boxplot(revenue$Concession, revenue$Parking, revenue$Rental.Car, revenue$Ground)
boxplot(revenue$Parking)
boxplot(revenue$Rental.Car)
boxplot(revenue$Ground)


library(stringr)
year = str_sub(revenue$Month.and.Year, end=-5)

#Added a new column "year" which includes only the year information 
revenue$year = year
revenue


#Outlier
summary(revenue$Concession)
profiling_num(revenue$Concession)
boxplot(revenue$Concession)



#Year wise visualisations of all the 4 non-airline revenues:
#Year 2012-------------------------------------------------------------------------------------------------------------------------------
year_12 = revenue%>%
  filter(year=="12")

#Concession
profiling_num(year_12$Concession)
summary(year_12$Concession)
boxplot(revenue$Concession)
ggplot(year_12, aes(year_12$Month, year_12$Concession)) + geom_bar(stat = "identity", fill = "orange")

#Parking
profiling_num(year_12$Parking)
summary(year_12$Parking)
boxplot(revenue$Parking)
ggplot(year_12, aes(year_12$Month, year_12$Parking)) + geom_bar(stat = "identity", fill = "orange")

#Rental car
profiling_num(year_12$Rental.Car)
summary(year_12$Rental.Car)
boxplot(revenue$Rental.Car)
ggplot(year_12, aes(year_12$Month, year_12$Rental.Car)) + geom_bar(stat = "identity", fill = "orange")

#Ground
profiling_num(year_12$Ground)
summary(year_12$Ground)
boxplot(revenue$Ground)
ggplot(year_12, aes(year_12$Month, year_12$Ground)) + geom_bar(stat = "identity", fill = "orange")


#Year 2013-------------------------------------------------------------------------------------------------------------------------------
year_13 = revenue%>%
  filter(year=="13")

#Concession
profiling_num(year_13$Concession)
summary(year_13$Concession)
ggplot(year_13, aes(year_13$Month, year_13$Concession)) + geom_bar(stat = "identity", fill = "orange")

#Parking
profiling_num(year_13$Parking)
summary(year_13$Parking)
ggplot(year_13, aes(year_13$Month, year_13$Parking)) + geom_bar(stat = "identity", fill = "orange")

#Rental car
profiling_num(year_13$Rental.Car)
summary(year_13$Rental.Car)
ggplot(year_13, aes(year_13$Month, year_13$Rental.Car)) + geom_bar(stat = "identity", fill = "orange")

#Ground
profiling_num(year_13$Ground)
summary(year_13$Ground)
ggplot(year_13, aes(year_13$Month, year_13$Ground)) + geom_bar(stat = "identity", fill = "orange")


#Year 2014-------------------------------------------------------------------------------------------------------------------------------
year_14 = revenue%>%
  filter(year=="14")

#Concession
profiling_num(year_14$Concession)
summary(year_14$Concession)
ggplot(year_14, aes(year_14$Month, year_14$Concession)) + geom_bar(stat = "identity", fill = "orange")

#Parking
profiling_num(year_14$Parking)
summary(year_14$Parking)
ggplot(year_14, aes(year_14$Month, year_14$Parking)) + geom_bar(stat = "identity", fill = "orange")

#Rental car
profiling_num(year_14$Rental.Car)
summary(year_14$Rental.Car)
ggplot(year_14, aes(year_14$Month, year_14$Rental.Car)) + geom_bar(stat = "identity", fill = "orange")

#Ground
profiling_num(year_14$Ground)
summary(year_14$Ground)
ggplot(year_14, aes(year_14$Month, year_14$Ground)) + geom_bar(stat = "identity", fill = "orange")


#Year 2015-------------------------------------------------------------------------------------------------------------------------------
year_15 = revenue%>%
  filter(year=="15")

#Concession
profiling_num(year_15$Concession)
summary(year_15$Concession)
ggplot(year_15, aes(year_15$Month, year_15$Concession)) + geom_bar(stat = "identity", fill = "orange")

#Parking
profiling_num(year_15$Parking)
summary(year_15$Parking)
ggplot(year_15, aes(year_15$Month, year_15$Parking)) + geom_bar(stat = "identity", fill = "orange")

#Rental car
profiling_num(year_15$Rental.Car)
summary(year_15$Rental.Car)
ggplot(year_15, aes(year_15$Month, year_15$Rental.Car)) + geom_bar(stat = "identity", fill = "orange")

#Ground
profiling_num(year_15$Ground)
summary(year_15$Ground)
ggplot(year_15, aes(year_15$Month, year_15$Ground)) + geom_bar(stat = "identity", fill = "orange")


#Year 2016-------------------------------------------------------------------------------------------------------------------------------
year_16 = revenue%>%
  filter(year=="16")

#Concession
profiling_num(year_16$Concession)
summary(year_16$Concession)
ggplot(year_16, aes(year_16$Month, year_16$Concession)) + geom_bar(stat = "identity", fill = "orange")

#Parking
profiling_num(year_16$Parking)
summary(year_16$Parking)
ggplot(year_16, aes(year_16$Month, year_16$Parking)) + geom_bar(stat = "identity", fill = "orange")

#Rental car
profiling_num(year_16$Rental.Car)
summary(year_16$Rental.Car)
ggplot(year_16, aes(year_16$Month, year_16$Rental.Car)) + geom_bar(stat = "identity", fill = "orange")

#Ground
profiling_num(year_16$Ground)
summary(year_16$Ground)
ggplot(year_16, aes(year_16$Month, year_16$Ground)) + geom_bar(stat = "identity", fill = "orange")





#Creating Df for the required columns:-------------------------------------------------------------------------------------------------------------------------------


revenue_df <- revenue[, c(3,4,5,6,7,8,9,10,11,12,15,21,22)]
colnames(revenue)


#Correlation Matrix
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(revenue_df, histogram=TRUE, pch=19)


install.packages("corrplot")
library(corrplot)
corr_matrix = cor(revenue_df); corr_matrix
corrplot(corr_matrix, method="circle", order = "hclust", addrect = 1)


library(GGally)
ggpairs(revenue_df)



  

#Concession:

#Setting training dataset:
n<-dim(revenue)[1]
n
revenue_train<-revenue[3:(n-4),]
dim(revenue_train)

revenue_test = revenue[63:66,]
revenue_cross_verify = revenue[10:15,]


rentalCar_a1 = revenue_train$Rental.Car
enplaned_a2 = revenue_train$Enplaned
deplaned_a3 = revenue_train$Deplaned
transfer_a4 = revenue_train$Transfer
originating_a5 = revenue_train$Originating
desination_a6 = revenue_train$Destination
origin_destin_a7 = revenue_train$Origin...Destin
concession_a8 = revenue_train$Concession
parking_a9 = revenue_train$Parking
ground_a10 = revenue_train$Ground
umcsent_a11 = revenue_train$UMCSENT
cannabis_12 = revenue_train$Cannabis.
month_a13 = revenue_train$Month
umcsent_lag1_a14 = revenue_train$UMCSENTLag1
umcsent_lag2_a15 = revenue_train$UMCSENTLag2
umcsent_lag3_a16 = revenue_train$UMCSENTLag3

concession_model = lm(concession_a8 ~ enplaned_a2+deplaned_a3+transfer_a4+originating_a5+desination_a6+origin_destin_a7+parking_a9+rentalCar_a1+ground_a10+umcsent_a11+
                        month_a13+cannabis_12+umcsent_lag1_a14+umcsent_lag2_a15)
summary(concession_model)
anova(concession_model)

#Using AIC to predict the x variables
library(MASS)
step = stepAIC(concession_model)
step$anova

#Building the model based on the independent variables screened by the stepAIC:
a17 = (deplaned_a3**4)
con_aic = lm(concession_a8 ~ a17+umcsent_lag2_a15+month_a13)
summary(con_aic)

#Testing the Assumptions:
#Finding the VIF Values: It checks the collinerity between the independent variables
library(car)
vif(con_aic)

plot(con_aic)
residualPlots(con_aic)

library(car)
durbinWatsonTest(con_aic)

AIC(con_aic)
BIC(con_aic)

#As the p value is greater than 0.05, the null hypothesis is accepted; indicating that the residuals are not 
#correlated, and no evidence of violation of the assumption of randomness and independence.


#Testing the model:

pred_con = predict(con_aic, data.frame(a17 = revenue_test$Deplaned**4, month_a13 = revenue_test$Month, umcsent_lag2_a15 = revenue_test$UMCSENTLag2))
pred_con

revenue_test$Concession

total_predicted_concession = sum(pred_con); total_predicted_concession
total_actual_concession = sum(revenue_test$Concession); total_actual_concession

error_concession_percentage = ((total_actual_concession - total_predicted_concession)/total_actual_concession)*100
error_concession_percentage

#Month wise prediction:
revenue_test$Concession

error_con_mar = abs(((revenue_test$Concession[1] - pred_con[1])/revenue_test$Concession[1])*100); error_con_mar
error_con_apr = abs(((revenue_test$Concession[2] - pred_con[2])/revenue_test$Concession[2])*100); error_con_apr
error_con_may = abs(((revenue_test$Concession[3] - pred_con[3])/revenue_test$Concession[3])*100); error_con_may
error_con_jun = abs(((revenue_test$Concession[4] - pred_con[4])/revenue_test$Concession[4])*100); error_con_jun



#Cross-validation: Testing the model on some other set of rows from the dataset.
pred_cross_val = predict(con_aic, data.frame(a17 = revenue_cross_verify$Deplaned**4, month_a13 = revenue_cross_verify$Month, umcsent_lag2_a15 = revenue_cross_verify$UMCSENTLag2))
pred_cross_val

total_cv_pre_con = sum(pred_cross_val); total_cv_pre_con
total_cv_act_con = sum(revenue_cross_verify$Concession); total_cv_act_con

error_cv_con = ((total_cv_act_con - total_cv_pre_con)/total_cv_act_con)*100
error_cv_con
  



  

#Parking

#Setting training dataset:
n<-dim(revenue)[1]
n
revenue_train<-revenue[1:(n-4),]
dim(revenue_train)

revenue_test = revenue[63:66,]
revenue_cross_verify = revenue[10:15,]

umcsent_lag1_a14 = revenue_train$UMCSENTLag1
umcsent_lag2_a15 = revenue_train$UMCSENTLag2
umcsent_lag3_a16 = revenue_train$UMCSENTLag3
rentalCar_a1 = revenue_train$Rental.Car
enplaned_a2 = revenue_train$Enplaned
deplaned_a3 = revenue_train$Deplaned
transfer_a4 = revenue_train$Transfer
originating_a5 = revenue_train$Originating
desination_a6 = revenue_train$Destination
origin_destin_a7 = revenue_train$Origin...Destin
concession_a8 = revenue_train$Concession
parking_a9 = revenue_train$Parking
ground_a10 = revenue_train$Ground
umcsent_a11 = revenue_train$UMCSENT
cannabis_12 = revenue_train$Cannabis.
month_a13 = revenue_train$Month
gdp_a19 = revenue_train$Monthly.Real.GDP.Index


parking_model = lm(parking_a9 ~ enplaned_a2+deplaned_a3+transfer_a4+originating_a5+desination_a6+origin_destin_a7+
                     concession_a8+rentalCar_a1+ground_a10+umcsent_a11+cannabis_12+month_a13)
summary(parking_model)

#Using AIC to predict the x variables
step_park = stepAIC(parking_model)
step_park$anova

#Building the model based on the independent variables screened by the stepAIC:

enplaned_transfer_train = log((revenue_train$Enplaned - revenue_train$Transfer))

parking_aic = lm(parking_a9 ~ month_a13+enplaned_transfer_train)
summary(parking_aic)

#Testing the Assumptions:
#Finding the VIF Values: It checks the collinerity between the independent variables
library(car)
vif(parking_aic)


AIC(parking_aic)
BIC(parking_aic)


plot(parking_aic)
residualPlots(parking_aic)

durbinWatsonTest(parking_aic)
#As the p value is greater than 0.05, the null hypothesis is accepted; indicating that the residuals are not 
#correlated, and no evidence of violation of the assumption of randomness and independence.


#Testing the model:
enplaned_transfer_test = log(revenue_test$Enplaned - revenue_test$Transfer)
pred_park = predict(parking_aic, data.frame(month_a13=revenue_test$Month, enplaned_transfer_train=enplaned_transfer_test))
pred_park

revenue_test$Parking

total_predicted_parking = sum(pred_park); total_predicted_parking
total_actual_parking = sum(revenue_test$Parking); total_actual_parking

error_parking_percentage = ((total_actual_parking - total_predicted_parking)/total_actual_parking)*100
error_parking_percentage

#Month wise prediction:

error_park_mar = abs(((revenue_test$Parking[1] - pred_park[1])/revenue_test$Parking[1])*100); error_park_mar
error_park_apr = abs(((revenue_test$Parking[2] - pred_park[2])/revenue_test$Parking[2])*100); error_park_apr
error_park_may = abs(((revenue_test$Parking[3] - pred_park[3])/revenue_test$Parking[3])*100); error_park_may
error_park_jun = abs(((revenue_test$Parking[4] - pred_park[4])/revenue_test$Parking[4])*100); error_park_jun




#Cross-validation: Testing the model on some other set of rows from the dataset.
enplaned_transfer_cv = log(revenue_cross_verify$Enplaned - revenue_cross_verify$Transfer)
pred_cross_val_park = predict(parking_aic, data.frame(month_a13=revenue_cross_verify$Month, enplaned_transfer_train=enplaned_transfer_cv))
pred_cross_val_park

total_cv_pre_park = sum(pred_cross_val_park); total_cv_pre_park
total_cv_act_park = sum(revenue_cross_verify$Parking); total_cv_act_park

error_cv_park = ((total_cv_act_park - total_cv_pre_park)/total_cv_act_park)*100
error_cv_park





  

    
#Rental Car
  
#Setting training dataset:
n<-dim(revenue)[1]
n
revenue_train<-revenue[1:(n-4),]
dim(revenue_train)

revenue_test = revenue[63:66,]
revenue_cross_verify = revenue[10:15,]

rentalCar_a1 = revenue_train$Rental.Car
enplaned_a2 = revenue_train$Enplaned
deplaned_a3 = revenue_train$Deplaned
transfer_a4 = revenue_train$Transfer
originating_a5 = revenue_train$Originating
desination_a6 = revenue_train$Destination
origin_destin_a7 = revenue_train$Origin...Destin
concession_a8 = revenue_train$Concession
parking_a9 = revenue_train$Parking
ground_a10 = revenue_train$Ground
umcsent_a11 = revenue_train$UMCSENT
cannabis_12 = revenue_train$Cannabis.
month_a13 = revenue_train$Month
gdp_a19 = revenue_train$Monthly.Real.GDP.Index

rentalCar_model = lm(rentalCar_a1 ~ enplaned_a2+deplaned_a3+transfer_a4+originating_a5+desination_a6+origin_destin_a7+
                       concession_a8+parking_a9+ground_a10+umcsent_a11+cannabis_12+month_a13+gdp_a19)
summary(rentalCar_model)

#Using AIC to predict the x variables
step_rCar = stepAIC(rentalCar_model)
step_rCar$anova

#Building the model based on the independent variables screened by the stepAIC:

rCar_aic = lm(rentalCar_a1 ~ concession_a8+ground_a10+parking_a9+cannabis_12+month_a13)
summary(rCar_aic)

#Testing the Assumptions:
#Finding the VIF Values: It checks the collinerity between the independent variables
library(car)
vif(rCar_aic)


AIC(rCar_aic)
BIC(rCar_aic)

plot(rCar_aic)
residualPlots(rCar_aic)

durbinWatsonTest(rCar_aic)
#As the p value is greater than 0.05, the null hypothesis is accepted; indicating that the residuals are not 
#correlated, and no evidence of violation of the assumption of randomness and independence.


#Testing the model
pred_rentalCar = predict(rCar_aic, data.frame(concession_a8=revenue_test$Concession, ground_a10=revenue_test$Ground, parking_a9=revenue_test$Parking, 
                                              cannabis_12=revenue_test$Cannabis., month_a13=revenue_test$Month))
pred_rentalCar

revenue_test$Rental.Car

total_predicted_rCar = sum(pred_rentalCar); total_predicted_rCar
total_actual_rCar = sum(revenue_test$Rental.Car); total_actual_rCar

error_rCar_percentage = ((total_actual_rCar - total_predicted_rCar)/total_actual_rCar)*100
error_rCar_percentage

#Month wise prediction:

error_rCar_mar = abs(((revenue_test$Rental.Car[1] - pred_rentalCar[1])/revenue_test$Rental.Car[1])*100); error_rCar_mar
error_rCar_apr = abs(((revenue_test$Rental.Car[2] - pred_rentalCar[2])/revenue_test$Rental.Car[2])*100); error_rCar_apr
error_rCar_may = abs(((revenue_test$Rental.Car[3] - pred_rentalCar[3])/revenue_test$Rental.Car[3])*100); error_rCar_may
error_rCar_jun = abs(((revenue_test$Rental.Car[4] - pred_rentalCar[4])/revenue_test$Rental.Car[4])*100); error_rCar_jun


#Cross-validation: Testing the model on some other set of rows from the dataset.

pred_cross_val_rCar = predict(rCar_aic, data.frame(concession_a8=revenue_cross_verify$Concession, ground_a10=revenue_cross_verify$Ground, parking_a9=revenue_cross_verify$Parking, cannabis_12=revenue_cross_verify$Cannabis., month_a13=revenue_cross_verify$Month))
pred_cross_val_rCar

total_cv_pre_rCar = sum(pred_cross_val_rCar); total_cv_pre_rCar
total_cv_act_rCar = sum(revenue_cross_verify$Rental.Car); total_cv_act_rCar

error_cv_rCar = ((total_cv_act_rCar - total_cv_pre_rCar)/total_cv_act_rCar)*100
error_cv_rCar






#Ground

#Setting training dataset:
n<-dim(revenue)[1]
n
revenue_train<-revenue[4:(n-4),]
dim(revenue_train)

revenue_test = revenue[63:66,]
revenue_cross_verify = revenue[10:15,]
  

rentalCar_a1 = revenue_train$Rental.Car
enplaned_a2 = revenue_train$Enplaned
deplaned_a3 = revenue_train$Deplaned
transfer_a4 = revenue_train$Transfer
originating_a5 = revenue_train$Originating
desination_a6 = revenue_train$Destination
origin_destin_a7 = revenue_train$Origin...Destin
concession_a8 = revenue_train$Concession
parking_a9 = revenue_train$Parking
ground_a10 = revenue_train$Ground
umcsent_a11 = revenue_train$UMCSENT
cannabis_12 = revenue_train$Cannabis.
month_a13 = revenue_train$Month
umcsent_lag1_a14 = revenue_train$UMCSENTLag1
umcsent_lag2_a15 = revenue_train$UMCSENTLag2
umcsent_lag3_a16 = revenue_train$UMCSENTLag3
parking_lag1_a17 = revenue_train$Parking.Lag1
parking_lag2_a18 = revenue_train$Parking.Lag2
parking_lag3_a19 = revenue_train$Parking.Lag3
gdp_a19 = revenue_train$Monthly.Real.GDP.Index
  
  
ground_model = lm(ground_a10 ~ rentalCar_a1+enplaned_a2+deplaned_a3+transfer_a4+originating_a5+desination_a6+origin_destin_a7+
                    concession_a8+parking_a9+umcsent_a11+cannabis_12+month_a13+umcsent_lag1_a14+umcsent_lag2_a15+parking_lag1_a17+parking_lag2_a18+parking_lag3_a19)
summary(ground_model)

#Using AIC to predict the x variables
step_ground = stepAIC(ground_model)
step_ground$anova


#Building the model based on the independent variables screened by the stepAIC:

parking_lag_transform = parking_lag2_a18**2+parking_a9**4+parking_lag3_a19+parking_lag1_a17**3
origin_uncsent_transform = (log(originating_a5) + log(umcsent_lag2_a15**0.4))


ground_aic = lm(ground_a10 ~ parking_lag_transform+month_a13+origin_uncsent_transform)
summary(ground_aic)

#Testing the Assumptions:
#Finding the VIF Values: It checks the collinerity between the independent variables
library(car)
vif(ground_aic)

AIC(ground_aic)
BIC(ground_aic)

plot(ground_aic)
residualPlots(ground_aic)

durbinWatsonTest(ground_aic)
#As the p value is greater than 0.05, the null hypothesis is accepted; indicating that the residuals are not 
#correlated, and no evidence of violation of the assumption of randomness and independence.


#Testing the model

b1 = revenue_test$Parking.Lag2**2+revenue_test$Parking**4+revenue_test$Parking.Lag3+revenue_test$Parking.Lag1**3
b2 = log(revenue_test$Originating)+log(revenue_test$UMCSENTLag2**0.4)

pred_ground = predict(ground_aic, data.frame(parking_lag_transform=b1, month_a13=revenue_test$Month, origin_uncsent_transform=b2))
pred_ground

revenue_test$Ground

total_predicted_ground = sum(pred_ground); total_predicted_ground
total_actual_ground = sum(revenue_test$Ground); total_actual_ground

error_ground_percentage = ((total_actual_ground - total_predicted_ground)/total_actual_ground)*100
error_ground_percentage

#Month wise prediction:

error_ground_mar = abs(((revenue_test$Ground[1] - pred_ground[1])/revenue_test$Ground[1])*100); error_ground_mar
error_ground_apr = abs(((revenue_test$Ground[2] - pred_ground[2])/revenue_test$Ground[2])*100); error_ground_apr
error_ground_may = abs(((revenue_test$Ground[3] - pred_ground[3])/revenue_test$Ground[3])*100); error_ground_may
error_ground_jun = abs(((revenue_test$Ground[4] - pred_ground[4])/revenue_test$Ground[4])*100); error_ground_jun


#Cross-validation: Testing the model on some other set of rows from the dataset.

b8 =revenue_cross_verify$Parking.Lag2**2+revenue_cross_verify$Parking**4+revenue_cross_verify$Parking.Lag3+revenue_cross_verify$Parking.Lag1**3
b9 =log(revenue_cross_verify$Originating)+log(revenue_cross_verify$UMCSENTLag2**0.4)

pred_cross_val_ground = predict(ground_aic, data.frame(parking_lag_transform=b8, month_a13=revenue_cross_verify$Month, origin_uncsent_transform=b9))
pred_cross_val_ground

total_cv_pre_ground = sum(pred_cross_val_ground); total_cv_pre_ground
total_cv_act_ground = sum(revenue_cross_verify$Ground); total_cv_act_ground

error_cv_rCar = ((total_cv_act_ground - total_cv_pre_ground)/total_cv_act_ground)*100
error_cv_rCar





#Total Revenue

#Setting training dataset:
n<-dim(revenue)[1]
n
revenue_train<-revenue[1:(n-4),]
dim(revenue_train)

revenue_test = revenue[63:66,]
revenue_cross_verify = revenue[10:15,]

rentalCar_a1 = revenue_train$Rental.Car
enplaned_a2 = revenue_train$Enplaned
deplaned_a3 = revenue_train$Deplaned
transfer_a4 = revenue_train$Transfer
originating_a5 = revenue_train$Originating
desination_a6 = revenue_train$Destination
origin_destin_a7 = revenue_train$Origin...Destin
concession_a8 = revenue_train$Concession
parking_a9 = revenue_train$Parking
ground_a10 = revenue_train$Ground
umcsent_a11 = revenue_train$UMCSENT
cannabis_12 = revenue_train$Cannabis.
month_a13 = revenue_train$Month
total_revenue_a20 = revenue_train$Total.Revenue
gdp_a19 = revenue_train$Monthly.Real.GDP.Index


total_revenue = lm(total_revenue_a20 ~ enplaned_a2+deplaned_a3+originating_a5+desination_a6+origin_destin_a7+umcsent_a11+
                     cannabis_12+month_a13+concession_a8+parking_a9+ground_a10+rentalCar_a1+gdp_a19)
summary(total_revenue)

#Using AIC to predict the x variables
library(MASS)
step_totalRevenue = stepAIC(total_revenue)
step_totalRevenue$anova


#Building the model based on the independent variables screened by the stepAIC:


originating_sqr = (originating_a5)**0.5
gdp = ((gdp_a19) - mean(gdp_a19))**2
rentalCar = ((rentalCar_a1 - mean(rentalCar_a1))**2)


total_revenue_aic = lm(total_revenue_a20 ~ month_a13+originating_sqr+gdp+rentalCar)
summary(total_revenue_aic)

library(car)
vif(total_revenue_aic)

AIC(total_revenue_aic)
BIC(total_revenue_aic)

plot(total_revenue_aic)
residualPlots(total_revenue_aic)

durbinWatsonTest(total_revenue_aic)
#As the p value is greater than 0.05, the null hypothesis is accepted; indicating that the residuals are not 
#correlated, and no evidence of violation of the assumption of randomness and independence.


#Testing the model

c2 = (revenue_test$Originating)**0.5
c3 = (revenue_test$Monthly.Real.GDP.Index - mean(revenue_test$Monthly.Real.GDP.Index))**2
c4 = ((revenue_test$Rental.Car - mean(revenue_test$Rental.Car))**2)

pred_total_revenue = predict(total_revenue_aic, data.frame(month_a13 = revenue_test$Month, originating_sqr = c2, gdp = c3, rentalCar = c4))
pred_total_revenue

revenue_test$Total.Revenue

total_predicted_total_revenue = sum(pred_total_revenue); total_predicted_total_revenue
total_actual_total_revenue = sum(revenue_test$Total.Revenue); total_actual_total_revenue

error_total_revenue_percentage = ((total_actual_total_revenue - total_predicted_total_revenue)/total_actual_total_revenue)*100
error_total_revenue_percentage

#Month wise prediction:

error_total_revenue_mar = abs(((revenue_test$Total.Revenue[1] - pred_total_revenue[1])/revenue_test$Total.Revenue[1])*100); error_total_revenue_mar
error_total_revenue_apr = abs(((revenue_test$Total.Revenue[2] - pred_total_revenue[2])/revenue_test$Total.Revenue[2])*100); error_total_revenue_apr
error_total_revenue_may = abs(((revenue_test$Total.Revenue[3] - pred_total_revenue[3])/revenue_test$Total.Revenue[3])*100); error_total_revenue_may
error_total_revenue_jun = abs(((revenue_test$Total.Revenue[4] - pred_total_revenue[4])/revenue_test$Total.Revenue[4])*100); error_total_revenue_jun


