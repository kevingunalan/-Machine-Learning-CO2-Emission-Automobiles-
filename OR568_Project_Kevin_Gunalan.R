library(moments)   #for skewness and kurtosis
library(dplyr)
library(corrr) #Focus function
library(ppcor) #Correlation Analysis
library(rcompanion) #CramerV correlation analysis
library(mice) #Imputation
library(splitTools) #Train, Valid, Test Split
library(DescTools) #PairApply Dataframe - CramersV
library(tidyr) #Drop_na
library(Hmisc) #Histogram for Dataframe
library(randomForest)
library(EnvStats) #Boxcox transformation
library(MASS)
library(sae) #Inverse Boxcox
library(superml) #GridSearchCV
library(caret)
library(earth) #MARS model
library(ggplot2) #Visualization
library(plotly) #Interactive Plot
library(stats) #AIC

#Setting up working directory
setwd("C:/Users/kevin.gunalan/OneDrive - George Mason University - O365 Production/Desktop/Desktop Total/MS in DAE/Fall 2021/OR 568/Project")

#Importing the data set
df <- read.csv("data.csv",na.strings=c(""," ", "  ","NA"))

# assigning new names to the columns of the data frame
colnames(df) <- c('ID', 'Country','Vehicle#','Manufacturer','Manufacturer_EU','Manufacturer_OEM', 'Registry_Name','Approval_Number', 
                  'Type','Variant', 'Version', 'Make', 'Commercial_Name','Vehicle_Category', 'Register_Category',
                  'New_Registrations', 'Mass_Kg', 'Test_Mass','Enedc_g.km','Ewltp_g.km','Wheel_Base_mm', 
                  'Axle_Width_mm', 'Axle_Width_Other_mm', 'Fuel_Type','Fuel_Mode', 'Engine_Capacity_cm3','Engine_Power_KW'
                  ,'Electric_Consumption_Wh.Km','Innovative_Tech', 'Ernedc_g.Km','Erwltp_g.Km', 'Deviation_Factor', 
                  'Verification_Factor', 'Status', 'Year','Electric_Range_Km')

#Extracting only Bulgaria data
bg <- df[df$Country == "BG",]
bg <- bg[order(bg$Approval_Number),]
bg <- bg[- c(1:8), ]
bg$Fuel_Type <- toupper(bg$Fuel_Type)

bar1 <- ggplot(bg, aes(x = Year, y = (Enedc_g.km)/1000000, fill = Fuel_Type)) +
  geom_bar(stat = "identity") +
  xlab("Year") + 
  ylab("Total Emissions in millions g/km") +
  guides(fill=guide_legend(title="Fuel Type"))+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold", colour = "Black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"), 
        axis.text = element_text(face = "bold"),
        legend.text = element_text(size = 9, face = "bold"), 
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "lightblue", size = 0.5, linetype = "solid",
                                         colour = "darkblue"))
ggplotly(bar1)

bar2 <- ggplot(data = subset(bg, Fuel_Type == c("PETROL", "DIESEL")), aes(x = Mass_Kg, y = Enedc_g.km, fill = Fuel_Type)) +
  geom_point(stat = "identity") +
  xlab("Car Mass (Kg)") + 
  ylab("Total Emissions (g/km)") +
  guides(fill=guide_legend(title="Fuel Type"))+
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold", colour = "Black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"), 
        axis.text = element_text(face = "bold"),
        legend.text = element_text(size = 9, face = "bold"), 
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "lightblue", size = 0.5, linetype = "solid",
                                         colour = "darkblue"))
ggplotly(bar2)


#Remove column Country from the data frame
remove_cou <- subset(bg, select = -c(Country))


#Replacing Null string with NA
remove_cou[remove_cou == "NULL"] <- NA
row.names(remove_cou) <- NULL

#Check for missing based on year to identify which year has most missing values per variable
year = list(2018,2019,2020)

for (X in year) {
  filter_year <- remove_cou[remove_cou$Year == X,]
  print(X)
  print(sapply(filter_year, function(y) (sum(is.na(y)))/857.32))
  print(nrow(filter_year))
} #There is no difference in missing value based on year

#Checking percentage of missing value per column
sapply(remove_cou, function(x) (sum(is.na(x)))/857.32)

#Dropping the columns with missing value percentage greater than 5-12%
cols_dis = subset(remove_cou, select = -c(`Vehicle#`, Test_Mass, Ewltp_g.km, Electric_Consumption_Wh.Km, Innovative_Tech, Ernedc_g.Km, Erwltp_g.Km, 
                                  Verification_Factor, Deviation_Factor, Electric_Range_Km))


#Remove column "r" from the data set, since the standard deviation is zero. Drop "Id" column as it is redundant, 
#Columns Cr and Status are removed as well because they have less than 2 levels

rem_cols <- subset(cols_dis, select = -c(New_Registrations,ID, Status, Register_Category))

#Verifying if there any columns with high missing value percentage
sapply(rem_cols, function(x) (sum(is.na(x)))/857.32)

#Creating a new dataframe
co2_emissions <- rem_cols
co2_emissions[co2_emissions==0] <- NA

co2_emissions$Approval_Number <- as.character(co2_emissions$Approval_Number)
#Train, Validation, Test set split
set.seed(222)
inds <- partition(co2_emissions$Mass_Kg, p = c(train = 0.6, valid = 0.2, test = 0.2))

train <- co2_emissions[inds$train, ]
valid <- co2_emissions[inds$valid, ]
test <- co2_emissions[inds$test, ]

#Determine correlation for the numeric variables with respect to target variable
train[,c(12:16,19:21)] %>% correlate()

#Removing year column as it has very low correlation and At1 & At2 have multi-collinearity. Select At1 as it has higher
#correlation with target variable
train <- subset(train, select = -c(Year,Axle_Width_Other_mm))
valid <- subset(valid, select = -c(Year,Axle_Width_Other_mm))
test <- subset(test, select = -c(Year,Axle_Width_Other_mm))

#Apply cramerV function to identify multi-col linearity and correlation
multi_col <- PairApply(train[,c('Manufacturer','Manufacturer_EU','Manufacturer_OEM', 'Registry_Name','Approval_Number', 
                                'Type','Variant', 'Version', 'Make', 'Commercial_Name','Vehicle_Category', 
                                'Fuel_Type','Fuel_Mode', "Enedc_g.km")], cramerV, symmetric = TRUE)

#Based on Cramer's value, extremely strong relationship feature pairs are discarded
train <- subset(train, select = -c(Manufacturer, Manufacturer_EU, Manufacturer_OEM, Registry_Name, 
                                   Type, Variant, Version, Make, Vehicle_Category, Fuel_Type))
valid <- subset(valid, select = -c(Manufacturer, Manufacturer_EU, Manufacturer_OEM, Registry_Name, 
                                   Type, Variant, Version, Make, Vehicle_Category, Fuel_Type))
test <- subset(test, select = -c(Manufacturer, Manufacturer_EU, Manufacturer_OEM, Registry_Name, 
                                 Type, Variant, Version, Make, Vehicle_Category, Fuel_Type))

#since only 0.001% of Tan variable is missing, NAs can be omitted.
train <- drop_na(train, c(Approval_Number))
valid <- drop_na(valid, c(Approval_Number))
test <- drop_na(test, c(Approval_Number))

#Imputation using MICE
imputed_train <- mice(train, method = "pmm" ,m = 10, maxit = 5, seed = 254)
imputed_valid <- mice(valid, method = "pmm",m = 20, maxit = 10, seed = 123)
imputed_test <- mice(test, method = "pmm",m = 20, maxit = 10, seed = 321)

#Select one imputated set
train_imp <- complete(imputed_train,5)
valid_imp <- complete(imputed_valid,10)
test_imp <- complete(imputed_test,10)

sapply(train_imp[,c(3:6,8,9)], function(x) skewness(x))

#Before Boxcox transformation
hist(train_imp[,c(3:6,8,9)])

sapply(train_imp[,c(3:6,8,9)], function(x) boxcox(x,optimize = TRUE,lambda = c(-3,3)))

train_imp$Mass_Kg <- bxcx(train_imp$Mass_Kg, lambda = -0.95846)
train_imp$Enedc_g.km <- bxcx(train_imp$Enedc_g.km, lambda = 0.26583)
train_imp$Wheel_Base_mm <- bxcx(train_imp$Wheel_Base_mm, lambda = -1.191)
train_imp$Axle_Width_mm <- bxcx(train_imp$Axle_Width_mm, lambda = -0.8203)
train_imp$Engine_Capacity_cm3 <- bxcx(train_imp$Engine_Capacity_cm3, lambda = -0.65668)
train_imp$Engine_Power_KW <- bxcx(train_imp$Engine_Power_KW, lambda = -0.667)

hist(train_imp[,c(3:6,8,9)])
sapply(train_imp[,c(3:6,8,9)], function(x) skewness(x))

valid_imp$Mass_Kg <- bxcx(valid_imp$Mass_Kg, lambda = -0.95846)
valid_imp$Wheel_Base_mm <- bxcx(valid_imp$Wheel_Base_mm, lambda = -1.191)
valid_imp$Axle_Width_mm <- bxcx(valid_imp$Axle_Width_mm, lambda = -0.8203)
valid_imp$Engine_Capacity_cm3 <- bxcx(valid_imp$Engine_Capacity_cm3, lambda = -0.65668)
valid_imp$Engine_Power_KW <- bxcx(valid_imp$Engine_Power_KW, lambda = -0.667)

test_imp$Mass_Kg <- bxcx(test_imp$Mass_Kg, lambda = -0.95846)
test_imp$Wheel_Base_mm <- bxcx(test_imp$Wheel_Base_mm, lambda = -1.191)
test_imp$Axle_Width_mm <- bxcx(test_imp$Axle_Width_mm, lambda = -0.8203)
test_imp$Engine_Capacity_cm3 <- bxcx(test_imp$Engine_Capacity_cm3, lambda = -0.65668)
test_imp$Engine_Power_KW <- bxcx(test_imp$Engine_Power_KW, lambda = -0.667)

#Simple Muliple Linear Regression
linear <- lm(data = train_imp,formula = Enedc_g.km ~ Mass_Kg + Wheel_Base_mm + Axle_Width_mm + Engine_Capacity_cm3 + Engine_Power_KW + Fuel_Mode)
summary(linear)

#Accuracy for Validation set
lm_pred <- predict(linear, newdata = valid_imp[-c(1,2,4)])
y_pred_lm <- bxcx(lm_pred, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_lm <- data.frame(cbind(actuals=valid_imp$Enedc_g.km, predicteds=y_pred_lm))
correlation_accuracy_lm <- cor(actuals_preds_lm)

#Accuracy for test set
lm_pred_test <- predict(linear, newdata = test_imp[-c(1,2,4)])
y_pred_lm_test <- bxcx(lm_pred_test, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_lm_test <- data.frame(cbind(actuals=test_imp$Enedc_g.km, predicteds=y_pred_lm_test))
correlation_accuracy_lm_test <- cor(actuals_preds_lm_test)

#RMSE for Linear Regression
RMSE(y_pred_lm, valid_imp$Enedc_g.km)
RMSE(y_pred_lm_test, test_imp$Enedc_g.km)

#Random Forest Regression - Takes 10mins to compute
rfm <- randomForest( Enedc_g.km ~ ., data = train_imp, mtry = 3, ntree = 200, importance = TRUE)
summary(rfm)

#Accuracy for Validation set
rfm_pred = predict(rfm, newdata = valid_imp[-4])
y_pred_rfm <- bxcx(rfm_pred, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_rfm <- data.frame(cbind(actuals=valid_imp$Enedc_g.km, predicteds=y_pred_rfm))
correlation_accuracy_rfm <- cor(actuals_preds_rfm)

#Accuracy for Test set
rfm_pred_test = predict(rfm, newdata = test_imp[-4])
y_pred_rfm_test <- bxcx(rfm_pred_test, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_rfm_test <- data.frame(cbind(actuals=test_imp$Enedc_g.km, predicteds=y_pred_rfm_test))
correlation_accuracy_rfm_test <- cor(actuals_preds_rfm_test)

#RMSE for Random Forest Regression
RMSE(valid_imp$Enedc_g.km, y_pred_rfm)
RMSE(y_pred_rfm_test, test_imp$Enedc_g.km)

#Variable Importance Plot
importance(rfm)
varImp(rfm)
varImpPlot(rfm)

#Polynomial Regression
poly <- lm(Enedc_g.km ~ polym(Mass_Kg + Wheel_Base_mm + Axle_Width_mm + Engine_Capacity_cm3 + Engine_Power_KW , degree = 6, raw = TRUE), data = train_imp)
summary(poly)

#Accuracy for Validation set
poly_pred = predict(poly, newdata = valid_imp[-c(1:2,4,7)])
y_pred_poly <- bxcx(poly_pred, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_poly <- data.frame(cbind(actuals=valid_imp$Enedc_g.km, predicteds=y_pred_poly))
correlation_accuracy_poly <- cor(actuals_preds_poly)

#Accuracy for Test set
poly_pred_test = predict(poly, newdata = test_imp[-c(1:2,4,7)])
y_pred_poly_test <- bxcx(poly_pred_test, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_poly_test <- data.frame(cbind(actuals=test_imp$Enedc_g.km, predicteds=y_pred_poly_test))
correlation_accuracy_poly_test <- cor(actuals_preds_poly_test)

#RMSE for Polynomial Regression
RMSE(y_pred_poly, valid_imp$Enedc_g.km, )
RMSE(y_pred_poly_test, test_imp$Enedc_g.km)

#AIC & BIC
AIC(linear, poly)
BIC(linear, poly)

#Multivariate Adaptive Regression Splines
multi <- earth(Enedc_g.km ~ Mass_Kg + Wheel_Base_mm + Axle_Width_mm + Engine_Capacity_cm3 + Engine_Power_KW + Fuel_Mode, data = train_imp, degree = 1, nprune = 23)
summary(multi) %>% .$coefficients %>% head(10)

#Accuracy for Validation set
mars_pred = predict(multi, newdata = valid_imp[-c(1,4)])
y_pred_mars <- bxcx(mars_pred, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_mars<- data.frame(cbind(actuals=valid_imp$Enedc_g.km, predicteds=y_pred_mars))
correlation_accuracy_mars <- cor(actuals_preds_mars)

#Accuracy for Test set
mars_pred_test = predict(multi, newdata = test_imp[-c(1,2,4)])
y_pred_mars_test <- bxcx(mars_pred_test, lambda = 0.26583, InverseQ = TRUE) #Inverse transformation of boxcox
actuals_preds_mars_test<- data.frame(cbind(actuals=test_imp$Enedc_g.km, predicteds=y_pred_mars_test))
correlation_accuracy_mars_test <- cor(actuals_preds_mars_test)

#RMSE for MARS
RMSE(y_pred_mars, valid_imp$Enedc_g.km)
RMSE(y_pred_mars_test, test_imp$Enedc_g.km)

#Hyper-parameter tuning for best degree and number of prunes
hyper_grid <- expand.grid(
  degree = 1:10, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

tuned_mars <- train(
  x = subset(train_imp, select = -c(Enedc_g.km, Approval_Number, Commercial_Name)),
  y = train_imp$Enedc_g.km,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

#View the results
tuned_mars$results
tuned_mars$bestTune
