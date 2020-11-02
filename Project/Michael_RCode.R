#Data Preprocessing
library(tidyverse)

#Read csv files
setwd("~/Documents/UW/2020Winter/INDE546_InferentialDataAnalysis/Project/Dataset")
data_2014 <- read.csv("2014_Financial_Data.csv", header = TRUE)
data_2015 <- read.csv("2015_Financial_Data.csv", header = TRUE)
data_2016 <- read.csv("2016_Financial_Data.csv", header = TRUE)
data_2017 <- read.csv("2017_Financial_Data.csv", header = TRUE)
data_2018 <- read.csv("2018_Financial_Data.csv", header = TRUE)


#data_2014 <- data_2014 %>% mutate(Year = 2014)
#data_2014 <- data_2014[, colnames(data_2014)[c(1, 226, 2:225)]]

#data_2015 <- data_2015 %>% mutate(Year = 2015)
#data_2015 <- data_2015[, colnames(data_2015)[c(1, 226, 2:225)]]

#data_2016 <- data_2016 %>% mutate(Year = 2016)
#data_2016 <- data_2016[, colnames(data_2016)[c(1, 226, 2:225)]]

#data_2017 <- data_2017 %>% mutate(Year = 2017)
#data_2017 <- data_2017[, colnames(data_2017)[c(1, 226, 2:225)]]

#data_2018 <- data_2018 %>% mutate(Year = 2018)
#data_2018 <- data_2018[, colnames(data_2018)[c(1, 226, 2:225)]]

#rename company names based on the year the data was collected
names <- c()
for (i in c(1:nrow(data_2014))){
  name <- paste(data_2014$X[i], "_2014", sep = "")
  names <- c(names, name)
}
data_2014$X <- names

names <- c()
for (i in c(1:nrow(data_2015))){
  name <- paste(data_2015$X[i], "_2015", sep = "")
  names <- c(names, name)
}
data_2015$X <- names

names <- c()
for (i in c(1:nrow(data_2016))){
  name <- paste(data_2016$X[i], "_2016", sep = "")
  names <- c(names, name)
}
data_2016$X <- names

names <- c()
for (i in c(1:nrow(data_2017))){
  name <- paste(data_2017$X[i], "_2017", sep = "")
  names <- c(names, name)
}
data_2017$X <- names

names <- c()
for (i in c(1:nrow(data_2018))){
  name <- paste(data_2018$X[i], "_2018", sep = "")
  names <- c(names, name)
}
data_2018$X <- names

#Rename an variable that has different name but indicates the same index for all sperated datasets
data_2014 <- data_2014 %>% rename(NextYearPriceVar = X2015.PRICE.VAR....)
data_2015 <- data_2015 %>% rename(NextYearPriceVar = X2016.PRICE.VAR....)
data_2016 <- data_2016 %>% rename(NextYearPriceVar = X2017.PRICE.VAR....)
data_2017 <- data_2017 %>% rename(NextYearPriceVar = X2018.PRICE.VAR....)
data_2018 <- data_2018 %>% rename(NextYearPriceVar = X2019.PRICE.VAR....)

#Combine dataset and a quick manipulation
data <- rbind(data_2014, data_2015, data_2016, data_2017, data_2018)
data <- data %>% filter(Revenue >= 0, R.D.Expenses >= 0)
data$Class <- as.factor(data$Class)

#Unit transformaton (Dollar -> Million Dollar)
million = 1000000

for (j in c(2:ncol(data))){
  na <- 0
  for (i in c(1:nrow(data))){
    if (is.na(data[i, j]) == FALSE & is.numeric(data[i, j]) == TRUE){
      if (abs(data[i, j]) > million){
        if (data[i, j] >= 0){data[i, j] = data[i, j]/million} else{data[i, j] = data[i, j]/(-million)}
      }
    } else{if (is.na(data[i, j]) == TRUE){na <- na + 1}}
  }
}

#Create New Variables to group the companies into 4 same-size groups 
quan = quantile(data$Revenue)
data <- data %>% mutate(Revenue.Group = case_when((Revenue <= quan[5] & Revenue > quan[4]) ~ "4th Quantile", (Revenue <= quan[4] & Revenue > quan[3]) ~ "3rd Quantile", (Revenue <= quan[3] & Revenue > quan[2]) ~ "2nd Quantile", Revenue <= quan[2] ~ "1st Quantile"))

#Save csv file
write.csv(data, "~/Documents/UW/2020Winter/INDE546_InferentialDataAnalysis/Project/Dataset/ProcessedDataset.csv")


#Existing Model Code
lm.existing <- lm(RD.OI.Ratio ~ Revenue + Revenue.Growth + Total.shareholders.equity + returnOnEquity + returnOnAssets + Net.Income, data = data_final)
#summary(lm.existing)



#LASSO Code
library(glmnet)
data_final <- read.csv("Dataset/FinalDataset.csv", header = TRUE)

#Remove categorical variable and NA values in our dependent variable
data_final <- data_final[,c(-1,-222,-223, -224)]
data_final <- data_final %>% drop_na(RD.OI.Ratio)

#Use glmnet function to perform LASSO
fit = glmnet(as.matrix(data_final[,-220]), as.matrix(data_final[,220]), family=c("gaussian"))
cv.fit = cv.glmnet(as.matrix(data_final[,-220]), as.matrix(data_final[,220]))
plot(cv.fit)

#Selected best model and the corresponding coefficients
#cv.fit$lambda.min is the best lambda value that results in the best model with smallest mean-squared error
cv.fit$lambda.min 
#This extracts the fitted regression parameters of the linear regression model using this lambda value.
coef(cv.fit, s = "lambda.min")

#Re-fit the regression model with selected variables by LASSO
var_idx <- which(coef(cv.fit, s = "lambda.min") != 0)
lm.reduced <- lm(RD.OI.Ratio ~ ., data = data_final[,c(var_idx, 220)])
#summary(lm.reduced.test)

#Eliminate insignificant variables
lm.test2 <- lm(RD.OI.Ratio ~ ., data =  data_final[,c(1,6,7,8,28,36,37,38,105,220)])
lm.test3 <- lm(RD.OI.Ratio ~ ., data =  data_final[,c(1,6,7,36,37,38,105,220)])
#summary(lm.test3)

#Create presentable tables and residual plots
confint(lm.test3)
table <- cbind(summary(lm.test3)$coefficient, confint(lm.test3))
par(mfrow = c(2, 2))
plot(lm.test3)




