install.packages("corrplot")
install.packages("ggplot2")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("caTools")
install.packages("rattle")
install.packages("gridExtra")
install.packages("ROCR")
install.packages("randomForest")
install.packages("randomForestSRC")
install.packages("reshape2")
install.packages("RColorBrewer")
library(readxl)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(e1071)
library(caTools)
library(rattle)
library(gridExtra)
library(ROCR)
library(ipred)
library(gbm)
library(C50)
library(randomForest)
library(randomForestSRC)
library(reshape2)
library(RColorBrewer)

# read_excel reads both xls and xlsx files
read_excel("my-old-spreadsheet.xls")
read_excel("my-new-spreadsheet.xlsx")

# Specify sheet with a number or name
read_excel("my-spreadsheet.xls", sheet = "data")
read_excel("my-spreadsheet.xls", sheet = 2)

# If NAs are represented by something other than blank cells,
# set the na argument
read_excel("my-spreadsheet.xls", na = "NA")

data <- read_excel("empatt.xlsx")

dim(data)
str(data)
colSums(sapply(data, is.na))

summary(data)
names(data)

qplot(x = JobSatisfaction, data = data)

qplot(x = JobLevel, data = data)

qplot(x = MonthlyIncome, data = data) +
  scale_x_continuous(breaks = c(1,1000,5000,7500,10000,12500,15000,20000)) +
  facet_wrap(~JobLevel, ncol = 3)

qplot(x = MonthlyIncome, data = data) +
  scale_x_continuous(breaks = c(0,5000,10000,15000,20000)) +
  facet_wrap(~Gender, ncol = 3)

qplot(x = MonthlyIncome, data = data) +
  scale_x_continuous(breaks = c(0,5000,10000,15000,20000)) +
  facet_grid(JobLevel~Gender)

qplot(x = DistanceFromHome, data = data)

binary.model <- rpart(Attrition~., data=data, cp=.02)
rpart.plot(binary.model)
fallen.leaves <- "false"