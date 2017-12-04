library(ggplot2)
library(readxl)

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