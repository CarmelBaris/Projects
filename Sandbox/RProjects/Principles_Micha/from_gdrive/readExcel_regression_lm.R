library("readxl")
data <- read_excel("C:/Users/PC/Downloads/Olympic_data.xlsx")
summary(lm(data$colName_Y~data$colName_X))