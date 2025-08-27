library(readxl)
#Q1
sal.a <- 5
sal.b <- 4
sal.x <- 45
Q1 <- sal.a*sal.x + sal.b # y.predicted = y.hat = a*x+b

#Q3 Find correlation between SAT and BA:
SAT <- c(691, 731, 572, 578 ,694, 734 ,693 ,672,
         589 ,738 ,568, 635, 565, 669, 587)
BA<-c(20,95,84,59,99,64,75,79,85,90,94,78,79,56,47)
Q3 <- cor(SAT, BA)

#Q4 Display linear regression:
plot(SAT,BA,col='blue',pch=20,cex=1.5)
lm(formula = BA ~ SAT)
Q4.intercept <- lm(formula = BA ~ SAT)$coefficients[1]
Q5.growth.coefficient <- lm(formula = BA ~ SAT)$coefficients[2]
abline(lm(BA ~ SAT),0.0049,lwd=2)


#Q5
Q5.growth.coefficient <- lm(formula = BA ~ SAT)$coefficients[2]

# Q6: 
grade.a <- 2.17
SD.midterm <- 5
r.cor <- 0.75
Q6 <- (grade.a*SD.midterm)/r.cor

#Q7
#Calculate (y.hat - y) for specific observation i
# where y.hat is predicted value of y, based on coefficients of regression line.
#Find (y.hat - y) <=> sum(all remainders) where remainder is distance from mean.
#We used a fitted linear model, i.e. the line that passes through all means,
# and ensures equal distances of observed values to mean value.
#Consequently, there's an equal sum of distances above the line (positive)
# and below the line (negative). Summing the two results in zero.
#For this reason, the sum of all distances/remainders is zero by definition.
Q7 <- 0

# Q8: In what predicted year will the women's record be equals to the men's?
#Load data
olympic_data <- read_excel("C:/Users/user/Documents/Carmel/RProjects/Principles_Micha/data/lessons/Olympic_data.xlsx")
year <- olympic_data$`Olympic year`

#Women's regression line
women <- olympic_data$`Women's winning time (s)`
women.fit <- lm(formula = women ~ year)
women.fit$coefficients[2] #approximate/truncated slope of prediction
women.fit$coefficients[1] #approximate/truncated intersect of prediction
women.a <- summary(women.fit)$coefficients[2,1] #precise slope of prediction
women.b <- summary(women.fit)$coefficients[1,1] #precise intersect of prediction

#Men's regression line
men <- olympic_data$`Men's winning time (s)`
men.fit <- lm(formula = men ~ year)
men.fit$coefficients[2] #truncated slope of prediction
men.fit$coefficients[1] #truncated intersect of prediction
men.a <- summary(men.fit)$coefficients[2,1] #precise slope of prediction
men.b <- summary(men.fit)$coefficients[1,1] #precise intersect of prediction

#Seeing as both men records & women records "share" the `year` parameter,
# the researchers wrongly assumed that by finding the intersection between
# the predicted values of the two separate linear models,
# they'd find the year for which the following equation holds true:
# best record for Olympic men == best record for Olympic women

#In other words, their poorly defined research question was 
# find year where predicted.men = predicted.women
# =>find x where y.hat.men = y.hat.women
# =>find x where men.a*x+men.b = women.a*x+women.b
x=(women.b-men.b)/(men.a-women.a)
Q8 <- x
#Their core assumption was wrong because it's misleading to compare data from 
# entirely different contexts, diff runners and in diff competitions.
#This defeats the concept of a linear `progress` throughout a given time series.



#Q10 Based on given data, calculate monthly state revenue from tax.

#Given data
# tax.rate = 0.2
# ttl.workers = 180
#  Y <- income, per individual
#  X <- monthly work hrs, per individual
#  Y = 30X+1500
#   SD(Y) = 45
#   SD(X) = 45
#   avg(X) = 180 hrs

#Steps
# a. Calculate avg(Y): Y(avg(X)) = 30*avg(X)+1500 = 6900
# b. Calculate avg.tax.per.individual = avg(Y)*tax.rate = 1380
# c. Calculate total tax = avg.tax.per.individual*ttl.workers = 248400