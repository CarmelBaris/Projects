# read the cdi_gdp and correct values 
getwd()
ls()
list.dirs()
setwd("..")
setwd("C:/Users/user/Documents/R_Language/Stats_Principles")

#Import the text file CIP_GDP
# install.packages("readxl")
library("readxl")
cdi_gdp <- read_excel("./data/tirguls/CIP_GDP.xlsx")
attach(cdi_gdp) # make columns accessible by simply giving their names
x<-cdi_gdp$CPI #consumer price index
y<-cdi_gdp$GDP #gross domestic product

#week 2 week 2 week 2 week 2 week 2 week 2 week 2 week 2 week 2 week 2
#week 2 week 2 week 2 week 2 week 2 week 2 week 2 week 2 week 2 week 2

n<-length(x)
n
summary(cdi_gdp)
hist(x)
summary(x)
hist(y)
summary(y)
mean(x)
mean(y)

#Scatter plot:
plot(x,y,pch=20) #`pch` determines which markers to display in the plot
abline(v=mean(x),col="blue") #v=x values
abline(h=mean(y),col="blue") #h=y values

#Question: By observing the plot, can you guess the sign of the correlation?
cov(x,y) 


cor(x,y) #The correlation between x and y.
cov(x,y)/(sd(x)*sd(y))
#Same value for both formulas

#Linear regression line:
fit<-lm(formula = y ~ x)
summary(fit)
abline(fit,col="red")

# the fit object include information about the regression such as
# coefficients, residuals, fitted values. we can reach them by fit$ & tab
fit$coefficients
fit$residuals
sum(fit$residuals)
fit$fitted.values
sum(fit$fitted.values-y)

a <- cov(x,y)/var(x)
b <- mean(y)-a*mean(x)
a
b

lm(formula = y ~ x)
a<-fit$coefficients[2]
b<-fit$coefficients[1]
#Question: What is the prediction for x=6?
#Answer:
a*6+b
#Question: What the prediction for x=11
a*11+b

#Question: What is the meaning of the slope?
a

#Question: based on the regression line, 
#What will be the difference in the prediction
# between two countries with 3 and 5 CPI?
#Answer:
2*a
#Question: Same question, but with ranks 1 and 3
#Answer:
2*a

#Question: What is the meaning of the intercept?

#Question: Without the intercept `b`,
# does the linear regression line
# pass through the averages' points? 
#a1 is the slope of a regression without intercept `b`
a1=mean(x*y)/mean(x*x)


#Linear regression line:
abline(0,a1,col="green")

#week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3
#week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3 week 3


#Question: find the R-squared
cdi_gdp$y.hat = b+a*x
cdi_gdp$e <- y-cdi_gdp$y.hat
R.2 <- var(cdi_gdp$y.hat)/var(y)
R.2


#Use summary command to see R^2, under the name "Multiple R-squared"
summary(lm(y~x))

# When we have only one explanatory variable, R^2 equals the squared correlation 
(cor(x,y))^2


#Question:what is the RMSE ? which country has the largest standardized residual?
summary(lm(y~x))

cdi_gdp$e1=fit$residuals
RMSE=sqrt(sum(cdi_gdp$e^2)/(n-2))
RMSE
#To display the countries & their standardized residuals, use `cbind` command
cdi_gdp$Country=as.character(cdi_gdp$Country)
cdi_gdp$z.e<-cdi_gdp$e/RMSE

cbind(cdi_gdp$Country,cdi_gdp$e/RMSE)
which.max(cdi_gdp$e/RMSE)
#the observation with the largest standardized residual
# is far from the regression line
plot(x,y,pch=20)
abline(lm(y ~ x),col="red")
points(cdi_gdp[91,2:3],pch=22,col="red")

#Question: What happens if we divide GDP by 1000?
y<-y/1000
a.new <- cov(x,y)/var(x)
b.new<- mean(y)-a.new*mean(x)
a/1000
a.new
b/1000
b.new

plot(x,y,pch=20)
abline(lm(y ~ x),col="red")

dat<-cdi_gdp[-91,]