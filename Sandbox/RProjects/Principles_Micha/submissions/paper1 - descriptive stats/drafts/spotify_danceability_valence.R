# ++++++++++++++++++++++++ # 
# PREPARING THE DATA
# ++++++++++++++++++++++++ #


# read the data
getwd()
# ls()
# list.dirs()
# setwd("C:/Users/user/Documents/R_Language/Stats_Principles")
songs <- read.csv("./data/mine/spotify1921to2020.csv")
dance <- songs$danceability
valence <- songs$valence

# preview variables
total.origin <- length(dance)
head(dance)
head(valence)
summary(valence)
summary(dance)

# only danceable songs from years 2017 and 2018
keep <- (dance>0 & songs$year > 2016 & songs$year < 2019)
dance <- dance[keep]
valence <- valence[keep]
total.origin - length(dance) # number of songs removed


# ++++++++++++++++++++++++ # 
# ANALYSIS
# ++++++++++++++++++++++++ #

summary(dance)
hist(dance, breaks = 50)
mean(dance)

summary(valence)
hist(valence, breaks = 50)
mean(valence)

#if the observations follow a Normal distribution, 
# a range covered by 1 standard deviation above the mean & 1 below it
#   includes about 68% of the observations; 
# a range of 2 standard deviations above and 2 below 
#   includes about 95% of the observations; 
# and of 3 standard deviations above and 3 below
#   includes about 99.7% of the observations.
sd1 <- sum(abs(valence-mean(valence))/sd(valence)<=1 & abs(dance-mean(dance))/sd(dance)<=1)
sd2 <- sum(abs(valence-mean(valence))/sd(valence)<=2 & abs(dance-mean(dance))/sd(dance)<=2)
sd3 <- sum(abs(valence-mean(valence))/sd(valence)<=3 & abs(dance-mean(dance))/sd(dance)<=3)

# Scatter plot (be patient, may run slowly):
plot(valence,dance,pch=20) # pch determines type of graphical markers
abline(v=mean(valence),col="blue") #v=valence values
abline(h=mean(dance),col="purple") #h=danceability values

#Observing the plot, we assume a positive correlation
cor <- cor(valence, dance)
cov <- cov(valence, dance)

#Linear regression line:
fit<-lm(dance ~ valence)
summary(fit)
abline(fit,col="red")

# the fit object includes info about regression such as
# coefficients, residuals, fitted values. access with fit$ & tab
fit$coefficients
fit$residuals
sum(fit$residuals)
fit$fitted.values
sum(fit$fitted.values-dance)

lm(formula = dance ~ valence)
a.origin<-fit$coefficients[2]
b.origin<-fit$coefficients[1]
a <- a.origin
b <- b.origin

#Question: What's the danceability prediction for valence=0.02?
#Answer:
a*0.02+b

#Question: What's the danceability prediction for valence=0.9?
a*0.9+b

#Question: What is the meaning of the slope?
a

#Question: According to the regression line, 
# what will be the difference in the prediction
# between two songs with 0.1 and 0.5 valence, respectively?
(0.5-0.1)*a

#Question: same as above
# between two songs with 0.5 and 0.9 valence, respectively?
(0.9-0.5)*a

#Question: What is the meaning of the intercept?
b

#Question:Is a linear regression line without intercept passes through the averages point? 
#a1 is the slope of a regression without intercept
a1=mean(valence*dance)/mean(valence*valence)
#Linear regression line:
abline(0,a1,col="green")






# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Internal Tests
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 


# Validations, expect True in all tests
cor.test <- cov(valence, dance)/(sd(valence)*sd(dance))
trunc(cor.test) == trunc(cor)

a.test <- valence.dance.cov/var(valence)
b.test <- mean(dance)-a*mean(valence)
trunc(a.origin) == trunc(a.test)
trunc(b.origin) == trunc(b.test)
