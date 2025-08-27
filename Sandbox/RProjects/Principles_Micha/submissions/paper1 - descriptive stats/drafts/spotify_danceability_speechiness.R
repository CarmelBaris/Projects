# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Preparing the data
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 

# read the data
getwd()
# ls()
# list.dirs()
# setwd("C:/Users/user/Documents/R_Language/Stats_Principles")
songs <- read.csv("./data/mine/spotify1921to2020.csv")
dance <- songs$danceability
speech <- songs$speechiness

# preview variables
total.origin <- length(dance)
head(dance)
head(speech)
summary(speech)
summary(dance)

# only danceable songs from years 2017 and 2018
keep <- (dance>0 & songs$year > 2016 & songs$year < 2019)
dance <- dance[keep]
speech <- speech[keep]
total.origin - length(dance) # number of songs removed


# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Analysis
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 

summary(dance)
hist(dance, breaks = 50)
mean(dance)

summary(speech)
hist(speech, breaks = 50)
mean(speech)

# Apply logarithm to "zoom-in" histogram view
log.speech <- log(speech)/sd(speech)
hist(log.speech, breaks = 50)

# Scatter plot (be patient, may run slowly):
plot(speech,dance,pch=20) # pch determines type of graphical markers
abline(v=mean(speech),col="blue") #v=speechiness values
abline(h=mean(dance),col="purple") #h=danceability values

#Observing the plot, we assume a positive correlation
cor <- cor(speech, dance)
cov <- cov(speech, dance)

#Linear regression line:
fit<-lm(dance ~ speech)
summary(fit)
abline(fit,col="red")

# the fit object includes info about regression such as
# coefficients, residuals, fitted values. access with fit$ & tab
fit$coefficients
fit$residuals
sum(fit$residuals)
fit$fitted.values
sum(fit$fitted.values-dance)

lm(formula = dance ~ speech)
a.origin<-fit$coefficients[2]
b.origin<-fit$coefficients[1]
a <- a.origin
b <- b.origin

#Question: What's the danceability prediction for speechiness=0.02?
#Answer:
a*0.02+b

#Question: What's the danceability prediction for speechiness=0.9?
a*0.9+b

#Question: What is the meaning of the slope?
a

#Question: According to the regression line, 
# what will be the difference in the prediction
# between two songs with 0.1 and 0.5 speechiness, respectively?
(0.5-0.1)*a

#Question: same as above
# between two songs with 0.5 and 0.9 speechiness, respectively?
(0.9-0.5)*a

#Question: What is the meaning of the intercept?
b

#Question:Is a linear regression line without intercept passes through the averages point? 
#a1 is the slope of a regression without intercept
a1=mean(speech*dance)/mean(speech*speech)
#Linear regression line:
abline(0,a1,col="green")






# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Internal Tests
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 


# Validations, expect True in all tests
cor.test <- cov(speech, dance)/(sd(speech)*sd(dance))
trunc(cor.test) == trunc(cor)

a.test <- speech.dance.cov/var(speech)
b.test <- mean(dance)-a*mean(speech)
trunc(a.origin) == trunc(a.test)
trunc(b.origin) == trunc(b.test)
