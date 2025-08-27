# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Preparing the data
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 

# read the data
getwd()
# ls()
# list.dirs()
setwd("C:/Users/user/Documents/R_Language/Principles_Micha")
songs <- read.csv("./data/mine/spotify1921to2020.csv")
dance <- songs$danceability
tempo <- songs$tempo

# preview variables
total.origin <- length(dance)
head(dance)
head(tempo)
summary(tempo)
summary(dance)

# only danceable songs from years 2017 and 2018
keep <- (dance>0 & songs$year > 2018 & songs$year < 2021)
dance <- dance[keep]
tempo <- tempo[keep]
total.origin - length(dance) # number of songs removed


# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Analysis
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 

summary(dance)
hist(dance, breaks = 50)
mean(dance)
var(dance)
sd(dance)

summary(tempo)
hist(tempo, breaks = 50)
mean(tempo)
var(tempo)
sd(tempo)

#Observing the plot, we assume a positive correlation
cor <- cor(tempo, dance, method = c("pearson"))
cov(tempo, dance, method = c("pearson"))
cor <- cor(tempo, dance)
cov <- cov(tempo, dance)

#Linear regression line:
fit<-lm(dance ~ tempo)
summary(fit)

# the fit object includes info about regression such as
# coefficients, residuals, fitted values. access with fit$ & tab
fit$coefficients
fit$residuals
sum(fit$residuals)
fit$fitted.values
sum(fit$fitted.values-dance)

a.origin<-fit$coefficients[2]
b.origin<-fit$coefficients[1]
a <- a.origin
b <- b.origin

# Scatter plot (be patient, may run slowly):
plot(fit, col="gray", pch=20) # pch determines type of graphical markers

abline(v=mean(tempo),col="blue") #v=temponess values
abline(h=mean(dance),col="purple") #h=danceability values


abline(fit,col="red")


#Question: What's the danceability prediction for tempo of=0.02 BPM?
#Answer:
a*0.02+b

#Question: What's the danceability prediction for tempo of =0.9 BPM?
a*0.9+b

#Question: What is the meaning of the slope?
a

#Question: According to the regression line, 
# what will be the difference in the prediction
# between two songs with 0.1 BPM and 0.5 BPM tempo levels, respectively?
(0.5-0.1)*a

#Question: same as above
# between two songs with 0.5 BPM and 0.9 BPM tempo levels, respectively?
(0.9-0.5)*a

#Question: What is the meaning of the intercept?
b

#Question:Is a linear regression line without intercept passes through the averages point? 
#a1 is the slope of a regression without intercept
a1=mean(tempo*dance)/mean(tempo*tempo)
#Linear regression line:
abline(0,a1,col="green")


# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 
# Internal Tests
# +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # +++++++ # 


# Validations, expect True in all tests
cor.test <- cov(tempo, dance)/(sd(tempo)*sd(dance))
trunc(cor.test) == trunc(cor)

a.test <- cov/var(tempo)
b.test <- mean(dance)-a*mean(tempo)
trunc(a.origin) == trunc(a.test)
trunc(b.origin) == trunc(b.test)

