# Data on head circumference of embryos 
# Data are based on real data, but random noise was added to observations

# read the data and correct values 
getwd()
ls()
list.dirs()
setwd("..")
setwd("C:/Users/user/Documents/R_Language/Stats_Principles")
dat <- read.csv("./data/lessons/HC_only.csv")
head(dat)
pregweek <- dat$week
HC <- dat$HC
HC[HC<60] <- HC[HC<60]*10
keep <- (HC>0 & pregweek<43) # define a logical condition
pregweek <- pregweek[keep]
HC <- HC[keep]

# prediction HC in week 30
HC30 <- HC[floor(pregweek)==30] # retrieve index based on logical condition
hist(HC30,freq=F,breaks=100,xlim=c(230,330))
mean(HC30)
round(quantile(HC30,c(0.03,0.05,0.10,0.95,0.99)),2)


#prediction all weeks
# apply the mean function, but separately for each week
week <- floor(pregweek)
table(week)
mean.by.week <- by(HC, week, mean)
q5.by.week <- by(HC, week, quantile, probs=0.05)
q95.by.week <- by(HC, week, quantile, probs=0.95)

plot(14:42,mean.by.week,pch=20,xlab='Week',ylab='HC',
     ylim=c(min(q5.by.week),max(q95.by.week)))
points(14:42,q5.by.week,pch='-',col='blue')
points(14:42,q95.by.week,pch='-',col='blue')

# regression
plot(pregweek,HC,pch=20,cex=0.2)
abline(v=30)
abline(lm(HC ~ pregweek),lwd=2,col="blue")

# calculating the regression coefficients
a <- cov(HC,pregweek)/var(pregweek)
b <- mean(HC)-a*mean(pregweek) # the mean(HC/pregweek) are numerical values
c(a,b) # the מגמה is that for every week that passes, the HC grows by 8.7555772

# Built-in function in R called linear model (lm for short):
lm(HC ~ pregweek) # HC distributed by pregweek
lm(formula = HC ~ pregweek) # intercept (with y axis) is the b value
y.hat <- b+a*pregweek # the predicted value of HC, by pregweek (y)
e <- HC - y.hat # the remainder, the diff btw predicted val and the observation

# now we can create a new plot, that displays the correlation
# btw the observations and the predicted values:
plot(pregweek,y.hat,pch=20,cex=0.5)  # pch defines the dots, cex is x-axis steps
plot(pregweek,y.hat,pch=20,cex=0.5, type = "l")  # pch defines the dots, cex is x-axis steps
plot(pregweek,e,pch=20,cex=0.3)

cor(pregweek,y.hat)
cor(pregweek,e)
# conclusion: there is no linear relation between the observations
# we still have a relation, but no "first-rate" relation i.e. linear relation
# Micha chose a relationship in which the linear relation isn't correct
# bc we need to remember that the model isn't accurate, and requires improvement


# transformation: manipulation of data s.t. better fits the linear model
log.week <- log(pregweek)
plot(week, HC)
plot(log.week, HC)
reg.log.week <- lm(HC ~ log.week) # we put the regression func into an object
# that includes the residuals, the remainders
abline(reg.log.week, lwd=2, col="blue")
plot(log.week, reg.log.week$residuals) # within our new object, 
# we're calling the residuals attribute
# because we're looking for the correlation btw the log of the week & residuals
# result: correlation is 0, the linear regression neither grows nor decreases

# now it's easier to "fit" the data into a linear correlation

# but doesn't the transformation change the meaning?
# no, because the HC remained the same, 
# and we changed the relation btw the subgroups
# instead of 1 week, we're looking at 1 log of a week




# remember: each dot in the plot is only the mean of that week
# i.e it signifies a week that has an entire distribution

# the joint plot aids in understanding the sub-populations
# where the ttl population is preg women in the weeks 30-42


# if we divide it by days, the observations become smaller and it's harder
# to conclude about the entire population


# maybe we can take info from week 29 (w29) and w31, and enrich the info on w30

# regression: takes the means from all days, and assumes a linear connection
# visually: imagine a straight line that cuts exactly all dots in our plot
# often, we'll see observations (dots) that remain outside
# therefore, we might need to think of a different function (non-linear)
# but right now, we'll choose the simplified linear and we'll "smooth" it out

# simplified method: 
# purpose: prediction of an average
# Advantage: Histogram is limited to a single week only vs linear representation
# instead of looking at all observations, we can represent it only by 2 params:
# a,b where Y=aX+b
# moreover, the linear format takes into account adjacent weeks,
# assuming a "smooth" transition between the observations






# We might be interested more in the quantiles, and not in the means
