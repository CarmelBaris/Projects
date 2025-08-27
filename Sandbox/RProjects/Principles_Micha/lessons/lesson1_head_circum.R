# Data is based on real data, but random noise was added to observations
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
back_d <- '../' 
wd <- "~/Users/user/Documents/Carmel/RProjects/"
(setwd(back_d))
(setwd(wd))
(setwd(WD))

?setwd
#loading the file
setwd("~/R_Language/Stats_Principles/data/")
dat <- read.csv("HC_only.csv")
head(dat)

# descriptive: week
pregweek <- dat$week
table(pregweek)
plot(table(pregweek))
table(floor(pregweek))
barplot(table(floor(pregweek)))

# we may decide to remove weeks>= 40
# clearly, the distribution is not uniform - many measures around weeks 22 and 33
# and just before delivery (week 40) or after week 40

# descriptive: HC
HC <- dat$HC
table(HC)
hist(HC)
hist(HC,freq=F)
hist(HC,freq=F,breaks=100)
hist(HC,freq=F,breaks=500)

# Joint
plot(pregweek,HC, type="l")
# problem....different units (mm/cm)
# a fast fix:
?abline
abline(h=60)
HC[HC<60] <- HC[HC<60]*10 # if HC smaller than 60, multiply*10 (cm > mm)
plot(pregweek,HC)
# still many outliers - what can we do?


# week 30
HC30 <- HC[floor(pregweek)==30]

# mean and variance
mean(HC30)
sum(HC30)/length(HC30)
var(HC30)
sum((HC30-mean(HC30))^2)/length(HC30)
sum((HC30-mean(HC30))^2)/(length(HC30)-1)

# plots and other descriptive stats
hist(HC30)
hist(HC30,breaks=100)
hist(HC30,breaks=500)

# empirical cumulative distribution function
ecdf30 <- ecdf(HC30)
ecdf30(270)
ecdf30(280)
ecdf30(c(250,260,270,280,290,300))
plot(ecdf30)

# Quantiles / percentiles
quantile(HC30,0.437)
quantile(HC30,c(0.03,0.05,0.1,0.9,0.95))

# basic statistics
mean(HC30)
median(HC30)
summary(HC30)
summary(dat)
mean30 <- mean(HC30)
med30 <- median(HC30)
mean(abs(HC30-mean30))
mean(abs(HC30-med30))
mean((HC30-mean30)^2)
mean((HC30-med30)^2)
var(HC30)
sd(HC30)


# normalizing
Z.HC30 <- (HC30-mean(HC30))/sd(HC30)
mean(Z.HC30)
sd(Z.HC30)
hist(HC30,freq=F,breaks=200)
hist(Z.HC30,freq=F,breaks=200)

Z.week <- (pregweek-mean(pregweek))/sd(pregweek)
mean(Z.week)
sd(Z.week)
hist(Z.week,freq=F,breaks = 500)
hist(pregweek,freq=F,breaks = 500)
