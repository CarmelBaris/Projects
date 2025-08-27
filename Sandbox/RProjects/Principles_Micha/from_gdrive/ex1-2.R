Age = c(53,43,33,45,46,55,41,55,36,45,55,50,49,47,69,51,48,62,45,37,50,50,50,58,53,57,53,61,47,56,
        44,46,58,48,38,74,60,32,51,50,40,61,63,56,45,61,70,59,57,69,44,56,50,56,43,48,52,62,48)
x=c(2,4,6)
y=c(1,3,5)
mean(Age)
median(Age)
frequency(Age)
table((Age))
cov(x,y)
names((sort(summary(as.factor(Age), decreasing=F)[1:3])))
hist(Age)
football<-read.csv("~/R_Language/Stats_Principles/data/firstgoalclean.csv")
quantile(Age)      
lm()
x=c(90, 68, 66, 62, 70, 62, 67, 66, 64, 64, 57, 59, 63, 57, 52,59, 53, 52, 49, 55,88, 88, 90, 88, 86, 82, 80, 82, 72, 78, 79, 75, 74, 79, 75, 69, 68, 69, 66, 74)
y=c(99, 69, 69, 69, 68, 68, 67, 65, 64, 63, 60, 60, 60, 59, 57, 56, 54, 54, 51, 49,92, 92, 92, 87, 84, 84, 83,82, 81, 80, 77, 77, 77, 77, 76, 71, 71, 71, 70, 70)
summary(lm(y~x))
library("readxl")
data <- read_excel("C:/Users/PC/Downloads/Olympic_data.xlsx")
summary(lm(data$M~data$Y))
summary(lm(data$W~data$Y))
print((40.543225-31.5398334)/(0.014878-0.0108579))



# ++++++++

Sal = c(145,621,262,208,362,424,339,736,291,58,498,643,390,332,750,368,659,234,396,300,343,536,543,217,298,1103,406,254,862, 204,206,250,21,298,350,800,726,370,536,291,808,543,149,350,242,198,
213,296,317,482,155,802,200,282,573,388,250,396,572)

# Measures of Center for Salary
# A measure of central tendency (measure of center) is a value that attempts to describe a set of data by identifying the central position of the data set (as representative of a "typical" value in the set).
# Unfortunately, measures of central tendency alone may be insufficient to truly describe the typical data in a set. It is possible that two data sets can have the same mean, but be very different kinds of sets. It is best to use measures of central tendency, along with other observations of the data set, to best describe the data set.

mean(Sal)
median(Sal)

# Standard Deviation for Salary
sd(Sal)
abs.dif = abs(Sal-mean(Sal)) # for each in Sal, calculate its dist from the mean
abs.dif
sd.dif = abs.dif/sd(Sal) # normalized/standardized distances
sd.dif
sum(sd.dif <= 1) # count of 
sum(sd.dif <= 2)
