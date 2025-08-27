# Part A. Elementary R
#
# R is like a calculator
5+3
log(10) #notice that the log function in R is the ln function that we know
4^2   
7*8
sin(3.14159/2)
sqrt(9)
abs(-10)

#function in R:every function has a name ,and the parameters are supplied in parentheses.
#if you aren't sure what parameters to supply just use the tab button!!
log(16,2)
log(10,2)

# (even if a function doesn't require parameters, you must write the parentheses.)


# if you want to save a result, you must assign it a name:
x <- 3+5
y <- exp(1)
# note: the operator = has the same meaning like the operator <- but the common operator in R is  <-
#to print the content of objects, just type their name
x
y

# to create a vector just use the c() function, providing the elements
x<- c(1,3,3,7,9,10)  
y<- c(1:6)
x 

#to access some elements you can provide their "index" withing brackets:
x[2]    # the second element of x
x+2 #this command adds 2 to every elemnt in x 
x[2] <- x[2] + 3
x[2] +3
#R help: a. if you don't know the exact name of the function you can try to type the 
#first letters and use the tab button. 
#b.if you know the name of the function but don't know execly how its work you can type ?
#before the function name:
?plot

x <- c(x,2) # adds new element at end
plot(x,y)
y <- c(y,0)
plot(x,y)
plot(y,x)

#Part B. Elementary statistics

### a note on probability distributions
# for almost any standard probability there is a sampling mechanism in R. 
# for each such distribution there are 4 useful sub-functions.
# e.g for NORMAL DISTRIBUTION:
dnorm(3)  # the density (or in the discrete case the probability) of 3 in a normal distribution
dnorm(6)
pnorm(0)# probability, the cumulative distribution (e.g. p(x<=0)) in the given parameters
pnorm(3)
qnorm(0.8) # the quantile - e.g. which k has p(x<=k)=0.8, like the Z table
rnorm(100)  # a sample of 100 random independent "observations" drawn from normal distribution

#to see a list of available distributions:
?distributions

#some useful commands in descriptive statistics
sum(x)
mean(x)
sd(x)
var(x)
median(x)
table(x)# a frequency table
quantile(x)
quantile(x,c(0.9,0.3))
plot(x,y)# a scatterplot
?hist
lenx = length(x)
hist(x, breaks = lenx) # y-axis is "freq", histogram helps visualize the density
hist(x, freq = F, breaks = lenx, ylim = c(0, 0.4)) #y-axis is "density" s.t. total area equals 1.

# we might want to display the y axis in percentiles <- how do we do that?

summary(x)#gives the min,max,median,25% quantile and the 75% quantile of x

#Part C. Real datasets

#import dataset: the  easiest  way to import dataset is to click on the "import Dataset" button(on the right side on your screen)
#then,choose the "From Text File" option and choose your file.
#notice that the only formats that R can identify in this is csv and txt.
#a second way to the same thins is to write: seminar2 <- read.csv("C:/Users/ur_username/Desktop/bla_bla.csv")
#where you need to supplied in parentheses the path of your dataset.

?airquality # built-in data file for download
airquality <- read.csv("C:/Users/user/Documents/R_Language/Stats_Principles/data/tirguls/airquality.csv") ##change the directory as its in your computer
head(airquality)
#if you want to see only one column you can use the $ sign.tab is also useful here.
temp <- airquality$Temp
airquality$Wind
#now we can calculate a lot of statistics
mean(airquality$Temp)
sd(airquality$Temp)
hist(airquality$Temp)
plot(airquality$Temp,airquality$Wind) # 
quantile(airquality$Temp)
#this command calculate the standardized values
Z.Temp <- (airquality$Temp-mean(airquality$Temp))/sd(airquality$Temp)
hist(Z.Temp)
#below are linear transformations of the mean and median values
 #evidently, their values change linearly (e.g multiply by scalar or add x^1)
 #in other words, the mean & median are "immune" to linear transformations
 #e.g for y = 4x+3 => mean(y) = 4mean(x)+3
y<-airquality$Temp*(9/5)+32
mean(y)
mean(airquality$Temp)*(9/5)+32 
median(y)
median(airquality$Temp)*(9/5)+32
sd(y)
sd(airquality$Temp)*abs((9/5))
