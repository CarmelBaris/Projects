# sample
n=20
U <- runif(1)*2
X <- rnorm(n,mean=U,sd=4)
m.X <- mean(X)
sd.x <- sd(X)

#CI
z <- qnorm(0.95)  # 90% CI
t <- qt(0.95,n-1)
left.known <- m.X-z*4/sqrt(n)
right.known <- m.X+z*4/sqrt(n)
left.unknown <- m.X-t*sd.x/sqrt(n)
right.unknown <- m.X+t*sd.x/sqrt(n)

#Length
right.known-left.known
right.unknown-left.unknown

#plot
a <- min(left.known,left.unknown)
b <- max(right.known,right.unknown)
plot(c(left.known,right.known),c(1,1),type='l', lwd=3,ylim=c(0,1.5),xlim=c(-3,5),
     xlab='mu',ylab="")
lines(c(left.unknown,right.unknown),c(0.5,0.5),type='l', lwd=3,col='blue')
abline(v=c(left.unknown,right.unknown),col='blue',lty='dashed')
abline(v=c(left.known,right.known),col='black',lty='dashed')
abline(v=U,col='red')


# dist of length
n <- 8
alpha <- 0.05
sd.true <- 5
X <- rnorm(10000*n,0,sd.true)
dim(X) <- c(10000,n)
L <- NULL
for (i in 1:10000){
  Si <- sd(X[i,])
  Li <- 2*Si/sqrt(n)*qt(1-alpha/2,n-1)
  L <- c(L,Li)
}

hist(L,freq=FALSE,breaks=100)
L.norm <- 2*sd.true/sqrt(n)*qnorm(1-alpha/2)
abline(v=L.norm,col='blue',lwd=3)
mean(L<L.norm)

