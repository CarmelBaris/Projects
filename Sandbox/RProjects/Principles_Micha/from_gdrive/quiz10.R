##---- find c ----
find_c<- function(alpha,u0,u1,sd,n){
  if (u0<u1){print(u0+qnorm((1-alpha))*sd/sqrt(n))}
  else{print(u0-qnorm(1-alpha)*sd/sqrt(n))}
}
#find_c(alpha=0.05,u0=200,u1=205,sd=4,n=25)

find_alpha <- function(u0,u1,c,sd,n){
  if (u0<u1){print(1-pnorm((c-u0)/(sd/sqrt(n))))}
  else{print(pnorm((c-u0)/(sd/sqrt(n))))}
}
#find_alpha(u0=40,u1=45,c=41.56,sd=4,n=25)


##---- find pie----
find_pie<-function(c,u0,u1,sd,n){
  if (u1>u0){print(1-pnorm((c-u1)/(sd/sqrt(n))))}
  else {print(pnorm((c-u1)/(sd/sqrt(n))))}
}
find_pie(c=70.5,u0=60,u1=70,sd=4,n=25)

##-------------------


#find_alpha(5,6,1/3,0.7,0.5,4)


#sd<-2
#n<-15
#weight<-mean(c(5,8,2,3,-1,-1,11.5,2.5,0,5,2,1,-2,3,2))
#u0<-4
#find_c(0.05,2,4,2,15)
#find_c(0.05,2,4,2,15)

