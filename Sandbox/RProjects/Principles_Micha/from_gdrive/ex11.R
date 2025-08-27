x=c(50.83,49.23,41.37,42.36,29.42,15.44,24.98,28.43,37.55,51.51,13.2,29.04,34.53,47.35,44.07,25.88)
h0<-30
h1<-30.2
alpha<-0.05
sd<-4
n<-15
'חישוב אזור הדחיה'
{
  if(h1>h0){print(paste("[",c=h0+qnorm(1-alpha)*(sd/sqrt(n)), ",???]"))}
  else{(paste("[-???,",c=h0-qnorm(1-alpha)*(sd/sqrt(n)), "]"))}
}
'הנוסחה המלאה - חישוב העוצמה - פאי'
{
  if (h1>h0){print(1-(pnorm(((h0+qnorm(1-alpha)*(sd/sqrt(n)))-h1)/(sd/sqrt(n)))))}
  else {print(pnorm((h0-h1)/(sd/sqrt(n))-qnorm(1-alpha)))}
}
