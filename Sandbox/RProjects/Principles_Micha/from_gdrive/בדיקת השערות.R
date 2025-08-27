'SD known'
'DATA'
h0<-40
h1<-45
alpha<-0.05
sd<-sqrt(100)
n<-16
'-------------------------------------'
'חישוב אזור הדחיה'
{
  if(h1>h0){print(paste("[",c=h0+qnorm(1-alpha)*(sd/sqrt(n)), ",infnity]"))}
  else{(paste("[-infnity,",c=h0-qnorm(1-alpha)*(sd/sqrt(n)), "]"))}
}
'-------------------------------------'
'חישוב רמת המובהקות - אלפא'
C<-191.1
{
  if (h0<h1){print(1-pnorm((C-h0)/(sd/sqrt(n))))}
  else{print(pnorm((C-h0)/(sd/sqrt(n))))}
}
'-------------------------------------'
'חישוב העוצמה - פאי'
{
  if (h1>h0){print(pnorm((h0-qnorm(1-alpha)*(sd/sqrt(n))-h1)/(sd/sqrt(n))))}
  else {print(pnorm((h0-h1)/(sd/sqrt(n))-qnorm(1-alpha)))}
}
'-------------------------------------'
'Confidence Interval for Two-Sided Composite Hypotheses (H0 is a single-value, H1 is a range)'
print(paste("[",h0-qnorm(1-(alpha/2))*sd/sqrt(n),",",h0+qnorm(1-(alpha/2))*sd/sqrt(n),"]"))
'Length of Confidence Interval'
print((h0+qnorm(1-(alpha/2))*sd/sqrt(n))-(h0-qnorm(1-(alpha/2))*sd/sqrt(n)))
'Area of Rejection for Two-Sided Hypotheses'
print((h0+qnorm(1-(alpha/2))*sd/sqrt(n))-h0) 
'-------------------------------------'
'P-VAL'
print(pnorm((h1-h0)/(sd/sqrt(n))))
h