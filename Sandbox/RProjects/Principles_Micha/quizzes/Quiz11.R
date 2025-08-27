'DATA'
h0<-80
h1<-83
alpha<-0.05
sd<-12
n<-100
'-------------------------------------'
'Q1'
'Power of a test - pie'
{
  if (h1>h0){print(1-(pnorm(((h0+qnorm(1-alpha)*(sd/sqrt(n)))-h1)/(sd/sqrt(n)))))}
  else {print(pnorm((h0-h1)/(sd/sqrt(n))-qnorm(1-alpha)))}
}
'-------------------------------------'
'Q2-3'
'Confidence Interval for On'
'???? ??? ???? ?????? ??-?????? ???????'
'???? ?????? ???? ?????? ??-??????'
'???? ???? ???'
'P-VAL - ????? ?? ?????'
'P-VAL - ????? ?? ?????'
'רווח סמך עבור השערות דו-צדדיות מורכבות'
print(paste("[",h0-qnorm(1-(alpha/2))*sd/sqrt(n),",",h0+qnorm(1-(alpha/2))*sd/sqrt(n),"]"))
'אזור הדחייה  עבור השערות דו-צדדיות'
print((h0+qnorm(1-(alpha/2))*sd/sqrt(n))-h0)
'אורך רווח סמך'
print((h0+qnorm(1-(alpha/2))*sd/sqrt(n))-(h0-qnorm(1-(alpha/2))*sd/sqrt(n)))
'-------------------------------------'
'Q5'
'P-VAL - השערה חד צדדית'
print(pnorm((h1-h0)/(sd/sqrt(n))))
'P-VAL - השערה דו צדדית'
Xm<-h0+qnorm(1-(alpha/2))*sd/sqrt(n)
'print(2*(1-pnorm((Xm-h0)/(sd/sqrt(n)))))'
