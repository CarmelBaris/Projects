hapoalim <- c(0.21, -9.39, -2.23, -8.69, 0.89, 9.48, -11.35, 4.90, -6.99, 9.12, 6.90, -4.19, -1.30, -0.34, -1.89, 12.90, 0.64, -3.56, 3.18, 1.00)
leumi <- c(-0.34, -0.46, 2.24, -5.32, -5.81, 8.24, -10.31, 0.06, -5.43, 1.35, 5.04, -10.37, 4.48, -0.24, -6.72, -9.65, 3.54, -0.78, 1.08, 4.09)
hapoal_var <- var(hapoalim)
leum_var <- var(leumi)
hapoal_mean <- mean(hapoalim)
leum_mean <- mean(leumi)
D <- hapoalim - leumi
D_var <- hapoal_var + leum_var - 2*cov(hapoalim, leumi)
2 * (1 - pt((mean(D)-0)/sqrt(var(D)/length(D)),length(D)-1))
'--------------------------'
'Q2'
'האם לדחות את השערת האפס'
qt(0.955,19)<T
'--------------------------'
'Q3'
Xm<-85
Ym<-84
var<-100
n<-80
m<-60
alpha<-0.05
s<-(Xm-Ym)/sqrt((var/n)+(var/m))
'האם לדחות'
s>qnorm(1-(alpha/2))
'P-VAL חישוב'
1-pnorm(s) 
'--------------------------'
'Q4'
'חישוב עוצמה'
diff<-4
alpha<-0.05
var<-100
n<-80
m<-60
1-pnorm(qnorm(1-alpha)-(diff/(sqrt((var/n)+(var/m)))))
