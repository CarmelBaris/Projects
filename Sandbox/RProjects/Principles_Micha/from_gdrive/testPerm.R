'מבחן פרמוטציות'
x <- c(8.2, 9.4, 10, 15.2, 16.1, 17.6, 21.5)
y <- c(4.2, 5.2, 5.8, 6.4, 7.0, 7.3, 10.1, 11.2, 11.3, 11.5)

T.stat <- function(X,Y){
  n.X <- length(X)
  n.Y <- length(Y)
  (abs(mean(X)-mean(Y)))/sqrt(var(X)/n.X+var(Y)/n.Y)
}

T.obs <- T.stat(x,y)

all <- c(x,y)
T.all <- NULL
for (b in 1:100000){
  perm <- sample(all)
'X,Y תלוי באורך של'
  X.b <- perm[1:7] 
  Y.b <- perm[8:17]
  T.all <- c(T.all,T.stat(X.b,Y.b))
}

my_p_val = mean(abs(T.all) >= abs(T.obs))
print(my_p_val)