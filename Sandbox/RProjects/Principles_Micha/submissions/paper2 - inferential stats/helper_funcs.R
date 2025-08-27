# ++++++++++++++++++++++++ #
# PRECISION
# ++++++++++++++++++++++++ #

SciToRealNum <- function(a,b){
  c < str(a-b)
  n <- 3
  power <- strtoi(substr(c, nchar(c)-n+1, nchar(c)))
  cat(paste0(round((a-b)*(10^power), 3),"10^", power))
}
