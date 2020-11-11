na.if <- function(x, y){
  x[x %in% y] <- NA
  return(x)
}
