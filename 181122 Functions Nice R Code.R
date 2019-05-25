# 181122 Functions Nice R Code
#https://nicercode.github.io/guides/functions/
#notes on this site


sum.of.squares <- function(x,y) {
  x^2 + y^2
}

sum.of.squares(2,3)

2^2+3^2

function.name <- function(arg1, arg2, arg3=2, ...) {
  newVar <- sin(arg1) + sin(arg2)  # do Some Useful Stuff
  newVar / arg3   # return value 
}

data.frame(a=1, b=2)

data.frame(a=1, b=2, c=3)

red.plot <- function(x, y, ...) {
  plot(x, y, col="red", ...)
}

red.plot(1:10, 1:10, xlab="My x axis", ylab="My y axis")


data$response.logit <- log(data$response / (1 - data$response))

logit <- function(p){
  
  
}
  log(p / (1-p))

data$response.logit <- logit(data$response)




