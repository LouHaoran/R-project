#####problem 1######
####################

##use combination of (1,1), compute the mean squared errors of both theta1 and theta2
set.seed(1213)
est1 = mean(replicate(1000, (max(runif(1,0,1))-1)^2))
set.seed(1213)
est2 = mean(replicate(1000, (2*mean(runif(1,0,1))-1)^2))


#function to compute the maximum likelihood estimator and methods of moment estimator 
fun1=function(n,theta){
  x = runif(n,0,theta)
  mle = max(x)
  mome = 2*mean(x)
  result = c(mle,mome)
  return(result)
}

#####problem 2#######
#####################

#b.##theta = 1.03250084 
theta = 5/(log(21.72+14.65+50.42+28.78+11.23))

#c.##use optim funtion
## negative log likelyhood function fun2
fun2 = function(theta,data1){
  f1 = sum(log(theta/(data1^theta+1)))
  return(-f1)
}
data1 = c(21.72, 14.65, 50.42, 28.78, 11.23)

##use optim function
est11 = optim(par = 0.4, fn = fun2, method = "L-BFGS-B", 
              lower = 0, hessian = TRUE, dat = data1)
est11

#Standard Error
I = sqrt(diag(solve(est11$hessian)))

alpha = 1 - 0.95

z = qnorm(1 - alpha/2)

#lower and upper bound of confidencial interval.
lowerBound = est11$par - z * I
upperBound = est11$par + z * I

ConItv = c(lowerBound, upperBound)
ConItv



