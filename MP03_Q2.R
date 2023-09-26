# maximizing the log-likelihood function of Pareto distribution by using Optim function.
data <- c(21.72,14.65,50.42,28.78,11.23)
neg.loglik.fun<-function(par,data){
  result=0
  for(i in data){
    result<- result + (log(par)-(par+1)*log(i) )
  }
  return(-result)
}
ml.est <- optim(par=5, fn=neg.loglik.fun, hessian = T, dat=data)
#get estimated value and other associated results.
ml.est

# Standard error of the estimation
std_err<-sqrt(diag(solve(ml.est$hessian)))
std_err

#Confidence interval for the estimation
conf_intr<- ml.est$par +c(-1,1)*qnorm(.975)*std_err
conf_intr