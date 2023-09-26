n <- c(1,2,3,5,10,30)
thetas <- c(1,5,50,100)
theta_mme <- numeric(0)
theta_mle <- numeric(0)
mse_mme <- numeric(0)
mse_mle <- numeric(0)
for (i in n){
  a=1
  for (j in thetas){
    for(k in 1:1000){
      data <- runif(i,0,j)
      theta_mme[k] <- 2*mean(data)
      theta_mle[k] <- max(data)
    }
    mse_mme[a] <- mean((theta_mme-j)^2)
    mse_mle[a] <- mean((theta_mle-j)^2)
    print(paste("Printing MSE_MME for theta = ",j," and n = ", i, " MSE = ",mse_mme[a]))
    print(paste("Printing MSE_MLE for theta = ",j," and n = ", i, " MSE = ",mse_mle[a]))
    a=a+1
  }
  plot(thetas,mse_mme,type = 'l', col="red",lwd=2, ylab="")
  title(main = paste("Plotting graph for MSE vs 'theta' for n = ",i), ylab = 'Mean-Squared Error')
  lines(thetas,mse_mle,type = 'l', col="blue",lwd=2)
  legend("topleft", legend = c('MME','MLE'), fill = c('red','blue')) 
}