# Simulation Study for final project

Nsim <- 10^4

# specify parameters
this.n <- 50
this.rho <- 0.01
this.x <- 1:this.n

predict.x <- this.n+1
#allocate memory for each simulation's bias
all.pi <- NULL

# simulation
for( i in 1:Nsim){
  # realization from AR(1)
  this.y <- arima.sim(n=this.n+1,list(ar=this.rho,ma=0))
  this.ystar <- this.y[this.n+1]
  this.y <- this.y[-(this.n+1)]
  
  
  data.sample <- data.frame(this.x,this.y)
  lmfitdata <- lm(data=data.sample,this.y~this.x)
  upper.bound <- predict(lmfitdata, newdata=data.frame(this.x=predict.x),interval="prediction")[3]
  lower.bound <- predict(lmfitdata, newdata=data.frame(this.x=predict.x),interval="prediction")[2]
  all.pi[i] <- (this.ystar < upper.bound & this.ystar > lower.bound)
}

# approximate prediction interval coverage
mean(all.pi)

# Look at the different plots of simulated data
this.n <- 100
this.rho <- 0.01
this.x <- 1:this.n

this.y <- arima.sim(n=this.n+1,list(ar=this.rho,ma=0))
this.ystar <- this.y[this.n+1]
this.y <- this.y[-(this.n+1)]
plot(this.x,this.y,type="b",main="Rho=0.90",ylab="y",xlab="Index")
data.sample <- data.frame(this.x,this.y)
lmfitdata <- lm(data=data.sample,this.y~this.x)
summary(lmfitdata)
dwt(lmfitdata)


# Looking at the different values for n
n.vals <- c(50,100,200,500)
prop.01 <- c(0.9536,0.9538,0.9483,0.9488)
prop.80 <- c(0.9157,0.9276,0.9424,0.9446)
prop.90 <- c(0.8858,0.9092,0.9290,0.9405)
prop.99 <- c(0.8403,0.8462,0.8463,0.8721)
prop.9999 <- c(0.8418,0.8344,0.8255,0.8328)

plot(n.vals,prop.01,type="l",ylim=c(0.8,1))
lines(n.vals,prop.80)
lines(n.vals,prop.90)
lines(n.vals,prop.99)
lines(n.vals,prop.9999)
