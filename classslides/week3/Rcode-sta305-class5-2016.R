#week 3 R code STA305

#generate data for 1st headache trial
set.seed(1)
datA <- rnorm(15,mean = 34,sd = 3)
datB <- rnorm(15,mean = 32,sd = 2.8)
mean(datA);sd(datA)
mean(datB);sd(datB)
t.test(datA,datB,var.equal=T)

#generate data for 2nd headache trial
set.seed(205)
datA <- rnorm(15,mean = 34,sd = 3)
datB <- rnorm(15,mean = 32,sd = 2.8)
mean(datA);sd(datA)
mean(datB);sd(datB)
t.test(datA,datB,var.equal=T)




x <- seq(-3,3,by=0.1)
plot(x,dt(x,df=28),type="l",main="t distribution df=28")
abline(v=qt(.975,df=28),col="blue")
abline(v=qt(.025,df=28),col="blue")
31-2.6abline(v=t.test(datA,datB,var.equal=T)$stat,col="red",lwd=3)

y <-c(datA,datB)
t.obs <- t.test(yA,yB,var.equal=T)$stat
obs.mean <- mean(datA)-mean(datB)
N <- 10000 #Number of resamples
t.stat.sim <- numeric(N)
res <- numeric(N)
for(s in 1:N)
{
  xsim <-sample(30,size=15,replace = F) #sample without replacement
  #compute t-stat. under new treatment assignment
  tmp <- t.test(y[xsim],y[-xsim],var.equal=T)
  #put t-stat in a vector
  t.stat.sim[s]<-tmp$stat
  res[s] <- mean(y[xsim])-mean(y[-xsim])
}
mean(abs(t.stat.sim)>=abs(t.obs))
2*(sum(res>=obs.mean)+1)/(N+1)

qqnorm(datA);qqline(datA)
qqnorm(datB);qqline(datB)
