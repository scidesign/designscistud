alpha <- .05
beta <- 1-.55
sigma <- 2
k <- 1
delta <- 1
margin <- 0

noninfer.pow <- function(alpha,delta,theta,n1,n2,sigma)
{
  zalpha <- qnorm(1-alpha)
  return(1-pnorm(zalpha-(delta+theta)/(sigma*(sqrt(1/n1+1/n2)))))
}
noninfer.pow(alpha,delta,margin,25,25,sigma)

library(TrialSize)

checkit <- TwoSampleMean.NIS(alpha,beta,sigma,k,delta,margin)
checkit

set.seed(12)
N <- 1000
n <- 50
res <- numeric(N)
res <- replicate(N,t.test(rexp(n = n,rate = 1/10),
                          rexp(n = n,rate = 1/6.67),var.equal = F)$p.value)
sum(res<=0.05)/N

set.seed(12)
N <- 1000
n <- 100
res <- numeric(N)
res <- replicate(N,wilcox.test(rexp(n = n,rate = 1/10),
                          rexp(n = n,rate = 1/6.67))$p.value)
sum(res<=0.05)/N

set.seed(12)
N <- 1000
n <- 65
res <- numeric(N)
res <- replicate(N,wilcox.test(rnorm(n = n,mean = .1^{-1},sd=sqrt(.1^{-2})),
                               rnorm(n = n,mean = .15^{-1},sd=sqrt(.15^{-2})))$p.value)
sum(res<=0.05)/N



set.seed(12)
N <- 1000
n <- 80
res <- numeric(N)
res <- replicate(N,t.test(rnorm(n = n,mean = .1^{-1},sd=sqrt(.1^{-2})),
                          rnorm(n = n,mean = .15^{-1},sd=sqrt(.15^{-2})),var.equal = F)$p.value)
sum(res<=0.05)/N

set.seed(12)
N <- 1000
n <- 80
res <- numeric(N)
res <- replicate(N,wilcox.test(rnorm(n = n,mean = .1^{-1},sd=sqrt(.1^{-2})),
                          rnorm(n = n,mean = .15^{-1},sd=sqrt(.15^{-2})),var.equal = F)$p.value)
sum(res<=0.05)/N


set.seed(12)
N <- 1000
n <- 20
res <- numeric(N)
res <- replicate(N,t.test(runif(n = n,min = 1,max = 10),
                          runif(n = n,min=1,max=5),var.equal = F)$p.value)
sum(res<=0.05)/N






t.test(rexp(n = 20,rate = 1/5),rexp(n = 20,rate = 1/8))
t.test(rexp(n = 20,rate = 1/5),rexp(n = 20,rate = 1/10))

x <- seq(0,20,by=0.1)
plot(x = x, y = dexp(x,rate = 1/8),type="l")
points(x = x,y=dexp(x,rate = 1/10),type = "b")

