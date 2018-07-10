# This is data for Q2

set.seed(1001)
A <- abs(round(rnorm(5000,20,5),2))
B <- abs(round(rnorm(5000,20.2,6),2))
sales <- c(A,B)
page <- c(rep("A",5000),rep("B",5000))
ABtest <- data.frame(page,sales)
write.csv(x = ABtest,file = "ABtest.csv")
t.test(A,B,var.equal = F)


ABtest <- read_csv("~/Dropbox/Docs/sta305/2017/assignments/hw1/ABtest.csv")
attach(ABtest)
N <- 250000
res <- numeric(N) # store the results

for (i in 1:N)
{
  index <- sample(length(sales),size=length(sales[page=="A"]),replace=F)
  res[i] <- median(sales[index])-median(sales[-index]) }

observed <- median(sales[page=="A"])-median(sales[page=="B"])

(sum(res <= observed)+1)/(N+1)

hist(res,xlab="medianA-medianB", main="Randomization Distribution of difference in medians")


# This is data for Q3

set.seed(1017)
pre <- rbinom(400,20,.8)
post <- rbinom(400,20,.9)
student <- 1:400
gender <- c(rep("M",200),rep("F",200))
langstudy <- data.frame(student,pre,post,gender)

write.csv(langstudy,file = "langstudy.csv")

FertF <- c(78, 82, 82, 65, 51, 75)
FertG <- c(72, 70, 55, 85, 59, 80)
diff <- FertF-FertG
meandiff <- mean(diff)

N <- 2^(6) # number of treatment assignments
res <- numeric(N) #vector to store results
LR <- list(c(-1,1)) # difference is multiplied by -1 or 1
trtassign <- expand.grid(rep(LR, 6)) # generate all possible treatment assign

for(i in 1:N){
  res[i] <- mean(as.numeric(trtassign[i,])*diff)
}

tbar <- mean(res)
pval <- sum(abs(res-tbar)>=abs(observed-tbar))/N
pval

hist(res, xlab="Mean Difference",main="Randomization Distribution of Mean Difference in Fertilizers")
abline(v = meandiff,col="blue") 

qqnorm(FertF);qqline(FertF)
qqnorm(FertG);qqline(FertG)
qqnorm(FertF-FertG);qqline(FertF-FertG)
t.test(FertF-FertG) 


 