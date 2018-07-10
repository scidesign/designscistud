yA <- c(2,4,2,1,9,9,2,2)
yB <- c(8,3,5,3,7,7,4)
beer <- c(yA,yB) #pool data
N <- choose(15,8)
res <- numeric(N) # store the results
#install.packages("combinat") # if package not installed then remove comment
library(combinat)
index <-combn(1:15,8) # Generate N treatment assignments
for (i in 1:N)
{
  res[i] <- mean(beer[index[,i]])-mean(beer[-index[,i]])
}
hist(res,xlab="ybarA-ybarB", main="Randomization Distribution of difference in means")

observed <- mean(yA)-mean(yB) #store observed mean difference
abline(v=observed,col="blue") #add line at observed mean diff
abline(v=-observed,col="blue") #add line at observed mean diff

tbar <- mean(res)
abline(v=tbar,col="red") #add line at observed mean diff
pval <- sum(abs(res-tbar)>=abs(observed-tbar))/N
pval

t.test(yA,yB)

X <-rbind(c(1,1,1,1),c(1,1,-1,-1),c(1,-1,1,-1),c(1,-1,-1,1))

solve( t(X) %*% X ) %*% t(X)

X1 <-rbind(c(1,1,1,1),c(1,1,0,0),c(1,0,1,0),c(1,0,0,1))
solve( t(X1) %*% X1 ) %*% t(X1)
