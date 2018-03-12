n = 2000
##Part 1##
##Generate data from logit##
dif1<-c()
dif2<-c()
for(i in 1: 100){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x <- cbind(x1,x2,x3)
  beta <- c(1,2,3)
  p <- 1/(1+exp(-x%*%beta))
  y <- rbinom(n,1,p)
  xfit <- x[1: 1000, ]
  xtest <- x[1001: 2000, ]
  yfit <- y[1: 1000]
  dat= data.frame(a = yfit, b = xfit)
  ptest <- p[1001: 2000]
  fit1 = glm(a ~ ., data=dat,family = binomial(link = "logit"))
  fit2 = glm(a ~ ., data=dat,family = binomial(link = "probit"))
  p1 <- predict(fit1, data.frame(b = xtest), type="response")
  p2 <- predict(fit2, data.frame(b = xtest), type="response")
  dif1[i] <- sum((p1 - ptest)^2)
  dif2[i] <- sum((p2 - ptest)^2)
}
diff1 <- mean(dif1)
diff2 <- mean(dif2)
cat("The mean error for logit link function is", diff1)
cat("The mean error for probit link function is", diff2)

##When generating data with logit link function, the mean error of logit
##link method is about 0.65, and the mean error of probit link method is about 0.75.
##So the mean error of logit link method is smaller than that of probit link method.
##Thus, logit link method more accuracy than probit link method.


##Part 2##
##Generate data from probit##
dif1<-c()
dif2<-c()
for(i in 1: 100){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x <- cbind(x1,x2,x3)
  beta <- c(1,2,3)
  p <-  pnorm(x%*%beta)
  y <- rbinom(n,1,p)
  xfit <- x[1: 1000, ]
  xtest <- x[1001: 2000, ]
  yfit <- y[1: 1000]
  dat= data.frame(a = yfit, b = xfit)
  ptest <- p[1001: 2000]
  fit1 = glm(a ~ ., data=dat,family = binomial(link = "logit"))
  fit2 = glm(a ~ ., data=dat,family = binomial(link = "probit"))
  p1 <- predict(fit1, data.frame(b = xtest), type="response")
  p2 <- predict(fit2, data.frame(b = xtest), type="response")
  dif1[i] <- sum((p1 - ptest)^2)
  dif2[i] <- sum((p2 - ptest)^2)
}
diff1 <- mean(dif1)
diff2 <- mean(dif2)
cat("The mean error for logit link function is", diff1)
cat("The mean error for probit link function is", diff2)

##When generating data with probit link function, the mean error of logit
##link method is about 0.65, and the mean error of probit link method is about 0.55.
##So the mean error of logit link method is larger than that of probit link method.
##Thus, probit link method more accuracy than logit link method.


##Part 3##
##Generate data from cloglog##
dif1<-c()
dif2<-c()
for(i in 1: 100){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x <- cbind(x1,x2,x3)
  beta <- c(1,2,3)
  p <-  1-exp(-exp(-x%*%beta))
  y <- rbinom(n,1,p)
  xfit <- x[1: 1000, ]
  xtest <- x[1001: 2000, ]
  yfit <- y[1: 1000]
  dat= data.frame(a = yfit, b = xfit)
  ptest <- p[1001: 2000]
  fit1 = glm(a ~ ., data=dat,family = binomial(link = "logit"))
  fit2 = glm(a ~ ., data=dat,family = binomial(link = "probit"))
  p1 <- predict(fit1, data.frame(b = xtest), type="response")
  p2 <- predict(fit2, data.frame(b = xtest), type="response")
  dif1[i] <- sum((p1 - ptest)^2)
  dif2[i] <- sum((p2 - ptest)^2)
}
diff1 <- mean(dif1)
diff2 <- mean(dif2)
cat("The mean error for logit link function is", diff1)
cat("The mean error for probit link function is", diff2)
##When generating data with cloglog link function, the mean error of logit
##link method is about 1.25, and the mean error of probit link method is about 1.45.
##So the mean error of logit link method is smaller than that of probit link method.
##Thus, For the model in this example, logit link method more accuracy than probit 
##link method.