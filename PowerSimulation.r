## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(cache=TRUE, fig.width=4, fig.height=3.5, dpi=600,
               prompt=TRUE,comment=NA)
options(width=50)
articlemode <- FALSE

## ----CI, echo=FALSE, fig.keep=if(articlemode){'high'}else{'all'}, results='asis'----
{plot.new()
plot.window(xlim=c(-0.5, 1.5), ylim=c(0,1.2))
axis(1, at=c(0,1), labels=c('No Effect','M.I.E.'))
box(bty='L')
abline(v=c(0,1), lty=2)}
if(!articlemode) cat('\n\n## Confidence Interval Approach\n\n')
arrows(-0.3, 0.2, 0.2, code=3, length=0.1)
if(!articlemode) cat('\n\n## Confidence Interval Approach\n\n')
arrows(0.3, 0.4, 0.8, code=3, length=0.1)
if(!articlemode) cat('\n\n## Confidence Interval Approach\n\n')
arrows(0.9, 0.6, 1.4, code=3, length=0.1)
if(!articlemode) cat('\n\n## Confidence Interval Approach\n\n')
arrows(1.1, 0.8, 1.6, code=3, length=0.1)
if(!articlemode) cat('\n\n## Confidence Interval Approach\n\n')
arrows(-0.1, 1.0, 1.2, code=3, length=0.1)

## ----t1------------------------------------------------------------------
power.t.test(n=300, delta=0.8, sd=3)

## ----t2------------------------------------------------------------------
simfun <- function(n=100, diff=0, sd=1) {
  x1 <- rnorm(n, 0,    sd)
  x2 <- rnorm(n, diff, sd)
  t.test(x1, x2)$p.value
}

## ----t22-----------------------------------------------------------------
out1 <- replicate(10000, simfun(n=300, diff=0.0, sd=3))
mean(out1 <= 0.05)
hist(out1)
abline(v=0.05, col='red')

## ----t23-----------------------------------------------------------------
out2 <- replicate(10000, simfun(n=300, diff=0.8, sd=3))
mean(out2 <= 0.05)
hist(out2)
abline(v=0.05, col='red')

## ----tci-----------------------------------------------------------------
binom.test(sum(out2<=0.05), length(out2))


## ----t3------------------------------------------------------------------
diff <- 0.8
n <- 300
sd <- 3
nsim <- 10000
x1 <- matrix(rnorm(n*nsim,0,sd), nrow=nsim)
x2 <- matrix(rnorm(n*nsim,diff,sd), nrow=nsim)
x <- cbind(x1,x2)
out <- apply(x, 1, function(xx) 
  t.test(xx ~ rep(1:2, each=n))$p.value)
mean(out <= 0.05)

## ----lowpower------------------------------------------------------------
simfun <- function(n=100, diff=0, sd=1) {
  x <- rnorm(n, diff, sd)
  out <- t.test(x)
  c(p=out$p.value, mean=out$estimate, ci=out$conf.int)
}

out <- replicate(1000, simfun(n=50, diff=0.3))
mean(out[1,] < 0.05)

plot(out[2,], seq_len(ncol(out)), 
     xlim=range(out[3:4,]), type='n',
     ylab='', xlab='Mean')
segments(x0=out[3,], x1=out[4,], y0=seq_len(ncol(out)), 
         col='green')
points(out[2,], seq_len(ncol(out)), pch='|')
abline(v=0.3, col='red')

## ----lowpower2-----------------------------------------------------------
w <- out[1,] < 0.05
plot(out[2,], seq_len(ncol(out)), 
     xlim=range(out[3:4,]), type='n',
     ylab='', xlab='Mean')
segments(x0=out[3,], x1=out[4,], y0=seq_len(ncol(out)), 
         col=ifelse(w, 'green',NA))
points(out[2,], seq_len(ncol(out)), pch=ifelse(w,'|',''))
abline(v=0.3, col='red')

## ----lowpower3-----------------------------------------------------------
mean( out[2,w] > 0.3 )
mean( out[2,w] > 0.6 )
mean( out[3,w] > 0.3 )

## ----mn------------------------------------------------------------------
simfun <- function(n=100, diff=0, sd=2, p=0.1) {
  x <- rnorm(n, diff, 
               ifelse(rbinom(n,1,p), sd, 1))
  c(t.test(x)$p.value,wilcox.test(x)$p.value)
}
out1 <- replicate(10000, simfun(n=30, diff=0))
rowMeans(out1 <= 0.05)
out2 <- replicate(10000, simfun(n=30, diff=0.5))
rowMeans(out2 <= 0.05)

## ----fet-----------------------------------------------------------------
simfun <- function(n=50, p=c(0.5, 0.5, 0.5)) {
  x1 <- sample(1:3, n, replace=TRUE)
  x2 <- rbinom(n, 1, p[x1])
  fisher.test( table(x1,x2) )$p.value
}
out1 <- replicate(10000, 
                  simfun(n=50, p=c(0.5, 0.5, 0.5)))
out2 <- replicate(10000, 
                  simfun(n=50, p=c(0.5, 0.3, 0.7)))
c(mean(out1<=0.05), mean(out2<=0.05))

## ------------------------------------------------------------------------
simfun <- function(n=100, true.p=0.02, test.p=0.04, 
                   alpha=0.05) {
  x <- rbinom(1, n, true.p)
  qbeta(1-alpha/2, x+1, n-x+1) < test.p
}
out1 <- replicate(10000, simfun(n=1000, true.p=0.04))
mean(out1)
out2 <- replicate(10000, simfun(n=1000))
mean(out2)

## ----reg-----------------------------------------------------------------
library(MASS)
simfun <- function(n=100, beta=rep(0,9),
                   mu.x=rep(0,8), var.x=diag(8),
                   sig=1) {
  x <- mvrnorm(n, mu.x, var.x)
  y <- cbind(1,x) %*% beta + rnorm(n,0,sig)
  mydat <- cbind( as.data.frame(x), y)
  names(mydat) <- c(paste0('x',1:8), 'y')
  fit1 <- lm(y ~ x4 + x5 + x6 + x7 + x8, data=mydat)
  fit2 <- lm(y ~ ., data=mydat)
  anova(fit1, fit2)[2,6]
                   }

## ----logistic------------------------------------------------------------
simfun <- function(n=1000, b0=0, b1=0) {
  x <- rnorm(n)
  eta <- b0 + b1*x
  p <- exp(eta)/(1+exp(eta))
  y <- rbinom(n,1,p)
  fit <- glm(y~x, family=binomial)
  coef(summary(fit))[2,4]
}

## ----logistic2, warning=FALSE, message=FALSE-----------------------------
library(MASS)
simfun <- function(n=1000, b0=0, b1=0) {
  x <- rnorm(n)
  eta <- b0 + b1*x
  p <- exp(eta)/(1+exp(eta))
  y <- rbinom(n,1,p)
  fit <- glm(y~x, family=binomial)
  ci <- confint(fit,2)
  c(ci, ci[2]-ci[1])
}

out <- replicate(100, simfun(b1=0.8))
apply(out, 1, range)
plot(out[1,], 1:100, type='n', xlim=range(out[1:2,]))
segments(out[1,],1:100, out[2,])

## ----cox-----------------------------------------------------------------
library(survival)
simfun <- function(n=100, beta = 0, cens.scale=1) {
  x <- rnorm(n, 10, 2)
  y <- rexp(n, 1/(1+beta*x))
  cens <- rexp(n, 1/cens.scale)
  time <- pmin(y,cens)
  status <- ifelse(y<cens, 1, 0)
#print(table(status))
  fit <- coxph(Surv(time,status) ~ x)
  coef(summary(fit))[1,5]
}

## ----mixedeffects, warning=FALSE, message=FALSE--------------------------
library(lme4)
simfun <- function(n.student=100, n.school=20, 
                   sig.student=1, sig.school=2,
                   b0=0, b1=0, b2=0, b12=0) {
  x1 <- rnorm(n.student*n.school, 0, 1)
  x2 <- rbinom(n.student*n.school, 1, 0.5)
  re.school <- rnorm(n.school,0,sig.school)
  school.id <- rep(1:n.school, each=n.student)
  y <- b0 + b1*x1 + b2*x2 + b12*x1*x2 + 
    re.school[school.id] + 
    rnorm(n.student*n.school,0,sig.student)
  fit1 <- lmer( y ~ x1 + (1|school.id))
  fit2 <- lmer( y ~ x1*x2 + (1|school.id))
  anova(fit1,fit2)[2,8]
}

## ----multsize------------------------------------------------------------
simfun <- function(n=c(10,20,30,50,75,100), diff=0.4) {
  x <- rnorm(max(n), diff, 1)
  sapply(n, function(nn) t.test(x[seq_len(nn)])$p.value)
}

out <- replicate(1000, simfun())
rbind( c(10,20,30,50,75,100),
  apply(out, 1, function(x) mean(x<=0.05)))

## ----parallel------------------------------------------------------------
library(parallel)
cl <- makeCluster(4)
clusterSetRNGStream(cl, 20160405)
clusterExport(cl, "simfun")
simfun2 <- function(i,...) simfun(...)
out <- parSapply(cl, 1:1000, FUN=simfun2, diff=0.6)
rbind( c(10,20,30,50,75,100),
  apply(out, 1, function(x) mean(x<=0.05)))

