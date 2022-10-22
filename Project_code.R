#==============================#
# Statistical Modeling         #
# Final Project - August       #
# Ricardo Castañeda            #
# r0731529                     #
#==============================# 
library(SemiPar)
library(MASS)
library(MuMIn) # Model selection
library(leaps)
library(dplyr)
library(glmmLasso)
library(glmnet)
library(gamlss)
library(MASS)
library(nlme)
library(ggplot2)

#==========================#
# Setting data set         #
#==========================#

data<- read.csv("H:/Masters_of_Statistics/Semester_4/Statistical Modeling/Final Project/August/Dataset/OnlineCourse2021.txt", sep="", header = TRUE)

studentnumber = 731529 
set.seed(731529)
rownumbers = sample(1:3042,size=500)
mydata = data[rownumbers,]
mydata2 = data[rownumbers,]
names(mydata) = c('y.p','y.s', 'x1','x2','x3','x4','x5','x6','x7',
                   'x8','x9', 'x10', 'x11')
names(mydata)
#===============#
# Question 1    #
#===============#

## Data for Q1
Q1 = mydata[,-1]

## Density for respons variable
par(mfrow=c(1,2))
plot(density(Q1$y.s))
hist(Q1$y.s)
par(mfrow=c(1,1))

# Possible models

fit.norm <-glm(y.s~., data = Q1, family = gaussian(link = "identity"))
fit.inv.nor <-glm(y.s  ~., data = Q1, family = inverse.gaussian(link = "1/mu^2"))
fit.Gam <-glm(y.s  ~., data = Q1, family = Gamma(link = "inverse"))
fit.poiss <-glm(y.s  ~., data = Q1, family = poisson(link = "log"))

## Log Normal 
fit.log.nor <-glm(log(y.s)  ~., data = Q1, family = gaussian(link = "identity"))
summary(fit.log.nor)

log.n = gamlss(formula = formula(y.s  ~.), sigma.formula = ~1, 
       nu.formula = ~1, data=Q1, tau.formula = ~1, family = 	LOGNO())

summary(log.n)
log.n$aic
##

log.n$aic; fit.norm$aic ; fit.Gam$aic;fit.inv.nor$aic; fit.poiss$aic

AICs = data.frame(Normal = fit.norm$aic,
                  Log_Normal = log.n$aic,
                  Inv_Normal = fit.inv.nor$aic,
                  Gamma = fit.Gam$aic,
                  Poisson = fit.poiss$aic ) 
AICs

#===============#
# Question 2    #
#===============#

## Data for Q2
Q2 = Q1
str(Q2)

## Variable correlation
cor.var= select(Q2, y.s,x4, x5, x6, x7,x9,x10,x11) # continuous var.
cor.var
str(cor.var)
cor(cor.var) # No important correlations

## AIC Variable selesction 
fit.int <-glm(y.s ~., data = Q2, family = gaussian())
summary(fit.int) # Only linear models

stepboth = stepAIC(fit.int, k =2, direction = "both", scope = list(upper = ~. , lower= ~1))
summary(stepboth)

## Selected Model 
fit.select <-glm(y.s ~ x1 + x2 + x3 + x4 + x5 + x6 + x8 + x9 + x10 + x11,
                  data = Q2, family = gaussian())
summary(fit.select)              


## Plots
attach(Q2)

par(mfrow=c(2,2))
plot( as.numeric(x1), y.s)
plot( as.numeric(x2), y.s)
plot( as.numeric(x3), y.s)
plot( as.numeric(x4), y.s)
plot( x5, y.s) #semi
plot( x6, y.s)
plot( x7, y.s) #removed
plot( as.numeric(x8), y.s)
plot( x9, y.s) #semi
plot( x10, y.s)#semi
plot( x11, y.s)#semi


## Semiparametric models

semi1.1 = spm( y.s ~ x1 + x2 + x3 + x4 + f(x5, basis = 'trunc.poly') + 
                   x6 + x8 + x9 + x10 + x11, spar.method = "ML")


semi1.2 = spm(y.s ~ x1 + x2 + x3 + x4 + f(x5, basis = 'trunc.poly',degree=2) + 
                x6  + x8 + x9 + x10 + x11, spar.method = "ML")

semi1.3 = spm(y.s ~ x1 + x2 + x3 + x4 + f(x5) + x6 + x8 + x9 + x10 + 
                    x11, spar.method = "ML")

##

semi2.1 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
              x6 + x8 + f(x9, basis = 'trunc.poly',degree=1) + x10 + x11, spar.method = "ML")

semi2.2 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
        x6 + x7 + x8 + f(x9, basis = 'trunc.poly',degree=2) + x10 + x11, spar.method = "ML")

semi2.3 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x8 + f(x9) + x10 + x11, spar.method = "ML")

##
semi3.1 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x8 + x9 + f(x10, basis = 'trunc.poly',degree=1) + x11, spar.method = "ML")

semi3.2 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x7 + x8 + x9 + f(x10, basis = 'trunc.poly',degree=2) + x11, spar.method = "ML")

semi3.3 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x8 + x9 + f(x10) + x11, spar.method = "ML")

##

semi4.1 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x8 + x9 + x10 + f(x11, basis = 'trunc.poly',degree=1), spar.method = "ML")

semi4.2 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x7 + x8 + x9 + x10 + f(x11, basis = 'trunc.poly',degree=2), spar.method = "ML")

semi4.3 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + 
                x6 + x8 + x9 + x10 + f(x11), spar.method = "ML")
##

semi5.1 = spm(y.s ~ x1 + x2 + x3 + x4 + f(x5, basis = 'trunc.poly',degree=2) + 
                x6 + x8 + f(x9, basis = 'trunc.poly',degree=2) + 
                f(x10, basis = 'trunc.poly',degree=1) + 
                f(x11, basis = 'trunc.poly',degree=1), spar.method = "ML")

semi5.2 = spm(y.s ~ x1 + x2 + x3 + x4 + f(x5, basis = 'trunc.poly',degree=2) + 
                x6 + x8 + f(x9, basis = 'trunc.poly',degree=2) + 
                f(x10, basis = 'trunc.poly',degree=2) + 
                f(x11, basis = 'trunc.poly',degree=2), spar.method = "ML")

semi5.3 = spm(y.s ~ x1 + x2 + x3 + x4 + f(x5) + 
                x6 + x8 + f(x9) + f(x10) + f(x11), spar.method = "ML")

## AIC Values

AIC.func <- function(x) {
  AIC <- -2*(x$fit$logLik - sum(x$aux$df))
  return(AIC)
}


AIC = c( AIC.func(semi1.1), AIC.func(semi1.2), AIC.func(semi1.3), 
         AIC.func(semi2.1), AIC.func(semi2.2), AIC.func(semi2.3),
         AIC.func(semi3.1), AIC.func(semi3.2), AIC.func(semi3.3),
         AIC.func(semi4.1), AIC.func(semi4.2), AIC.func(semi4.3),
         AIC.func(semi5.1), AIC.func(semi5.2), AIC.func(semi5.3))

AIC = matrix(AIC)
row.names(AIC) = c('semi1.1','semi1.2','semi1.3',
                   'semi2.1','semi2.2','semi2.3',
                   'semi3.1','semi3.2','semi3.3',
                   'semi4.1','semi4.2','semi4.3',
                   'semi5.1','semi5.2','semi5.3')

colnames(AIC) = " AIC Values"
AIC

str(Q2)
## Model plot and Summary
summary(semi5.3)
par(mfrow=c(2,2))

plot(semi5.3, ylim=c(20,100))


par(mfrow=c(1,1))
#===============#
# Question 3    #
#===============#

## sections A and B
bivar1 = spm(y.s ~ x1 + x2 + x3 + x4 + x5 + x6 +x8 +f(x9,x10)+ 
              x11, spar.method = "ML")
plot(bivar1)

bivar2 = spm(y.s ~ x1 + x2 + x3 + x4 + f(x5) + x6 +x8 +f(x9,x10) + 
             f(x11), spar.method = "ML")

plot(bivar2)


AIC.biv1 = AIC.func(bivar1)
AIC.biv2 = AIC.func(bivar2)

##non-parametric test
y = y.s; new = poly(cbind(x9,x10),4);
colnames(new)

x9o = new[,1]
x10o = new[,5]
model <- null <- lm(y~x1+x2+x3+x4+x5+x6+x8+x9o+x10o+x11) 

new <- new[, -c(1,5)]
k <- dim(new)[2]
LogL <- NULL
LRT <- NULL
df <- NULL
m <- NULL 
LRT_m <- NULL

for ( i in 1:k){
  model <- update(model,as.formula(paste0(".~. + new[,",i,"]")))
  LogL[i] <- logLik(model)[1]
  m[i] <- length(model$coefficients)-length(null$coefficients)
  LRT[i] <- 2*(LogL[i]-logLik(null))
  LRT_m[i] <- LRT[i]/m[i]
  }

LRT_m
TnOS <- max(LRT_m)
order<- which.max(LRT_m)

#pvalue
m2 <- 1000 
cn <- TnOS

pvalue = 1-exp(-sum((1-pchisq((1:m2)*cn, 1:m2))/(1:m2)))
pvalue

## Section C - Parametric model
null.fit <- glm(y~x1+x2+x3+x4+x5+x6+x8+x9o+x10o+x11) 
int.fit  <- glm(y~x1+x2+x3+x4+x5+x6+x8+x9o+x10o+x11+x10*x11) 

null.fit$aic;int.fit$aic

detach(Q2)


#============#
# Question 4 #
#============#

Q4<- mydata[,-2]
names(Q4)
attach(Q4)
hist(y.p)

## 4.A penalized L1 estimation      



## Starting values
lambda <- seq(0,2000,by=10)
dist = binomial(link = logit)
BIC<-rep(Inf,length(lambda))

PQL<-glmmPQL(y.p~1,random = ~1|x1,family=dist,data=Q4)


for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  fits <- try(glmmLasso(y.p~ as.factor(x1)+as.factor(x2)+as.factor(x3)+x4+x5+x6+
                          x7+as.factor(x8)+x9+x10+x11,  
                        rnd = list(x1=~1),  
                        family = dist, data = Q4, lambda=lambda[j],switch.NR=T,final.re=TRUE),
                        silent=TRUE)  
  
  
  if(class(fits)!="try-error")
  {  
    BIC[j]<-fits$bic
  }
  
}
BIC
opt<-which.min(BIC)
opt

lasso.fit <- glmmLasso(y.p~ as.factor(x1)+as.factor(x2)+as.factor(x3)+x4+x5+x6+
                          x7+as.factor(x8)+x9+x10+x11, rnd = list(x1=~1),  
                          family = dist, data = Q4, lambda=lambda[opt],
                          switch.NR=F,final.re=TRUE)


summary(lasso.fit)
plot(lambda,BIC)

lambda[6];BIC[6]

## 4.B Table

lasso.fit2 <- glmmLasso(y.p~ as.factor(x1)+as.factor(x2)+as.factor(x3)+x4+x5+x6+
                         x7+as.factor(x8)+x9+x10+x11, rnd = list(x1=~1),  
                       family = dist, data = Q4, lambda=0,
                       switch.NR=F,final.re=TRUE)

summary(lasso.fit)


Penalized = lasso.fit$coefficients
Penalized = lasso.fit$
No_Penalized = lasso.fit2$coefficients

table = data.frame(No_Penalized,Penalized)
table

p =predict(lasso.fit)
which.min(p)
p[278]
