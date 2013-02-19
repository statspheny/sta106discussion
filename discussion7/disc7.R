## Import the data
mydata = read.table("CH18PR17.txt")
y = mydata$V1
x = mydata$V2
plot(x,y)

## part(a)
Y1 = y[x==1]
Y2 = y[x==2]
Y3 = y[x==3]
Y4 = y[x==4]

Y1dot = mean(Y1)
Y2dot = mean(Y2)
Y3dot = mean(Y3)
Y4dot = mean(Y4)

residuals1 = Y1-Y1dot
residuals2 = Y2-Y2dot
residuals3 = Y3-Y3dot
residuals4 = Y4-Y4dot

## You could also learn the lm function: lm to fit linear models
factorx = factor(x)
fit1 = lm(y~factorx)
fit1$fittedvalues
fit1$residuals


## part (b)  Residual plots
residuals = c(residuals1,residuals2,residuals3,residuals4)
plot(residuals,x)


## part (c) Brown Forsythe test
Y1dev = abs(Y1 - median(Y1))
Y2dev = abs(Y2 - median(Y2))
Y3dev = abs(Y3 - median(Y3))
Y4dev = abs(Y4 - median(Y4))

alldev = c(Y1dev,Y2dev,Y3dev,Y4dev)

SSE = sum((Y1dev-mean(Y1dev))^2) + sum((Y2dev-mean(Y2dev))^2) + 
  sum((Y3dev-mean(Y3dev))^2) + sum((Y4dev-mean(Y4dev))^2)

SSTR = 16*(mean(Y1dev)-mean(alldev))^2 + 16*(mean(Y2dev)-mean(alldev))^2 +
  16*(mean(Y3dev)-mean(alldev))^2 + 16*(mean(Y4dev)-mean(alldev))^2

Fstat = (SSTR/3)/(SSE/60)
Fstat

Fcritical = qf(0.95,3,60)
Fcritical

pvalue = 1-pf(Fstat,3,60)
pvalue

## using R package
# install.packages("HH")
library(HH)
bftest = hov(y~factorx)
bftest

## part (d)
Y1bar = mean(Y1)
s1 = sd(Y1)
Y2bar = mean(Y2)
s2 = sd(Y2)
Y3bar = mean(Y3)
s3 = sd(Y3)
Y4bar = mean(Y4)
s4 = sd(Y4)

## part(e)

lambda = 1
K2 = prod(y)^(1/64)
K1 = 1/(lambda*K2^(lambda-1))
ytransform = K1*(y^lambda-1)
# use if lambda = 0
# ytransform = K2*log(y)
Y1 = ytransform[x==1]
Y2 = ytransform[x==2]
Y3 = ytransform[x==3]
Y4 = ytransform[x==4]
Y1dot = mean(Y1)
Y2dot = mean(Y2)
Y3dot = mean(Y3)
Y4dot = mean(Y4)
residuals1 = Y1-Y1dot
residuals2 = Y2-Y2dot
residuals3 = Y3-Y3dot
residuals4 = Y4-Y4dot
residuals = c(residuals1,residuals2,residuals3,residuals4)
qqnorm(residuals)
SSE = sum(residuals^2)
SSE

## or do it using R packages
# install.package("TeachingDemos")
library(TeachingDemos)
ytransform = bct(y,1)
fit = lm(ytransform~factorx)
SSE = sum(fit$residuals^2)
SSE
