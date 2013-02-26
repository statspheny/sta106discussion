##########################################################
## Problem 19.14
## reading in the data file
mydata = read.table("CH19PR14.txt")

## get the data
y = mydata$V1
a = mydata$V2
b = mydata$V3

## find the fitted values and residuals using brute force method
mu11 = mean( y[ a==1 & b==1 ])
mu12 = mean( y[ a==1 & b==2 ])
mu13 = mean( y[ a==1 & b==3 ])
mu21 = mean( y[ a==2 & b==1 ])
mu22 = mean( y[ a==2 & b==2 ])
mu23 = mean( y[ a==2 & b==3 ])
mu31 = mean( y[ a==3 & b==1 ])
mu32 = mean( y[ a==3 & b==2 ])
mu33 = mean( y[ a==3 & b==3 ])

## use this to get fitted values
mu11
mu12
mu13
mu21
# etc..

## The set of y's are ...
y11 = y[a==1 & b==1]
y12 = y[a==1 & b==2]
y13 = y[a==1 & b==3]
y21 = y[a==2 & b==1]

## or use the aov function in R
## a, b, are the factors a:b is the interaction factor of a and b
## you must use the factor command if using the aov function
a = factor(a)
b = factor(b)
fit = aov(y~a+b+a:b)

## get the fitted values and residuals
fittedvalues = fit$fitted.values
residuals = fit$residuals

## you can also use the lm function for linear model
lmfit = lm(y~a+b+a:b)
lmfit$fitted.values
lmfit$residuals




## plot residuals against fitted values
plot(fittedvalues,residuals)

## add a title and some color
plot(fittedvalues,residuals,main="Fitted values vs residuals")




## make qqplot
normalplot = qqnorm(fit$residuals)

normalplot$x
normalplot$y

rho = cor(normalplot$x, normalplot$y)




#############################################################
## Problem 15
## Plot the Estimated Treatment Means Plot
plot(a,fittedvalues)

## label the axes and the title
plot(a, fittedvalues, xlab="Factor A", ylab="Fitted Values", main="Estimated Treatment Means")

## Using interaction plot function
interaction.plot(a,b,y)

## Again, label the axes and the title
interaction.plot(a,b,y, main="Estimated Treatment Means",ylab="Fitted Values",xlab="Factor A")



## Get ANOVA Table
summary(fit)

## This is what the result looks like
##             Df Sum Sq Mean Sq F value Pr(>F)    
## a            2 220.02  110.01  1827.9 <2e-16 ***
## b            2 123.66   61.83  1027.3 <2e-16 ***
## a:b          4  29.42    7.36   122.2 <2e-16 ***
## Residuals   27   1.63    0.06                   
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 



## Test Interactions

## H0: The interactions are zero
## Ha: Not all interactions are zero
## (Get these values from the result of summary(fit)
Fvalue = (29.42/4)/(1.63/27)
Fcrit = qf(0.95,4,27)
pval = 1-pf(Fvalue,4,27)

## Test Factor A
## H0: All alpha_i's are equal to 0
## Ha: Not all alpha_i's are equal to 0
Fvalue = (220.02/2)/(1.63/27)
Fcrit = qf(0.95,2,27)
pval = 1-pf(Fvalue,2,27)


## Test Factor B
## H0: All beta_j's are equal to 0
## Ha: Not all beta_j's are equal to 0
Fvalue = (123.66/2)/(1.63/27)
Fcrit = qf(0.95,2,27)
pval = 1-pf(Fvalue,2,27)
