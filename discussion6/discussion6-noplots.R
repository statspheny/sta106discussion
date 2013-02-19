
Y1 = c(7.6,8.2,6.8,5.8,6.9,6.6,6.3,7.7,6.0)
Y2 = c(6.7,8.1,9.4,8.6,7.8,7.7,8.9,7.9,8.3,8.7,7.1,8.4)
Y3 = c(8.5,9.7,10.1,7.8,9.6,9.5)

Y1bar = mean(Y1)
Y2bar = mean(Y2)
Y3bar = mean(Y3)

residuals = c(Y1-Y1bar,Y2-Y2bar,Y3-Y3bar)
factors = c(rep(1,9),rep(2,12),rep(3,6))

plot(residuals,factors)

qqnorm(residuals)

# expected residuals
expected = qnorm(ppoints(residuals),0,1)

cor(expected,sort(residuals))

loc1 = c('U','E','E','E','E','U','U','U','U')
loc2 = c('E','E','E','E','U','U','U','U','U','E','E','E')
loc3 = c('E','U','E','U','U','E')

locations = c(loc1,loc2,loc3)

Uresiduals = residuals[locations=='U']
Ufactors = factors[locations=='U']

plot(Uresiduals,Ufactors,main="United States Location")


Eresiduals = residuals[locations=='E']
Efactors = factors[locations=='E']

plot(Eresiduals,Efactors,main="European Location")
