#https://rstudio-pubs-static.s3.amazonaws.com/332491_1a839b62d1ed404dbef681d83f5d01c4.html
# model
# y = a + bx + cd + e
# x -control variable, d treatment, e error
# Corelation matrix for x,d,z(IV for d) and e
R<-matrix(cbind(1,0.001,0.002,0.001,
                0.001,1,0.7,0.3,
                0.002,0.7,1,0.001,
                0.001,0.3,0.001,1),nrow=4)
rownames(R)<-colnames(R)<-c("x","d","z","e")
R


U = t(chol(R))
nvars = dim(U)[1]
numobs = 1000
set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,0,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
data = as.data.frame(newX)
attach(data)

cor(data)

y<-10+1*x+1*d+e

# 2SLS

#
ols<-lm(formula = y~x+d)
summary(ols)

tsls1<-lm(d~x+z)
summary(tsls1)


d.hat<-fitted.values(tsls1)

tsls2<-lm(y~x+d.hat)
summary(tsls2)


## Mediation

model.0 <- lm(y ~ x + d)
summary(model.0)

model.M <- lm(d ~ z + x)
summary(model.M)

model.Y <- lm(y ~ d + z + x)

library(mediation)
results <- mediate(model.y = model.Y, model.m = model.M, treat='z', mediator='d',
                   boot=TRUE, sims=500)
summary(results)
