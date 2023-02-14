#### Assignment
getwd()
setwd("C:/Users/SAMSON")
ozone <- read.table("ozone (1).txt", header=TRUE)
head(ozone)
Y = ozone$ozone
X1 = ozone$rad
X2 = ozone$temp
X3 = ozone$wind
model.identity = glm(Y ~ X1 + X2 + X3, family = gaussian(link = "identity"))
model.inverse = glm(Y ~ X1 + X2 + X3, family = gaussian(link = "inverse"))
model.log =  glm(Y ~ X1 + X2 + X3, family = gaussian(link = "log"))
model.exponential = glm(Y ~ log(X1) + log(X2) + log(X3), family = gaussian(link = "log"))
#1a 
summary(model.identity)$aic
summary(model.inverse)$aic
summary(model.log)$aic # iii Best Model AIC = 972.5169
summary(model.exponential)$aic
# 1b
var.yi = summary(model.log)$dispersion
mu = predict(model.log,type="response")
pearson = (Y - mu)/sqrt(var.yi)
model.pearson.normal = glm(pearson^2 ~ mu,family = gaussian(link ="identity"))
model.pearson.gamma = glm(pearson^2 ~ mu,family = Gamma(link ="identity"))
model.pearson.inv = glm(pearson^2 ~ mu,family = inverse.gaussian(link ="identity"))
summary(model.pearson.normal)#i insignificant at 1%
summary(model.pearson.gamma)
summary(model.pearson.inv)
# 1c
betahat = summary(model.log)$coef[1:4]
betahat = matrix(betahat)
K = matrix(c(0,0,1,1),nrow = 4,ncol = 1)
W = (t(t(K)%*%betahat))*solve(t(K)%*%vcov(model.log)%*%K)*(t(K)%*%betahat)
W # 10.66784
plot(model.identity)
plot(model.log)
plot(model.exponential)
plot(model.inverse)

