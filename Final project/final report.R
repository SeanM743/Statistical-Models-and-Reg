
data = read.csv("B19.csv")

colnames(data) <- c("Quality","variety","ph","sulfates", "density",
                    "color","pcolor","acolor","anthocyanins",
                    "ionization","ionanth")

data$variety.f = factor(data$variety)

model = lm("Quality~variety.f + ph + sulfates+density+color+pcolor+acolor + anthocyanins+ionization +
            ionanth",data=data)
summary(model)

#run correlation test on all values
cor(data[,unlist(lapply(data, is.numeric))])

#Found a completely collinear value - removed ionanth

model = lm("Quality~variety.f + ph + sulfates+density+color+pcolor+acolor + anthocyanins+ionization",data=data)
summary(model)

#remove acolor
model = lm("Quality~variety.f + ph + sulfates+density+color+pcolor+anthocyanins+ionization",data=data)
summary(model)


#let's look at the dependent variable on a plot
plot(data$Quality)
abline(h=mean(data$Quality),col="Red")


reduced = lm("Quality~1",data=data)
summary(reduced)

model1 = lm("Quality~variety.f + ph + sulfates+color+pcolor+anthocyanins+ionization",data=data)
anova(model1,model)

model1 = lm("Quality~variety.f + ph + sulfates+density+pcolor+anthocyanins+ionization",data=data)
anova(model1,model)

#new model with color gone
model = lm("Quality~variety.f + ph + sulfates+density+pcolor+anthocyanins+ionization",data=data)
summary(model)

reduced = lm("Quality~variety.f + ph + sulfates+density+pcolor+anthocyanins",data=data)
summary(reduced)

anova(reduced,model)

model = lm("Quality~variety.f + ph + sulfates+density+pcolor+anthocyanins",data=data)
summary(model)

#Adequacy checking

qqnorm(model$residuals,
       ylab = "Residuals",
       xlab = "Normal Scores",
       main = "Wine Residuals")
qqline(model$residuals)

plot(model$fitted.values,model$residuals)
abline(0,0)

plot(model$fitted.values,rstudent(model))
abline(0,0)

model = lm("Quality~variety.f + ph + sulfates+density+pcolor+acolor+ionization",data=data)

test =  lm("Quality~ variety.f +ph+density+pcolor",data)
summary(test)

library("leaps")

#bring back all factors
model7 = regsubsets(Quality~variety.f + ph + sulfates+density+pcolor+acolor + ionization,data=data)
reg.summary = summary(model7)
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')

library("car")
subsets(model7, statistic="rss")
subsets(model7, statistic="adjr2")
subsets(model7, statistic="cp")
subsets(model7, statistic="bic")

#6 factor model
model = regsubsets(Quality~variety.f + ph + sulfates+density+pcolor+anthocyanins,data=data)
reg.summary = summary(model)
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')

library("car")
#subset options are bic, cp, adjr2, and rss
subsets(model, statistic="adjr2")

#stepwise regression with 7 regressor model

null.model = lm(Quality~1,data=data)
full.model = lm(Quality~variety.f + ph + sulfates+density + pcolor+acolor + ionization,data=data)
test6 = lm(Quality~variety.f + ph + sulfates+density+pcolor + anthocyanins,data=data)

step(null.model,scope=list(lower=null.model,upper=full.model),direction="forward")
step(full.model,data=data,direction="backward")
step(null.model,scope=list(upper=full.model),data=data,direction="both")

#exhaustive search
regsubsets.out <-
  regsubsets(Quality~variety.f + ph + sulfates+density+pcolor+acolor + ionization,
             data = data,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
a = summary(regsubsets.out)

#final model search
model1 = lm(Quality~variety.f + ph + sulfates+ionization,data = data)
summary(model1)
model2 = lm(Quality~variety+ph+sulfates+ionization,data=data)
summary(model2)

#calculate press statistic and other parameters for table

qpcR::PRESS(model2)
ssres = sum(model2$residuals^2)
msres = ssres/model2$df.residual
ssres
msres

#Cross validation 
set.seed(300)
wine.samples <- sample(1:32, 24, replace=F)
wine.new = data[wine.samples,]

b = lm(Quality~variety+ph+sulfates+ionization,data=wine.new)
c = lm(Quality~density,data=wine.new)

ind = seq(1:32)
wine.deleted_ind = setdiff(ind,wine.samples)
wine.deleted = data[wine.deleted_ind,]

b$fitted.values #gives us fitted values for model b observations
pred_vals = predict.lm(b,wine.deleted)

tss_4 = sum((wine.deleted$Quality - pred_vals)^2)

pred_vals = predict.lm(c,wine.deleted)
tss_1 = sum((wine.deleted$Quality - pred_vals)^2)

plot(wine.deleted$Quality)
points(pred_vals,col="Red")

#examine residuals one last time
plot(model2$fitted.values,rstudent(model2))
abline(h=0)

qqnorm(model2$residuals,
       ylab = "Residuals",
       xlab = "Normal Scores",
       main = "Wine Residuals")
qqline(model2$residuals)

#interaction terms

model.int = lm(Quality~variety.f + ph + sulfates + ionization**2 + density*acolor,data = data)
summary(model.int)















