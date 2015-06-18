#data analysis prelim
#Hongying Yang 79202826
setwd("~/Dropbox/course/statistics/data analysis prelim/Rcourse/data analysis prelim")
#install.packages("nlme")
install.packages("foreign")
install.packages("reshape")
library(nlme)
library(foreign)
attach(mtcars)
library(reshape)

#outline
#note: this is a balanced design longitude dataset 
#check constant variance over time: cov(Yij,Yik), assume constant over all individuals: in R corr(data) in wide format
#look at example: treatment of lead expose child 203
#check if there is correlation among araiables, assumption of independency will lead to the estimation of sampling
#variances(standard errors) are inlated (bigger than actual), but the estimation will still be unbiaed
#check normality, each covarite is noraml or not
#check "linearity" by scatter plots of all pairs of residual (eij, eik)

#modeling the mean
#treat time as a categorical predictor, hypothesis: does the mean response profile differ across groups? i.e. does group*time 
#interation present? No, almost parallel mean response profile
#if no group*time interation, are the means changing over time?-time main effect?
#do the means differ across groups?- group main effect?
#see example: lead data
#important: write the hypotheses of interest in terms of regression parameters: lecture 3
#linear spline at a time knot

#mixed effects model:allowto seperatly model within-subject and between subject variability(lecture 5 for detail)
#Hypothesis tesing with LMEs, F-test: in R: anova(mod,L) lecture 5
#use of REML and ML, ML used for any test about beta
#wald F-test
#resudual analysis and diagnostics: 1) mean model(most important): residual vs fitted model
#2) covariance model: check ACF, F-test for models or AIC, residual vs fitted plots
#3)constant variance/nomality for residuals and randome effects: qq plots / scatter plots of residuals
#R example: dental data and MIT data
#GLMM: lecture 8

data = read.csv("DA2014.csv")
head(data)
#only focus on the last measurement(OBSNum=5) of EFW and Scort for each subject, is there a significant association between EFW and Scort
#this is a question of GLM
#since each individual is indenpendent, we can use simple linear regression 
data.num5 = data[data$ObsNum==5,]
par(mfrow=c(3,2))
plot( data.num5$EFW,data.num5$Scort,type="l",ylab="Scort")
plot( data.num5$EFW,data.num5$GA,ylab="GA")
plot( data.num5$EFW, data.num5$Gender,ylab="gender")
plot( data.num5$EFW,data.num5$BMI, ylab="BMI")
plot( data.num5$EFW,data.num5$RiskOB, ylab="RiskOB")
plot(data.num5$Scort, data.num5$GA, xlab="Scort", ylab="GA")

plot( data.num5$EFW,data.num5$Scort,type="l",ylab="Scort")
par(mfrow=c(2,2))
hist(data.num5$EFW,xlab="EFW",main="")
hist(data.num5$Scort,xlab="Scort",main=" ")
hist(data.num5$GA,xlab="GA",main="")
hist(data.num5$BMI,xlab="BMI",main="")

#hist(log(data.num5$Scort))
modfull = lm(EFW~Scort+GA+BMI+ as.factor(Gender) + as.factor(RiskOB)+as.factor(Race),data=data.num5)
mod0 = lm(EFW~1,data=data.num5)
mod1 = lm(EFW~1+GA,data=data.num5)
anova(mod0,mod1)
mod2 = lm(EFW~1+Scort+GA, data=data.num5)
mod3 = lm(EFW~1+Scort,data=data.num5)
mod4 = lm(EFW~1+Scort*GA,data=data.num5)
#SSE0 = sum(R0^2);
#n = length(data.num5$EFW)
#df0 = n - 1;
mod1 = lm(data.num5$EFW~data.num5$Scort)
fitted1 = mod2$fitted;
R1 = mod2$resid;
# Pearson (conditional) residuals =
# (y - x*beta-hat - z*b-hat)/sigma-hat
resid(mod2, type="pearson")
plot(R1,main="plot of residual from lm(EFW~Scort)")
acf(R1,main="acf of residual from lm(EFW~Scort)")

par(mfrow=c(1,1)); 
plot(data.num5$Scort, data.num5$EFW, xlab="Scort", ylab="EFW");
lines(data.num5$Scort, fitted1, col=2);
#comments: concluded no clear trend of EFW and Scort for ObsNum==5



# ## Model 2: X2 only
# mod2 = lm(data.num5$EFW ~ data.num5$GA); 
# fitted2 = mod2$fitted;
# R2 = mod2$resid; 
# SSE2 = sum(R2^2);
# df2 = n - 2; 
# RegSS2 = SSE0 - SSE2;
# Rsq2 = RegSS2/SSE0;
# Rsq2
# summary(mod2);
# 
# par(mfrow=c(1,1)); 
# plot(data.num5$GA, data.num5$EFW, xlab="GA", ylab="EFW");
# lines(data.num5$GA, fitted2, col=2);
# 
# ## Model 12: X1 first; X2 second
# mod12 = lm( data.num5$EFW~ data.num5$Scort + data.num5$GA); 
# fitted12 = mod12$fitted;
# R12 = mod12$resid; 
# SSE12 = sum(R12^2);
# df12 = n - 3; 
# RegSS12 = SSE0 - SSE12;
# Rsq12 = RegSS12/SSE0;
# Rsq12
# summary(mod12);


# ## Comparing Rsquared
# Rsq1
# Rsq2
# Rsq12
# ## Improvement by X2 (after X1) is small because 
# ## X2 and X1 are correlated. 
# 
# ## Added variable plots (X2 after X1)
# res.Y.X1 = mod1$resid; 
# outX2.X1 =  lm(data.num5$GA ~ data.num5$Scort); 
# res.X2.X1 = outX2.X1$resid; 
# 
# par(mfrow=c(1,1));
# plot(y=res.Y.X1, x=res.X2.X1);
# resreg = lm(res.Y.X1 ~ res.X2.X1); 
# summary(resreg)


#Focusing on the first(ObsNum=1) and last(ObsNum = 5) measurements of EFW and Scort for each subject, how is the change
#in cortisol related to thechange in EFW during pregnancy
#this maybe anova or glmm
data.num1 = data[data$ObsNum==1,]
data.num15 = rbind(data.num1,data.num5)
data.num15.sub = data.num15[,c(1,2,3,8,9)]

# Scortdiff = data.num5$Scort - data.num1$Scort
# EFWdiff = data.num5$EFW - data.num1$EFW
# lm(EFWdiff~Scortdiff)
data.num15.w = reshape(data.num15.sub, timevar="ObsNum", idvar=c("subid"),direction="wide")

par(mfrow=c(2,2))
hist(data.num15$EFW,xlab="EFW",main="")
hist(data.num15$Scort,xlab="Scort",main=" ")
hist(data.num15$GA,xlab="GA",main="")
hist(data.num15$BMI,xlab="BMI",main="")
#explore positive correlation between EFW measurements on same individual

##plot(data.num15.w$EFW.1,data.num15.w$EFW.5,xlab="EFW.1",ylab="EFW.5")
cor(data.num15.w$EFW.1, data.num15.w$EFW.5)
data.num15.grp = groupedData(EFW ~ GA | subid, data = data.num15.sub,
                      labels=list(x = "GA", y = "EFW"), units = list(x = "(weeks)"))
par(mfrow=c(1,2))
plot(data.num15.grp$EFW~data.num15.grp$GA,xlab="GA",ylab="EFW")
for (i in unique(data.num15.grp$subid)){
  lines(data.num15.grp$EFW[data.num15.grp$subid==i]~data.num15.grp$GA[data.num15.grp$subid==i],col=i)
}

plot(data.num15.grp$EFW~data.num15.grp$Scort,,xlab="Scort",ylab="EFW")
for (i in unique(data.num15.grp$subid)){
  lines(data.num15.grp$EFW[data.num15.grp$subid==i]~data.num15.grp$Scort[data.num15.grp$subid==i],col=i)
}

modfull = lm(EFW ~ Scort+GA+BMI+ as.factor(Gender) + as.factor(RiskOB)+as.factor(Race),data=data.num15)
mod = lm(Scort ~ GA,data=data.num15)
#comment: Scort is highly correlated with GA
mod1 = lm(EFW ~ Scort,data=data.num15)
mod2 = lm(EFW ~Scort, data =  data.num15)
anova(mod1,mod2)
#anova of mod1 and mod2 shows that adding scort improve the prediction of EFW

mod3 = lme(EFW ~ Scort, random = ~ 1|subid, data=data.num15,method="ML")
mod4 = lme(EFW ~ Scort, random = ~ 1|subid, data=data.num15,method="ML")

mod2 = lm(EFW~ 1+Scort,data=data.num15.grp)
summary(mod1)$coef


#comment, this shows that both random intercept and slop is needed
mod0 = lme(EFW~ 1+Scort,random = ~ Scort-1|subid,data=data.num15,method="ML")
mod1 = lme(EFW ~ 1+Scort, random = ~ Scort|subid, data=data.num15,method="ML")
anova(mod0,mod1)
mod0 = lme(EFW~ 1+Scort,random = ~ 1|subid,data=data.num15,method="ML")
mod1 = lme(EFW ~ 1+Scort, random = ~ Scort|subid, data=data.num15,method="ML")
anova(mod0,mod1)

#check if randome effect is needed, by doing likelihood ratio test on lme and lm: this is Ombao's method
mod0 = lm(EFW ~ Scort,data=data.num15)
mod2 = lme(EFW ~ Scort, random = ~Scort|subid, data=data.num15) #use this in residual
mod0loglik = as.numeric(logLik(mod0),REML=TRUE)
#test statistics
Tstat = -2*(mod0loglik-mod2$logLik)

## Compare this to the reference distribution whih is 0.5*X2(0) + 0.5*X2(1) 
M = 10000;
refdist = c(1:M); 
for(m in 1:M){
  temp1 = rbinom(1,1,0.5);
  temp2 = 0.5*temp1*rchisq(1,1);
  refdist[m] = temp2;
}
refdist = sort(refdist);
hist(refdist);
sum(refdist==0);
## critical value
alpha = 0.05;
refdist[floor((M*(1-alpha)))]
## approximate p-value
sum(refdist>Tstat)/M
## Compare this with the p-value under the chisq with df = 1 (since there is one less par in M0 than M1)
1-pchisq(Tstat,1)

#comment: the p value are both 0 ????

# $fixed = population-averaged intercept and slope (beta-hats)
# $random$Subject = within-subject fitted intercept and slope (beta-hat+bi-hat)
fitpop = mod2$fitted[,1]
fitsub = mod2$fitted[,2]
residsub = mod2$resid[,2]
residpop = mod2$resid[,1]
plot(residpop)
acf(residpop,100)
plot(residsub)

#add GA
mod2 = lme(EFW ~ 1+Scort*ObsNum,, random = ~ 1+Scort|subid, data=data,method="ML")





#using all avaiable longitudinal measurements during pregnancy, does the relationship between EFW and Scort change orver the course of GA
#this is a question of glmm

# Note that the following plot is misleading due to the equally spaced times:

# Better plot of means:

# cast the melted data
# cast(data, formula, function) 
par(mfrow=c(2,2))
hist(data$EFW,xlab="EFW",main="")
hist(data$Scort,xlab="Scort",main=" ")
hist(data$GA,xlab="GA",main="")
hist(data$BMI,xlab="BMI",main="")

#check of lme is needed or not, plot of subject specific plots
mod.lm = lmList(EFW ~ 1+Scort |subid , data=data)
plot(intervals(mod.lm))
intervals(mod.lm)
#for the plot of intervals, we can see that for Scort, we have some variations with in individuals, we will
#deal with this by using random effect model (with both intercept and slop on Scort), i.e: coefficients differ across subjects
#fit mixed effects model
mod1 = lme(EFW ~ 1+Scort, random = ~1+Scort|subid, data=data)
mod.lm = lm(EFW~1+Scort,data=data)
summary(mod1)$coef
# $fixed = population-averaged intercept and slope (beta-hats)
# $random$Subject = within-subject fitted intercept and slope (beta-hat+bi-hat)
# Random effect estimates by subject:
ranef(mod1)
# Fitted values
# Population-averaged fitted values:
head(fitted(mod1,level=0))
# Subject-specific (conditional) fitted values:
head(fitted(mod1,level=1))

# Scatterplot of coefficients - slope vs. intercept:
pairs(mod1)
# Compare lm fits to lme fit:
# LME fits are "shrunk" towards the overall population mean line.
#CI for each coeffecient
!!!!! intervals(mod1)
#check residual of lme model
#acf within subject

residpob = mod1$resid[,1]
residsub = mod1$resid[,2]
resid1mat = matrix(residsub, ncol=100 ,byrow=F)
#check if there is correlation within one subject trough time
acf(resid1mat[,1])
#see if there is a clear trend of residual between subject for each time
plot(t(resid1mat)[,1])
acf(t(resid1mat)[,1],100)
mean(residsub)
plot(residsub)
qqnorm(mod1,~resid(.,type="p"),abline(c(0,1)))

#estimating variance components
install.packages("varComp")
library(varComp)
varcompout = VarCorr(mod1)
#check if randome effect is needed, by doing likelihood ratio test on lme and lm: this is Ombao's method
mod0 = lm(EFW ~ Scort-1,data=data)
mod2 = lme(EFW ~ Scort*GA, random = ~Scort|subid, data=data)
mod0loglik = as.numeric(logLik(mod0),REML=TRUE)
#test statistics
Tstat = -2*(mod0loglik-mod2$logLik)

## Compare this to the reference distribution whih is 0.5*X2(0) + 0.5*X2(1) 
M = 10000;
refdist = c(1:M); 
for(m in 1:M){
  temp1 = rbinom(1,1,0.5);
  temp2 = 0.5*temp1*rchisq(1,1);
  refdist[m] = temp2;
}
refdist = sort(refdist);
hist(refdist);
sum(refdist==0);
## critical value
alpha = 0.05;
refdist[floor((M*(1-alpha)))]
## approximate p-value
sum(refdist>Tstat)/M
## Compare this with the p-value under the chisq with df = 1 (since there is one less par in M0 than M1)
1-pchisq(Tstat,1)

#plot(comparePred(mod0,mod2))

#model selection
# We cannot use REML for LRT comparing different fixed effects models -->
# R gives warning:

# Need to re-fit using ML:
mod0 = lm(EFW~1+Gender,data=data.num15)
mod1.ML = lme(EFW ~ 1+Scort+GA, data = data.num15, random = ~ 1+Scort|subid, method="ML")
mod2.ML = lme(EFW ~ 1+Scort*GA, data = data.num15, random = ~ 1+Scort|subid, method="ML")
# Ho: 1+Scort 
# Ha: 1+Scort*GA
# LRT:
anova(mod1.ML,mod2.ML)
# Low p-value --> Sex plus interaction term needed in model.
### Residual and Random Effect Diagnostics
##

### Assumptions for linear mixed effect models:
## 1. The within-group errors (epsilons) are independent and identically
##    normally distributed with mean zero and constant variance, and are
##    independent of random effects.
## 2. The random effects are normally distributed, with mean zero and
##    covariance matrix G, and are independent for different groups.

## Assessing assumptions on within-group errors --
# Raw conditional residuals (default):
resid(mod1, level=1)
# same as
resid(mod1)
# Raw marginal residuals:
resid(mod1, level=0)

# Pearson (conditional) residuals =
# (y - x*beta-hat - z*b-hat)/sigma-hat
resid(mod1, type="pearson")

# Plot lme object --> pearson residuals vs. fitted values (x*beta-hat + z*b-hat).
# What to look for?
plot(mod1)
# Boxplots of residuals by subject -
# the "." is interpreted as the fitted model object itself (mod);
# abline=0 adds a line at 0.
# What to look for?
plot(mod1, subid ~ resid(.), abline=0)




fitpop = mod1$fitted[,1]
fitsub = mod1$fitted[,2]
residsub = mod1$resid[,2]
residpop = mod1$resid[,1]
plot(residpop)
acf(residpop)
mod2 = lme(EFW ~ 1+Scort*ObsNum, random = ~1+Scort|subid, data=data)

mod2 = lme(EFW ~ 1+Scort*GA, random = ~1+Scort|subid, data=data)
mod1 = lme(EFW ~ 1+Scort+GA, random = ~1+Scort|subid, data=data)
mod= lm(EFW~1+Scort+GA, data=data)


data.grp = groupedData(EFW ~ GA | subid, data = data,
                             labels=list(x = "GA", y = "EFW"), units = list(x = "(weeks)"))

plot(data.grp$Scort[data.grp$subid==79854],data.grp$EFW[data.grp$subid==79854],,type="l",col=1,xlab="Scort",ylab="EFW")
for (i in unique(data.grp$subid)){
  lines(data.grp$Scort[data.grp$subid==i],data.grp$EFW[data.grp$subid==i],type="l",col=i)
}

plot(data.grp$GA[data.grp$subid==79854],data.grp$EFW[data.grp$subid==79854],type="l",col=1,xlab="GA",ylab="EFW")

for (i in unique(data.grp$subid)){
  lines(data.grp$GA[data.grp$subid==i],data.grp$EFW[data.grp$subid==i],type="l",col=i)
}

plot(data.grp$Scort[data.grp$ObsNum==1],data.grp$EFW[data.grp$ObsNum==1],type="l",col=1,xlab="Scort",ylab="EFW")

for (i in unique(data.grp$ObsNum)){
  lines(data.grp$Scort[data.grp$ObsNum==i],data.grp$EFW[data.grp$ObsNum==i],type="l",col=i)
}

mdata <- melt(data, id=c("subid","ObsNum"))
subjmeans <- cast(mdata, subid~variable, mean)
timemeans <- cast(mdata, ObsNum ~ variable,mean)
plot(timemeans$Scort,timemeans$EFW,type="l",col=1,xlab="Scort",ylab="EFW")

for (i in unique(timemeans$ObsNum)){
  lines(timemeans$Scort[data.grp$ObsNum==i],timemeans$EFW[data.grp$ObsNum==i],type="l",col=i)
}

matplot(c(1,2,3,4,5),timemeans[,], xlab="ObsNum",type="o", col=c("red"))
matplot(c(1,2,3,4,5),timemeans$EFW, xlab="ObsNum",type="o", col=c("red"))

model =  lm(data$EFW ~ data$Scort + data$GA + data$Scort*data$GA)

#in order to look at the interaction effect, I put scort into categorical 

data$Scort.f = cut(data$Scort,3,labels=c('Low','Medium','High'))
table(data$Scort.f)
data$Scort.f = as.factor(data$Scort.f)
data.sub = data[,c(1,2,9,10,11)]

data.l = data.sub[data.sub$Scort.f=="Low",]
data.m = data.sub[data.sub$Scort.f=="Medium",]
data.h = data.sub[data.sub$Scort.f=="High",]

#data.l.w = reshape(data.l, timevar="ObsNum", idvar=c("subid"),direction="wide")
data.l.1 = data.l[data.l$GA.f== 1,]
data.l.2 = data.l[data.l$GA.f== 2,]
data.l.3 = data.l[data.l$GA.f== 3,]
data.l.4 = data.l[data.l$GA,f== 4,]
data.l.5 = data.l[data.l$GA.f== 5,]
data.l.EFW.mean = c(mean(data.l.1$EFW),mean(data.l.2$EFW),mean(data.l.3$EFW),mean(data.l.4$EFW),mean(data.l.5$EFW))

data.m.1 = data.m[data.m$GA.f== 1,]
data.m.2 = data.m[data.m$GA.f== 2,]
data.m.3 = data.m[data.m$GA.f== 3,]
data.m.4 = data.m[data.m$GA.f== 4,]
data.m.5 = data.m[data.m$GA.f== 5,]
data.m.EFW.mean = c(mean(data.m.1$EFW),mean(data.m.2$EFW),mean(data.m.3$EFW),mean(data.m.4$EFW),mean(data.m.5$EFW))

data.h.1 = data.h[data.h$GA.f== 1,]
data.h.2 = data.h[data.h$GA.f== 2,]
data.h.3 = data.h[data.h$GA.f== 3,]
data.h.4 = data.h[data.h$GA.f== 4,]
data.h.5 = data.h[data.h$GA.f== 5,]
data.h.EFW.mean = c(mean(data.h.1$EFW),mean(data.h.2$EFW),mean(data.h.3$EFW),mean(data.h.4$EFW),mean(data.h.5$EFW))

Time = c(1,2,3,4,5)
plot(Time,data.l.EFW.mean,xlab="GA level",ylab="EFW",type="l",col="red",lty=1)
lines(Time,data.m.EFW.mean,type="l",col="blue",lty=1)
lines(Time,data.h.EFW.mean,type="l",col="black",lty=1)
legend(1,3000,c("low stress","medium stress","high stress"),col=c("red","blue","black"),lty=c(1,1,1))


#plot GA VS EFW with different Scort
plot(data.l$GA.f,data.l$EFW,xlab="GA",ylim=c(min(data$EFW),max(data$EFW)),ylab="EFW",type="l",col=2,lty=1)
par(new=TRUE)
plot(data.m$GA.f,data.m$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=3,lty=1)
par(new=TRUE)
plot(data.h$GA.f,data.h$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=4,lty=1)
legend(0.5,4000,c("low stress","medium stress","high stress"),col=c(2:4),lty=c(1,1,1))


#plot ObsNum VS EFW with different Scort
data.l$ObsNum = as.factor(data.l$ObsNum)
data.m$ObsNum = as.factor(data.m$ObsNum)
data.h$ObsNum = as.factor(data.h$ObsNum)
plot(data.l$ObsNum,data.l$EFW,xlab="ObsNum",ylim=c(min(data$EFW),max(data$EFW)),ylab="EFW",type="l",col=2,lty=1)
par(new=TRUE)
plot(data.m$ObsNum,data.m$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=3,lty=1)
par(new=TRUE)
plot(data.h$ObsNum,data.h$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=4,lty=1)
legend(0.5,4000,c("low stress","medium stress","high stress"),col=c(2:4),lty=c(1,1,1))


#no, there is no interaction between EFW and stress over GA

#in order to look at the interaction effect, I put GA into categorical too

data$GA.f = cut(data$GA,5,labels=c("1","2","3","4","5"))
#data$GA.f = cut(data$GA,pretty(range(data$GA), n = 5),labels=c("1","2","3","4","5"))
table(data$GA.f)
data$GA.f = as.factor(data$GA.f)
data.sub.GA = data[,c(1,2,9,10,11)]

data.1 = data.sub.GA[data.sub.GA$GA.f=="1",]
data.2 = data.sub.GA[data.sub.GA$GA.f=="2",]
data.3 = data.sub.GA[data.sub.GA$GA.f=="3",]
data.4 = data.sub.GA[data.sub.GA$GA.f=="4",]
data.5 = data.sub.GA[data.sub.GA$GA.f=="5",]


plot(data.1$Scort.f,data.1$EFW,xlab="Scort",ylim=c(min(data$EFW),max(data$EFW)),ylab="EFW",type="l",col=1,lty=1)
par(new=TRUE)
plot(data.2$Scort.f,data.2$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=2,lty=1)
par(new=TRUE)
plot(data.3$Scort.f,data.3$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=3,lty=1)
par(new=TRUE)
plot(data.4$Scort.f,data.4$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=4,lty=1)
par(new=TRUE)
plot(data.5$Scort.f,data.5$EFW,xlab="",ylab="",ylim=c(min(data$EFW),max(data$EFW)),type="l",col=5,lty=1)

legend(0.1,4000,c("lev1","lev2","lev3","lev4","lev5"),col=c(1:5),lty=c(1,1,1,1,1))


