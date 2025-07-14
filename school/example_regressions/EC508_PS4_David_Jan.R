install.packages("tidyverse")
library(tidyverse)
install.packages("daewr")
library(daewr)
install.packages("car")
library(car)
install.packages("AER")
install.packages("systemfit")
library("AER")
library("systemfit")


load("C:/Users/dal21/OneDrive/BU Fall 2019/EC 508 - Econometrics/Problem Sets/Data Sets- R/sleep75.RData")

#i) Estimate this equation separately for men and women and report the results in the usual form.
#Are there notable differences in the two estimated equations?

nrow(data) #706

data_m <- filter(data, data$male==1)

model_1<- lm(data_m$sleep ~ data_m$totwrk + data_m$educ + data_m$age + data_m$agesq +data_m$yngkid)  
model_1
summary(model_1)

data_f <- filter(data, data$male==0)

model_2<- lm(data_f$sleep ~ data_f$totwrk + data_f$educ + data_f$age + data_f$agesq +data_f$yngkid)
model_2
summary(model_2)

#ii) Compute Chow Test for equality of the parameters in the sleep equation for men (SSR1) and women (SSR2)
#SSR (restricted) - just the whole regression - pooled regression


model_pooled <- lm(data$sleep ~ data$totwrk + data$educ + data$age + data$agesq +data$yngkid) 
model_pooled
summary(model_pooled)

ssr_pooled<- sum(residuals(model_pooled)^2)
ssr1<- sum(residuals(model_1)^2)
ssr2<- sum(residuals(model_2)^2)

n<- nrow(data) #706
k<- 5

chow_stat<- (ssr_pooled - (ssr1 + ssr2))/(ssr1+ssr2)*(n-2*(k+1))/(k+1) #2.11635
chow_stat

#chow test follows an F distribution with df1= k + 1 and df2= n-2(k+1) degrees of freedom, 
#where k is the number of regressors, N1 is the number of observations of group 1, N2 is the number of obs of group 2

df1<- k+1 #6
df2<- n-2*(k+1) #694

fcrit=qf(.95,df1=df1,df2=df2) #2.1116
#however the F dist from the tables is 2.10, so reject the null

#iii - allow for a different intercept for males and females and determine whether the interaction terms
#involving male are jointly significant

View(data)

data1<- data %>%
            mutate(male_educ = male*educ) %>%
            mutate(male_totwrk = male*totwrk) %>%
            mutate(male_age = male*age) %>%
            mutate(male_agesq = male*agesq) %>%
            mutate(male_yngkid = male*yngkid)

model3<- lm(data1$sleep ~ data1$totwrk + data1$educ + data1$age + 
                data1$agesq +data1$yngkid + data1$male + data1$male_totwrk +
                data1$male_educ + data1$male_age + data1$male_agesq + data1$male_yngkid)
                

nullhyp<- c("data1$male_totwrk", "data1$male_educ", "data1$male_age", "data1$male_agesq", "data1$male_yngkid")

linearHypothesis(model3,nullhyp) 
#F statistic is 1.2558 and p value indicates we cannot reject null, no indication that there are differences in slopes

load("C:/Users/dal21/OneDrive/BU Fall 2019/EC 508 - Econometrics/Problem Sets/Data Sets- R/charity.RData")

charity<-data
View(charity)

model4<- lm(charity$respond ~ charity$resplast + charity$avggift)
summary(model4)

model5<- lm(charity$respond ~ charity$resplast + charity$avggift + charity$propresp)
summary(model5)

model6<- lm(charity$respond ~ charity$resplast + charity$avggift + charity$propresp + charity$mailsyear)
summary(model6)


load("C:/Users/dal21/OneDrive/BU Fall 2019/EC 508 - Econometrics/Problem Sets/Data Sets- R/vote1.RData")
vote1<- data
view(desc)

#i - Estimate a model with voteA as the dep. variable and prtystrA, democA, ln(expendA) and ln(expendB) as indep. var.
#Obtain the OLS residuals u^i and regress these on all of the independent variables. Explain why you obtain R^2=0

model7<- lm(vote1$voteA ~ vote1$prtystrA + vote1$democA + vote1$lexpendA + vote1$lexpendB)
summary(model7)

residuals<- residuals(model7)

model8<- lm(residuals ~ vote1$prtystrA + vote1$democA + vote1$lexpendA + vote1$lexpendB)
summary(model8)

#ii - compute the Breusch-Pagan test for heteroskedasticity. Use the F statistic version and report the P-value

install.packages("lmtest")
library(lmtest)

model7<- lm(vote1$voteA ~ vote1$prtystrA + vote1$democA + vote1$lexpendA + vote1$lexpendB)
summary(model7)

#BP test manually

#squared residuals
vote.ressq<- residuals(model7)^2 #use this as the dep.variable of a regression

#new regression
vote.bpreg<- lm(vote.ressq~vote1$prtystrA + vote1$democA + vote1$lexpendA + vote1$lexpendB)
summary(vote.bpreg)

#BP test via package
bptest(model7) #if reject, then heteroskedastic, if cannot reject, then homoskedasticity

#iii - compute the special case of the white test for heteroskedasticity again using the F statistic form.
#How strong is the evidence for heteroskedasticity now?

#estimate the model by OLS as usual
model7
#obtain OLS residuals and fitted values 
res<- model7$residuals
fit.val<- model7$fitted.values
#compute the squared OLS residuals and squared fitted values
res^2
fit.val^2
#run res^2 on 
vote.white.reg<- lm(res^2~ fit.val + fit.val^2)
summary(vote.white.reg)

load("C:/Users/dal21/OneDrive/BU Fall 2019/EC 508 - Econometrics/Problem Sets/Data Sets- R/gpa1.RData")
gpa<-data

#i) Use OLS to estimate a model relating colGPA to hsGPA, ACT, skipped, and PC. Obtain OLS residuals
reg1<- lm(gpa$colGPA~gpa$hsGPA + gpa$ACT + gpa$skipped + gpa$PC)
summary(reg1)

reg1$residuals #this command would give you the residuals
sum(reg1$residuals)

#ii) Compute the special case of the White test for heteroskedasticity. residuals^2 on colGPA, colGPA^2
gpa.residuals<- reg1$residuals
gpa.ressquared<- (gpa.residuals)^2

gpa.fit.val<-reg1$fitted.values #these are the fitted values
gpa.fit.valsq<- (gpa.fit.val)^2

#lm
gpa.reg.white<- lm(gpa.ressquared ~ gpa.fit.val + gpa.fit.valsq)
summary(gpa.reg.white)

white.fit.val<- gpa.reg.white$fitted.values
sum(white.fit.val)


#iii)
install.packages("schoolmath")
library(schoolmath)

is.positive(white.fit.val)

#OLS regression first
regOLS<- lm(gpa$colGPA~ gpa$skipped + gpa$PC)
summary(regOLS)

#WLS second
h<- white.fit.val #fitted values from the white regression
weight<- 1/sqrt(h) #weights

#manually
w_colGPA<- (gpa$colGPA)*(weight)
w_skipped<- (gpa$skipped)*(weight)
w_PC<- (gpa$PC)*(weight)

regWLS1<- lm(w_colGPA~ w_skipped + w_PC)
summary(regWLS1)


#iv) obtain heteroskedasticity-robust standard errors

#I found this online without having to use vcovHC
summaryR.lm <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
    
    if (!require(car)) stop("Required car package is missing.")
    
    type <- match.arg(type)
    V <- hccm(model, type=type)
    sumry <- summary(model)
    table <- coef(sumry)
    table[,2] <- sqrt(diag(V))
    table[,3] <- table[,1]/table[,2]
    table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
    
    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
    
    print(sumry)
    cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
    
}

summaryR.lm(regOLS,type="hc0") #white standard error - hc0

data

#Ch 15 - IV
load("C:/Users/dal21/OneDrive/BU Fall 2019/EC 508 - Econometrics/Problem Sets/Data Sets- R/fertil2.RData")

#i) OLS regression first
model1<- lm(data$children~ data$educ + data$age + data$agesq)
summary(model1)

#ii) run IV of frsthalf as an IV for educ

install.packages("foreign")
install.packages("stargazer")
library("foreign")
library("stargazer")

#regress x on z + control variables on frsthalf
firststage_model<- lm(data$educ~data$frsthalf + data$age + data$agesq)
summary(firststage_model) #frsthalf is significant, therefore good IV for educ


#iii) now run ivregress after verified it's a good IV for educ
ivmodel<- ivreg(data$children~data$educ + data$age + data$agesq | data$frsthalf + data$age + data$agesq)
summary(ivmodel)


#iv) add binary variables, electric, tv, and bicycle to the model and assume these are exogenous
#estimate the equation by OLS first 
model2<- lm(data$children~ data$educ + data$age + data$agesq + data$electric + data$tv + data$bicycle)
summary(model2)

#Then run the equation using 2SLS
ivmodel2<- ivreg(data$children~data$educ + data$age + data$agesq + 
                     data$electric + data$tv + data$bicycle | data$frsthalf + 
                     data$age + data$agesq + data$electric + data$tv + data$bicycle)
summary(ivmodel2)



