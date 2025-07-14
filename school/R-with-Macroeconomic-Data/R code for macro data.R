install.packages("tidyverse")
library(tidyverse)
library(dplyr)
install.packages("readr")
library("readr")

url <- "https://raw.githubusercontent.com/dal211/R-with-Macroeconomic-Data/master/cross_country_data.csv"
x<- read.csv(url)
head(x)

x
#GDP per worker in 1985 and 2005

View(x)
x1<- filter(x, Year==1985 | Year==2005)
head(x1)

x1<-x1 %>%
        arrange((Country)) %>%
        mutate(GDPperworker= RealGDP / LaborForce) 
head(x1)

#average savings rate from 1985 to 2005, s
x2<- filter(x, Year %in% (1985:2005))

x2<- x2 %>%
       group_by(Country) %>%
       summarize(avg_savings_rate=mean(SavingsRate,
                                       .rm=TRUE))

head(x2)

#avg growth rate of the labor force from 1985-2005, n.
#compute using the log approx. formula, avg growth rate of a variable x from year t to year t+1
n_df<- filter(x, Year==1985 | Year==2005)
head(n_df)
x_nrate<- n_df %>%
                arrange(Country, Year) %>%
                group_by(Country) %>%
                mutate(perctdifflabor = (LaborForce - lag(LaborForce))/(lag(LaborForce))) %>%
                mutate(loglaborrate = (log(LaborForce) - lag(log(LaborForce)))) %>%
                mutate(avgloglaborrate = (log(LaborForce) - lag(log(LaborForce)))*1/20) %>% #avg growth rate each year 
                select(Country, Year, LaborForce, loglaborrate, avgloglaborrate)

head(x_nrate)
#avg growth rate of GDP per worker from 1985 to 2005, use the log approx G- Y/L
GDP_df<- filter(x, Year==1985 | Year==2005)
GDP_df<-GDP_df %>%
        arrange((Country)) %>%
        mutate(GDPperworker= RealGDP / LaborForce) %>%
        group_by(Country) %>%
        mutate(logapproxGDPperworker = (log(GDPperworker) - lag(log(GDPperworker)))) %>%
        mutate(avglogGDPperworker = (log(GDPperworker) - lag(log(GDPperworker)))*1/20) %>%
        select(Country,Year,GDPperworker,logapproxGDPperworker, avglogGDPperworker)

head(GDP_df)

#number of countries in the sample data
x4<- tibble(x$Country)
head(x4)
count(distinct(x4))

#mean across countries of: 
#log(Y/L - 1985)
x5<- filter(x,Year==1985)

x5<- x5 %>%
        mutate(GDPperworker= RealGDP / LaborForce) %>% 
        mutate(logGDPperworker=log(GDPperworker)) %>%
        select(Country, Year, GDPperworker, logGDPperworker)
head(x5)
mean(x5$logGDPperworker)

#log(Y/L - 2005)
x6<- filter(x, Year==2005)
x6<- x6 %>%
        mutate(GDPperworker= RealGDP / LaborForce) %>% 
        mutate(logGDPperworker=log(GDPperworker)) %>%
        select(Country, Year, GDPperworker, logGDPperworker)
head(x6)
mean(x6$logGDPperworker)

#s 
x_savings<- x %>%
                group_by(Country) %>%
                summarize(avg=mean(SavingsRate,
                                   .rm=TRUE))
head(x_savings)
mean(x_savings$avg)


#n - avg growth rate of the labor force - throughout all time
x7<- filter(x,Year==1980 | Year==2005)
x7<- x7 %>%
        arrange(Country, Year) %>%
        group_by(Country) %>%
        mutate(loglaborgrowth = (log(LaborForce) - lag(log(LaborForce)))) %>%
        mutate(avgloglaborgrowth = (log(LaborForce) - lag(log(LaborForce)))*1/25)

x7<- select(x7,Country, LaborForce, loglaborgrowth, avgloglaborgrowth)
x7<- filter(x7, avgloglaborgrowth!="NA")

head(x7)
mean(x7$avgloglaborgrowth,na.rm=TRUE)

#average growth rate of GDP per worker g(y/L) - throughout all time by country
x8<- filter(x,Year==1980 | Year==2005)

x8<- x8 %>%
        arrange(Country, Year) %>%
        group_by(Country) %>%
        mutate(GDPperworker= RealGDP / LaborForce) %>%
        mutate(logGDPperworker = log(GDPperworker)) %>%
        mutate(avglogGDPperworker = (log(GDPperworker) - lag(log(GDPperworker)))*1/25) %>% #avg GDP per worker growth rate each year
        select(Country, LaborForce, logGDPperworker, avglogGDPperworker)    

x8<- filter(x8, avglogGDPperworker!="NA")
head(x8)

mean(x8$avglogGDPperworker, na.rm=TRUE)


#standard deviation across countries
    #for log(Y/L - 1985)
    head(x5)
    sd(x5$logGDPperworker, na.rm=TRUE)
    
    #for log(Y/L - 2005)
    head(x6)
    sd(x6$logGDPperworker, na.rm=TRUE)
    
    #for s
    head(x_savings)
    sd(x_savings$avg)
    
    #for n
    head(x7)
    sd(x7$avgloglaborgrowth)
    
    #for g of Y/L
    head(x8)
    sd(x8$avglogGDPperworker)
    
#Unconditional Convergence

#avg growth rates of GDP per worker from 1985 to 2005 by unique country

GDPdf_alltime<-  filter(x, Year==1985 | Year==2005)
head(GDPdf_alltime)

GDPdf_alltime<-GDPdf_alltime %>%
                arrange((Country)) %>%
                mutate(GDPperworker= RealGDP / LaborForce) %>%
                group_by(Country) %>%
                mutate(logapproxGDPperworker = (log(GDPperworker) - lag(log(GDPperworker)))) %>%
                mutate(avglogGDPperworker = (log(GDPperworker) - lag(log(GDPperworker)))*1/20) %>%
                select(Country,Year,GDPperworker,logapproxGDPperworker, avglogGDPperworker)            

head(GDPdf_alltime)

GDPdf_alltime<- GDPdf_alltime %>% 
                    arrange(Country) %>%
                    select(Country,avglogGDPperworker) %>% #collapse to get values of growth rates for each country from 1985 to 2005
                    filter(avglogGDPperworker!="NA") 

head(GDPdf_alltime)

count(distinct(GDPdf_alltime))
GDPdf_alltime<- data.frame(GDPdf_alltime)

head(GDPdf_alltime)

#log(Y/L - 1985)
x1.2<- filter(x, Year==1985)

x1.2<- x1.2 %>%
    arrange(Country) %>%
    mutate(GDPperworker1985= RealGDP / LaborForce) %>%
    mutate(logGDPperworker1985 = log(GDPperworker1985)) %>%
    select(logGDPperworker1985)

head(x1.2)

x9<- cbind(GDPdf_alltime,x1.2)
colnames(x9) <- c("Country", "G-(Y/L)", "LogGDPperWorker1985")
head(x9)

y<- x9$`G-(Y/L)` 
x.1<- x9$LogGDPperWorker1985

#regression
model1<- lm(y ~ x.1)
model1
summary(model1)
attributes(model1)

plot(x.1,y, main="Linear Regression", 
     xlab="log(Y/L) in 1985 (indep. variable)",pch=19,cex.axis=.6,
     ylab="Avg growth rate Y/L from 1985 to 2005 (dep. variable) ")
abline(model1) #add a line through the points

#c - MRW Revisted

#Log(Y/L - 2005) - dependent variable
x2.1<- filter(x, Year==2005)

head(x2.1)
x2.1<- x2.1 %>%
            arrange(Country) %>%
            mutate(GDPperworker2005= RealGDP / LaborForce) %>%
            mutate(logGDPperworker2005 = log(GDPperworker2005)) %>%
            select(Country, logGDPperworker2005)
head(x2.1)

#log(s) - Indep. variable x1
x2.2<- x2
head(x2.2)
x2.2<- x2.2 %>%
            mutate(logsavingsrate = log(avg_savings_rate))

head(x2.2)
count(distinct(x2.2))

#log(n+.05) - Indep. variable x2
head(x_nrate)
x_nrate1<- filter(x_nrate, avgloglaborrate!="NA")
head(x_nrate)
head(x_nrate1)
x_nrate1<-  x_nrate1 %>%
                arrange((Country)) %>%
                mutate(log_nrate1_plusconstant = log(avgloglaborrate + .05)) %>%
                select(Country, log_nrate1_plusconstant)
head(x_nrate1)

#new df
MRW_df<- cbind(x2.1,x2.2$logsavingsrate,x_nrate1$log_nrate1_plusconstant)
colnames(MRW_df)<-c("Country","logGDPperworker2005","logS","log(n+g+d)")
head(MRW_df)


#MLR 

y4.1<- MRW_df$logGDPperworker2005
x4.1<- MRW_df$logS
x4.2<- MRW_df$`log(n+g+d)`

model2<- lm(y4.1~ x4.1 + x4.2)
model2
summary(model2)
#attributes(model4)
anova(model2)



#HJ Levels Accounting - solve for Ai in year 2005
alpha<- (1/3)
HF_df<- filter(x, Year==2005)
head(HF_df)
alpha_exp <- alpha / (1-alpha)

head(HF_df)

HF_df<- HF_df %>%
            arrange(Country) %>%
            mutate(GDPperworker05= RealGDP / LaborForce) %>%
            mutate(capitalintensity05 = PhysicalCapital / RealGDP) %>%
            mutate(humancap_per_person05 = HumanCapital) %>%
            mutate(Implied_Prod = GDPperworker05 / (capitalintensity05^(alpha_exp)*humancap_per_person05)) %>%
            mutate(logA = log(Implied_Prod)) %>%
            mutate(logGDPperworker05 = log(GDPperworker05)) %>%
            select(Country, Year, GDPperworker05, capitalintensity05, humancap_per_person05, Implied_Prod, logA, logGDPperworker05)
            
head(HF_df)
#mean and standard deviation across countries of log(A) in 2005
mean(HF_df$logA)
sd(HF_df$logA)

#run plot and regression line
HF_reg<- HF_df %>%
            select(Country, logGDPperworker05, logA)

head(HF_reg)
y5.1<- HF_reg$logGDPperworker05
x5.1<- HF_reg$logA

model5<- lm(y5.1 ~ x5.1)
model5
summary(model5)

plot(x5.1,y5.1, main="Linear Regression", 
     xlab="log(A) - 2005 (indep. variable)",pch=19,cex.axis=.6,
     ylab="log(Y/L) - 2005 (dep. variable) ")
abline(model5) #add a line through the points
