#REWRITING USING TIDYVERSE
#CS544 Project David Jan-Liu dal2111@bu.edu 
#Data Set - Salaries analysis by college majors (WSJ)

install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library("readr")

url <- "https://raw.githubusercontent.com/dal211/ClassProjectinR/master/degrees-that-pay-back.csv"
tib<- read_csv(url)
tib
names(tib)

#Adding column to log the most popular undergraduate majors at Boston University
BU<- c("Business Management", "Communications", "Economics", "Psychology", 
       "Health Care Administration", "Information Technology (IT)", "Biology")

pop1<- ifelse(tib[1]=="Business Management" | tib[1]=="Communications" 
       | tib[1]=="Economics" | tib[1]=="Economics" |tib[1]=="Psychology" 
       | tib[1]=="Health Care Administration" | tib[1]=="Information Technology (IT)" 
       | tib[1]=="Biology",1, 0)
colnames(pop1)<- "pop"
pop1<- data.frame(pop1)
tib<- bind_cols(tib,pop1)

major_cat<- data.frame(c("Business","STEM","STEM","Social Science","Arts & Humanities","Arts & Humanities","STEM","Business",
              "STEM","STEM","STEM","Social Science","STEM","STEM","STEM","Social Science","Arts & Humanities",
              "Social Science","Social Science","STEM","Arts & Humanities","Arts & Humanities","Business","STEM",
              "STEM","STEM","Arts & Humanities","Health","Social Science","Social Science","STEM","STEM",
              "Arts & Humanities","Social Science","Arts & Humanities","Business","Business","STEM","STEM",
              "Arts & Humanities","Health","Health","Arts & Humanities","Health","STEM","Social Science",
              "Social Science","Arts & Humanities","Social Science","Arts & Humanities"))

colnames(major_cat)<- "Major_Category"
Major_Category<- data.frame(major_cat)

tib<- bind_cols(tib,major_cat) #joining another column to the df that buckets each major listed in a broader category
tib[,c(1,ncol(tib)-1,ncol(tib))]

#Converting character salary figures into numeric  
tib$`Starting Median Salary`<- as.numeric(gsub('[$,]', '', tib$`Starting Median Salary`))
tib$`Mid-Career Median Salary`<- as.numeric(gsub('[$,]', '', tib$`Mid-Career Median Salary`))
tib$`Mid-Career 10th Percentile Salary`<- as.numeric(gsub('[$,]', '', tib$`Mid-Career 10th Percentile Salary`))
tib$`Mid-Career 25th Percentile Salary`<- as.numeric(gsub('[$,]', '', tib$`Mid-Career 25th Percentile Salary`))
tib$`Mid-Career 75th Percentile Salary`<- as.numeric(gsub('[$,]', '', tib$`Mid-Career 75th Percentile Salary`))
tib$`Mid-Career 90th Percentile Salary`<- as.numeric(gsub('[$,]', '', tib$`Mid-Career 90th Percentile Salary`))

tib %>%
    arrange(`Undergraduate Major`)

#Analyzing the Data

#Categorical barplot of starting salaries by broader area of study category
a<- tib %>%
        group_by(Major_Category) %>%
        summarize(count=n(), Starting_Salary=round(mean(`Starting Median Salary`)/1000)) %>%
        arrange(desc(Starting_Salary))

ggplot(a,aes(x=Major_Category,y=Starting_Salary))+ #declaring which variables from "a" should be on the x and y axis
    geom_col(fill="dodgerblue2",color="white")+
    geom_text(aes(label=Starting_Salary),vjust=1.5,color="white")+
    theme(axis.title = element_text(size=12), axis.text=element_text(size=10))+
    ylab ("Starting Career Salary in'000s")+
    xlab("Area of Study")

#Categorical barplot of mid-career salaries by broader area of study category
b<- tib %>%
    group_by(Major_Category) %>%
    summarize(count=n(), Mid_Career_Salary=round(mean(`Mid-Career Median Salary`)/1000)) %>%
    arrange(desc(Mid_Career_Salary))

ggplot(b,aes(x=Major_Category,y=Mid_Career_Salary))+
    geom_col(fill="lemonchiffon1",color="black")+
    geom_text(aes(label=Mid_Career_Salary),vjust=1.5,color="black")+ #add the number labels onto each bar
    theme(axis.title = element_text(size=12), axis.text=element_text(size=10))+ #shrink text labels
    ylab ("Mid Career Salary in'000s")+
    xlab("Area of Study")

#Dotplot of starting and mid-career salaries by popular majors at Boston University
pop_tib<- filter(tib,pop1==1)
colnames(pop_tib)

ggplot(pop_tib,aes(x = `Starting Median Salary`/1000, y = reorder(`Undergraduate Major`, `Starting Median Salary` )))+
    geom_segment(aes(yend=`Undergraduate Major`),xend=0,color="grey50")+  #draw line from axis to each point
    geom_point(size=3,color='dodgerblue2')+
    scale_color_brewer(palette="Set1",limits=c(0,1))+
    geom_text(aes(label=round(`Starting Median Salary`/1000)),vjust=1.5,color="black",position=position_dodge(width=0.9),size=3)+
    theme_bw()+
    theme(
        panel.grid.major.y = element_blank(), #no horizontal grid lines
        legend.position= c(1,0.55), #put legend inside the plot area
        legend.justification=c(1,0.5)
    ) +
    ylab ("Popular Majors at Boston U")+
    xlab("Starting Median Salary in '000s")

ggplot(pop_tib,aes(x = `Mid-Career Median Salary`/1000, y = reorder(`Undergraduate Major`, `Mid-Career Median Salary` )))+
    geom_segment(aes(yend=`Undergraduate Major`),xend=0,color="grey50")+  #draw line from axis to each point
    geom_point(size=3,color='yellow1')+
    scale_color_brewer(palette="Set1",limits=c(0,1))+
    geom_text(aes(label=round(`Mid-Career Median Salary`/1000)),vjust=1.5,color="black",position=position_dodge(width=0.9),size=3)+
    theme_bw()+
    theme(
        panel.grid.major.y = element_blank(), #no horizontal grid lines
        legend.position= c(1,0.55), #put legend inside the plot area
        legend.justification=c(1,0.5)
    ) +
    ylab ("Popular Majors at Boston U")+
    xlab("Mid Career Median Salary in '000s")

#Numerical Analysis

#Boxplot for all listed majors - Starting Median Income

fivenum.var1<- fivenum(tib$`Starting Median Salary`) #per R bloggers, use fivenum instead of summary, due to different ways of calculating quantiles 
fivenum.var1<- fivenum.var1/1000
summary(fivenum.var1)

par(las=2,mai=c(1.5,2.5,.25,.5)) 
boxplot(tib$`Starting Median Salary`/1000, horizontal=FALSE, xaxt="n", 
        main="50 Listed Majors",ylab="Starting Median Salaries in '000s")
axis(side=2, at=fivenum.var1, labels=FALSE)

#Boxplot for all listed majors - Mid-Career Median Income

fivenum.var2<- fivenum(tib$`Mid-Career Median Salary`) #per R bloggers, use fivenum instead of summary, due to different ways of calculating quantiles 
fivenum.var2<- fivenum.var2/1000
summary(fivenum.var2)

par(las=2,mai=c(1.5,2.5,.25,.5)) 
boxplot(tib$`Mid-Career Median Salary`/1000, horizontal=FALSE, xaxt="n", 
        main="50 Listed Majors",ylab="Mid-Career Median Salaries in '000s")
axis(side=2, at=fivenum.var2, labels=FALSE)

#Incoming Freshman STEM majors  
#SOURCE: Higher Education Research Institute, University of California at Los Angeles, Survey of the American Freshman: National Norms, special tabulations (2013).
url1 <- "https://raw.githubusercontent.com/dal211/ClassProjectinR/master/First-time%2C%20full-time%20freshmen%20at%204-year%20institutions%20intending%20an%20S%26E%20major.csv"

library("readr")
tib1<- read_csv(url1)
colnames(tib1)<- c("Year","% Total", "% Female STEM", "% Male STEM")

# ggplot(tib1,aes(x=Year))+
#     geom_line(aes(y=`% Male STEM`),color="pink")+
#     geom_line(aes(y=`% Total`),color="blue")+
#     geom_line(aes(y=`% Female STEM`),color="grey")+
#     ylim(0,50)+
#     ylab(label="% of Incoming Freshman majoring in STEM")+
#     xlab("Year")

library(reshape2)
dd=melt(tib1,id=c("Year"))

ggplot(dd) + geom_line (aes(x=Year,y=value, color=variable )) +
    scale_color_manual(values=c("red","green","blue")) +
    ylim(min(dd$value),max(dd$value)) +
    ylab(label="% of Incoming Freshman majoring in STEM")+
    xlab("Year")

#Distribution of Data (Density Plots)

# ggplot(tib,aes(`Starting Median Salary`/1000))+
#     geom_density()

tib2<- tib[,c(1,2,3)]
tib2<- melt(tib2,id=c("Undergraduate Major"))
tib2<- arrange(tib2,variable,-tib2$value)

ggplot(tib2, aes(x=value,color=variable,fill=variable)) +
    geom_density(alpha=.1) +
    xlim(min(tib2$value),max(tib2$value))

#Sampling of Data

#Drawing random samples of a variable to show the applicability of the Central Limit Theorem
#The Central Limit Theorem states that the distribution of the sample means for a given 
#sample size of the population has the shape of the normal distribution.

#Using the variable mid-career incomes to sample the data in order to demonstrate the CLT

#Average of Mid Career Incomes - $74,786
#Standard Deviation of Mid Career Incomes - 15,927
#Judging from the Density Plot of the 50 Majors in Mid-Career Incomes, the distribution is close to normal,
#so running a large number of trials of the average of x sample size means will approximate a normal distribution

install.packages("prob")
library(prob)

install.packages("sampling")
library(sampling)

k<- tib$`Mid-Career Median Salary`
class(k)
#k is the variable I am using to demonstrate the CLT

par(mfrow = c(2,2))
par(mar=c(5.1,4.1,4.1,2.1))

#Sample Size = 2 ; 1000 trials of picking 2 samples to average out of the mid-career income variable
set.seed(6456)
samples<- 1000 #
sample.size<- 2
xbar1<- numeric(samples)

for (i in 1:samples) {
    
    xbar1[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar1

hist(xbar1/1000, prob=TRUE, breaks=15,
     main="Sample Size = 2", xlim=c(40,110), ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

mean(xbar1)

#Sample Size = 3 ; 1000 trials of picking 3 samples to average out of the mid-career income variable
set.seed(6456)
samples<- 1000
sample.size<- 3
xbar2<- numeric(samples)

for (i in 1:samples) {
    
    xbar2[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar2
hist(xbar2/1000, prob=TRUE, breaks=15,
     main="Sample Size = 3", xlim=c(40,110), 
     ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

#Sample Size = 4 ; 1000 trials of picking 4 samples to average out of the mid-career income variable
set.seed(6456)
samples<- 1000
sample.size<- 4
xbar3<- numeric(samples)

for (i in 1:samples) {
    
    xbar3[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar3
hist(xbar3/1000, prob=TRUE, breaks=15,
     main="Sample Size = 4", xlim=c(40,110), 
     ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

#Sample Size = 5 ; 1000 trials of picking 5 samples to average out of the mid-career income variable
set.seed(6456)
samples<- 1000
sample.size<- 5
xbar4<- numeric(samples)

for (i in 1:samples) {
    
    xbar4[i] <- mean(sample(k, size=sample.size,replace=TRUE))
    
}

xbar4

hist(xbar4/1000, prob=TRUE, breaks=15,
     main="Sample Size = 5", xlim=c(40,110), 
     ylim=c(0,.2), xlab= "Means of Mid-Career Incomes in 1000s", cex.axis=.8)

mean(xbar1)
mean(xbar2)
mean(xbar3)
mean(xbar4)
sd(xbar1)
sd(xbar2)
sd(xbar3)
sd(xbar4)

#Sampling Methods on data - https://collegescorecard.ed.gov/data/ 
#new data to make things more interesting, with a larger dataset that simulates more of a population dataset to perform sampling on

#we can compare dataset mean, variance, and standard deviation versus sampling characteristics

#Tuition cost for 7k+ schools in the U.S.
library("readr")
url2 <- "https://raw.githubusercontent.com/dal211/ClassProjectinR/master/2017_tuitions.csv"
x1<- read_csv(url2)
head(x1)
nrow(x1)
x1<- x1[x1$COSTT4_A !="NULL",]

#Cost of Education Column (Main Dataset)
x1$COSTT4_A<- as.numeric(t(x1$COSTT4_A)) #character to numeric
mean(x1$COSTT4_A) #23,464.12
var(x1$COSTT4_A)  #225,615,879 
sd(x1$COSTT4_A)   #15,020.52

#Simple Random Sampling with Replacement
set.seed(53534)
n<- 70
N<- nrow(x1)
s<- srswr(n,N)
s[s != 0]
rows<- (1:nrow(x1))[s!=0]
length(rows)
rows<- rep(rows,s[s!=0])

sample.1<- x1[rows,]
m1<- sample.1$COSTT4_A
nrow(sample.1) #70 samples taken

par(mfrow = c(1,1))

hist(m1/1000,breaks="Sturges",prob="FALSE",xlim=(c(0,65)), main="Simple Random with Replacement (n=70)", 
     xlab="Tuition Cost in 1000s",ylim=(c(0,30)))

mean(m1) #23,464.13
var(m1)  #172,783,661
sd(m1)   #13,144.72

##Simple Random Sampling without Replacement
set.seed(53534)
s<- srswor(n,N) #simple random sample without replacement (srswor)
s[s != 0]
rows<- (1:nrow(x1))[s!=0]
length(rows)
rows<- rep(rows,s[s!=0])
sample.2<- x1[rows,]
m2<- sample.2$COSTT4_A
nrow(sample.2) #70 samples taken

hist(m2/1000,breaks="Sturges",prob="FALSE",xlim=(c(0,70)), main="Simple Random w/o Replacement (n=70)", 
     xlab="Tuition Cost in 1000s",ylim=(c(0,40)))

mean(sample.2$COSTT4_A) #25,335.9
var(sample.2$COSTT4_A)  #200,377,740
sd(sample.2$COSTT4_A)   #14,155.48

#Systematic Sampling
N<- nrow(x1)
n<- 70
#items in each group
k<- ceiling(N/n)
k
#The next step is to select an item at random from the first group of k items. 
#Based on this selection, the sample of size n is drawn by selecting every kth item.

#random item from the first group of 70 items, then base the kth item from that number
set.seed(536745)
r<- sample(k,1) #first item from the first 70
s<- seq(r,by = k,length= n)
sample.3<- na.omit(x1[s,]) #remove NA
m3<- sample.3$COSTT4_A
nrow(sample.3)

hist(m3/1000,breaks="Sturges",prob="FALSE",xlim=(c(0,70)), main="Systematic Sampling (n=70)", 
     xlab="Tuition Cost in 1000s",ylim=(c(0,40)))

mean(sample.3$COSTT4_A) #26,206.38
var(sample.3$COSTT4_A)  #230,916,544
sd(sample.3$COSTT4_A)   #15,195.94

#Linear Regression - correlation between starting median salary and mid-career median salary by major
#Mid-Career Income as the outcome variable
#d = Starting median income
#e = Mid-career median income

par(mfrow = c(1,1))
install.packages("corrplot")
library(corrplot)

d<- tib$`Starting Median Salary`
e<- tib$`Mid-Career Median Salary`

plot(d,e, main="Linear Regression", 
     xlab="Starting median income (dependent variable)",pch=19,cex.axis=.6,
     ylab="Mid-Career median income")
cor(d,e)

cor.test(d,e) #good stats here for correlation, for two variables

model1<- lm(e ~ d)
summary(model1)
attributes(model1)
abline(model1) #add a line through the points




















