# install.packages(
#     c("tidyverse", "dplyr", "viridis", 
#       "digest")
# )
# 
# install.packages("tidyverse")
# install.packages("hrbrthemes")
# install.packages("reshape")
# install.packages("ggrepel")
# install.packages("readr")
library(dplyr)
library(ggplot2)
library("viridis")
library("digest")
library("hrbrthemes")
library("reshape")
library("ggrepel")
library("readr")


url <- "https://raw.githubusercontent.com/dal211/Wealth-and-Income-Inequality-Paper/master/dfa-networth-levels.csv"
data <- read.csv(url,stringsAsFactors = FALSE)

data<- data %>%
        arrange(desc(Category),Date) 
df<-data 
dates.df<- data.frame(substr(df$Date,1,4)) #separate YYYY from YYYY:Q#
df<- bind_cols(df,dates.df) #add a column only for YYYY
df<- df[c(1,15,2,3,4,5,6,7,8,9,10,11,12,13,14)] #rearrange column order
names(df)[names(df)=="substr.df.Date..1..4."]<-"Year"

head(df)

#Sum all columns by category and year

df1<- df %>%
        group_by(Year,Category) %>%
        summarize_if(is.numeric,sum,na.rm=TRUE) %>%
        mutate(Wealth_Share= (Net.worth /sum(Net.worth))*100) %>%
        mutate(Asset_share= (Assets /sum(Assets))*100) %>%
        mutate(RE_share= (Real.estate /sum(Real.estate))*100) %>%
        mutate(CD_share= (Consumer.durables /sum(Consumer.durables))*100) %>%
        mutate(Equity_share= (Corporate.equities.and.mutual.fund.shares /sum(Corporate.equities.and.mutual.fund.shares))*100) %>%
        mutate(Pen_Share= (Pension.entitlements /sum(Pension.entitlements))*100) %>%
        mutate(PriBus_Share= (Private.businesses /sum(Private.businesses))*100) %>%
        mutate(OtherA_Sh= (Other.assets /sum(Other.assets))*100) %>%
        mutate(Liab_Share= (Liabilities /sum(Liabilities))*100) %>%
        mutate(Mortgages_Sh= (Home.mortgages /sum(Home.mortgages))*100) %>%
        mutate(Consumer_credit_Sh= (Consumer.credit /sum(Consumer.credit))*100) %>%
        mutate(Other_liab_Sh= (Other.liabilities /sum(Other.liabilities))*100) 
        
df1<- df1[c(1,2,3,15,17,18,19,20,21,22,23,24,25,26,4,5,6,7,8,9,10,11,12,13,14)]
names(df1)[names(df1)=="Corporate.equities.and.mutual.fund.shares"]<-"Equities"

df1<- df1 %>% filter(Year!="1989", Year!="2019")

View(df1)
write.csv(df1,file="df1_prop.csv")

#Area Chart - Main Plot
df1$Category <- factor(df1$Category, levels=c("Top1","Next9","Next40","Bottom50")) #sort legend

ggplot(df1,aes(x=as.numeric(as.character(Year)), y=Wealth_Share, fill=Category)) +
    geom_area(alpha=0.6, size=1, color = "black") + ylab("% of Total Net Worth") + xlab("Year") + 
    scale_fill_viridis(discrete=T) + theme_bw() + ggtitle("U.S. Household Wealth Distribution (1990-2018)") + geom_vline(xintercept = 2007, color="grey", size=.00001)


#For Bottom 50% Only
df1_B50<- df1 %>%
          filter(Category=="Bottom50", Year!="1989", Year!="2019") %>%
          arrange(Category) %>%
          group_by(Category) %>%
          mutate(wealth_growth_rate = ((Net.worth - lag(Net.worth))/lag(Net.worth))*100 ) %>%
          mutate(asset_growth_rate = ((Assets - lag(Assets))/lag(Assets))*100 ) %>%
          mutate(liab_growth_rate = ((Liabilities - lag(Liabilities))/lag(Liabilities))*100 ) %>%
          select(Year, Category, Net.worth, Wealth_Share, wealth_growth_rate, Assets, asset_growth_rate, Liabilities, liab_growth_rate)
          
write.csv(df1_B50,file="df1_b50.csv")          

View(df1_B50)

#Bottom 50% area chart
p<- ggplot(df1_B50,aes(x=as.numeric(as.character(Year)), y=Net.worth/1000000, fill=Category)) +
  geom_area(alpha=0.6, size=1, color = "black") + ylab("Net Worth ($M)") + xlab("Year") + 
  scale_fill_viridis(discrete=T) + theme_bw() + ggtitle("U.S. Household Wealth Distribution - Bottom 50% (1990-2018)")

p1<- p + geom_label_repel(
    data=df1_B50 %>% filter(wealth_growth_rate< -8), #filter data first
    aes(label=round(wealth_growth_rate,1)),fill="yellow", size=3) 

p1 + theme(legend.position = "none")  + geom_vline(xintercept = 2007, color="grey", size=.00001)

#some analysis on negative wealth growth rates for bottom 50%

  #between 1990 and 1999? (no negative declines from year to year)
  df1_early<- df1_B50 %>% filter(Year %in% 1990:2000)
  mean(df1_early$wealth_growth_rate,na.rm=TRUE) #7.45%
  median(df1_early$wealth_growth_rate,na.rm=TRUE) #6.355%
  
  #between 2001-2011 (all negative declines from year to year except from 2004-2005)
  df1_mid<- df1_B50 %>% filter(Year %in% 2001:2011)
  mean(df1_mid$wealth_growth_rate,na.rm=TRUE) #-14.2%
  median(df1_mid$wealth_growth_rate,na.rm=TRUE) #-8.119%
  
  #between 2012-2018
  df1_end<- df1_B50 %>% filter(Year %in% 2012:2018)
  mean(df1_end$wealth_growth_rate,na.rm=TRUE) #36.9%
  median(df1_end$wealth_growth_rate,na.rm=TRUE) #2.905

#Recession years ->  July 1990 - March 1991, March 2001-November 2001, December 2007-June 2009 (great recession)

#how do the bottom 50% compare to the top 10%?
  
  #For Top 10% Only
  df1_T10<- df1 %>%
    filter(Category=="Top1" | Category=="Next9" , Year!="1989", Year!="2019") %>%
    arrange(Category) %>%
    group_by(Category) %>%
    select(Year, Category, Net.worth,Assets,Real.estate,Consumer.durables,Equities,Pension.entitlements,Private.businesses,Other.assets,Liabilities,Home.mortgages,Consumer.credit,Other.liabilities)
  
  
  df1_T10<- data.frame(df1_T10)
  df1_T10<- df1_T10 %>%
              #select(Year, Net.worth) %>%
              arrange(Year) %>%
              group_by(Year)%>%
              summarize_if(is.numeric,sum,na.rm=TRUE)
              #summarize(sum(Net.worth))
  
  names(df1_T10)[names(df1_T10)=="sum(Net.worth)"]<-"Net.worth"
  head(df1_T10)
  
  df1_T10 <- df1_T10 %>%
              mutate(wealth_growth_rate = ((Net.worth - lag(Net.worth))/lag(Net.worth))*100 )

  View(df1_T10)

  write.csv(df1_T10,file="df1_T10_1.csv")          
  
  #Top 10% area chart
  p<- ggplot(df1_T10,aes(x=as.numeric(as.character(Year)), y=Net.worth/1000000)) +
    geom_area(alpha=0.6, size=1, color = "black") + ylab("Net Worth ($M)") + xlab("Year") + 
    scale_fill_viridis(discrete=T) + theme_bw() + ggtitle("U.S. Household Wealth Distribution - Top 10% (1990-2018)")
  
  p1<- p + geom_label_repel(
    data=df1_T10 %>% filter(wealth_growth_rate< 0), #filter data first
    aes(label=round(wealth_growth_rate,1)),fill="yellow", size=3) 
  
  p1 + theme(legend.position = "none") + geom_vline(xintercept = 2007, color="grey", size=.00001)
    # geom_text(data=df1_T10, mapping=aes(x=2007, y=250, label="Great Recession Start"), size=3.5, angle=90, vjust=-0.4, hjust=0) 
     
  #some analysis on wealth growth rates for Top 10%
  
  #between 1990 and 1999?
  df1_T10early<- df1_T10 %>% filter(Year %in% 1990:2000)
  mean(df1_T10early$wealth_growth_rate,na.rm=TRUE) #7.67
  median(df1_T10early$wealth_growth_rate,na.rm=TRUE) #8.10
  
  #between 2001-2011 
  df1_T10mid<- df1_T10 %>% filter(Year %in% 2001:2011)
  mean(df1_T10mid$wealth_growth_rate,na.rm=TRUE) #4.91164
  median(df1_T10mid$wealth_growth_rate,na.rm=TRUE) #6.453362
  
  #between 2012-2018
  df1_T10end<- df1_T10 %>% filter(Year %in% 2012:2018)
  mean(df1_T10$wealth_growth_rate,na.rm=TRUE) #6.395163
  median(df1_T10$wealth_growth_rate,na.rm=TRUE) #6.683719





