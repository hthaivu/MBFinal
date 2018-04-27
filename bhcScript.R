library(tidyverse)
library(ggplot2)
library(GGally)
library(lubridate)
library(tufte)
library(ggthemes)

# getting only top 10

top20Banks =cdsTimeSeries %>%
  group_by(bankName)%>%
  summarise(meanAssets = mean(totalAssets)) %>%
  arrange(desc(meanAssets)) %>%
  head(20)

top20BankCDS = cdsTimeSeries %>%
  filter(bankName %in% top20Banks$bankName)
  

# JP Morgan
jpMorgan = bhcDataAll %>%
  filter(grepl('JPMORGAN', bankName)) %>%
  group_by(bankName, date) %>%
  summarise(Assets = mean(totalAssets),
            Loans = mean(totalLoans),
            NetCDS = mean(cdsNetBuy))

ggplot(jpMorgan) + 
  geom_line(aes(x=date,y=NetCDS))

ggplot(jpMorgan) + 
  geom_point(aes(x=Loans/Assets, y=NetCDS))


# Summary
cdsSummary = cdsTimeSeries %>%
  group_by(date) %>%
  filter(cdsNetBuy!=0, quarter %in% c(1,3))%>%
  summarise(count = n())

# Visualization
ggplot(cdsSummary) + 
  geom_line(aes(x=date,y=count), color="dark blue") +
  labs(x = "Date", y = "Number of Banks", title = "Banks With Non-Zero CDS Balance", subtitle = "Period 2006-2017") + 
  scale_y_continuous(breaks=c(20,22,24,26,28,30,32,34)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_bw()

ggplot(cdsTimeSeries,aes(x=log(loansCI_US), y=log(cdsNetBuy))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method='lm',formula=y~x)

ggplot(top20BankCDS,aes(x=bankName, y=loansCI_US/totalLoans)) +
  geom_boxplot() +
  coord_flip()


ggplot(cdsTimeSeries) +
  geom_point(aes(x=log(loansCI_to_totalLoans), y=log(netCDS_to_loansCI), size = totalAssets), alpha = 0.25)

ggplot(top20BankCDS) +
  geom_point(aes(x=log(loansCI_to_totalAssets), y=log(cdsNetBuy/totalAssets), size = totalAssets, color = bankName), alpha = 0.5)+
  scale_size_continuous(guide = FALSE)

ggplot(top20BankCDS) +
  geom_point(aes(x=log(loansCI_to_totalLoans), y=log(cdsNetBuy/loansCI_US), color = bankName), alpha = 0.75)



# Statistical Analysis
cdsLeverage = lm(cdsNetBuy ~ totalAssets + loansCI_to_totalLoans , data = cdsTimeSeries)
summary(cdsLeverage)

cdsLoansCI = lm(netCDS_to_loansCI ~ loansCI_to_totalLoans , data = cdsTimeSeries, na.action = na.exclude)
summary(cdsLoansCI)



####### Pre-Analysis 

test =read.table("data/bhcf1703.txt",sep="^", nrows=1300, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)

bhcData = test %>%
  select(bankName = RSSD9017,
         date = RSSD9054,
         totalAssets = BHCK2170,
         totalLoans = BHCK2122,
         loansCI_US = BHCK1763,
         loansCI_foreign = BHCK1764,
         cdsSold = BHCKC968,
         cdsBought = BHCKC969
         ) %>%
  filter(!is.na(totalAssets)) %>%
  mutate(totalCI = loansCI_US + loansCI_foreign, cdsNetBuy = cdsBought - cdsSold) %>%
  arrange(desc(totalAssets))

ggpairs(bhcData,columns = c("totalAssets","totalLoans","totalCI","cdsNetBuy"))

cdsBHC = bhcData %>%
  filter(!cdsNetBuy==0) %>%
  mutate(logCDSBought = log(cdsBought), logCDSSold = log(cdsSold)) %>%
  arrange(desc(cdsNetBuy))

ggpairs(cdsBHC,columns = c("totalAssets","totalLoans","totalCI","logCDSBought","logCDSSold","cdsNetBuy"))


View(bhcData)

View(cdsBHC)
