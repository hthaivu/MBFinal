library(tidyverse)
library(ggplot2)
library(GGally)
library(lubridate)
library(tufte)
library(ggthemes)


####### Download all files and read from file

bhcDataAll = data.frame()
pb <- txtProgressBar(min = 2006, max = 2017, style = 3)
for (year in 2006:2017){
  setTxtProgressBar(pb, year)
  for (quarter in 1:4){
    temp <- tempfile()
    # Create url
    url = paste("https://www.chicagofed.org/api/sitecore/BHCHome/GetFile?searchbox=Search&searchbox=&people_filter_name=-+Name+-&people_filter_role=-+Role+-
                &people_filter_team=-+Team+-&SelectedYear=",year,sep="")
    url = paste(url,"&SelectedQuarter=",sep="")
    url = paste(url,quarter,sep="")
    # Download 
    temp = "temp.zip"
    download.file(url,temp,mode="wb",quiet=TRUE)
    tempData <- read.table(unzip(temp),sep="^", nrows=1300, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
    unlink(temp)  
    # Select
    bhcDataTemp = tempData %>%
      select(bankName = RSSD9017,
             totalAssets = BHCK2170,
             totalLoans = BHCK2122,
             loansCI_US = BHCK1763,
             loansCI_foreign = BHCK1764,
             cdsSold = BHCKC968,
             cdsBought = BHCKC969
      ) %>%
      filter(!is.na(totalAssets)) %>%
      mutate(year = year, quarter = quarter, totalCI = loansCI_US + loansCI_foreign, cdsNetBuy = cdsBought - cdsSold)
    # Combine
    bhcDataAll = rbind(bhcDataAll,bhcDataTemp)
  }
}
close(pb)

####### Post-processing

bhcDataAll <- bhcDataAll %>% 
  mutate(mon = quarter * 3) %>%
  mutate(date=ymd(sprintf('%04d%02d%02d', year, mon, 1)))

cdsHolder = bhcDataAll %>% 
  filter(cdsNetBuy != 0) %>%
  group_by(bankName) %>%
  summarise(count = n())

cdsTimeSeries = bhcDataAll %>%
  filter(bankName %in% cdsHolder$bankName)

# Barclays aliases
barclays = bhcDataAll %>%
  filter(grepl('BARCLAYS', bankName))
cdsTimeSeries = rbind(cdsTimeSeries,barclays)

# JP Morgan
jpMorgan = bhcDataAll %>%
  filter(grepl('JPMORGAN', bankName)) %>%
  group_by(bankName, date) %>%
  summarise(Assets = mean(totalAssets),
            Loans = mean(totalLoans),
            NetCDS = mean(cdsNetBuy))

ggplot(jpMorgan) + 
  geom_line(aes(x=date,y=NetCDS))


# Summary
cdsSummary = cdsTimeSeries %>%
  group_by(date) %>%
  filter(cdsNetBuy!=0, quarter %in% c(1,3))%>%
  summarise(count = n())

ggplot(cdsSummary) + 
  geom_line(aes(x=date,y=count), color="dark blue") +
  labs(x = "Date", y = "Number of Banks", title = "Banks With Non-Zero CDS Balance", subtitle = "Period 2006-2017") + 
  scale_y_continuous(breaks=c(20,22,24,26,28,30,32,34)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_bw()


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
