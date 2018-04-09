library(tidyverse)
library(ggplot2)
library(GGally)

test =read.table("data/bhcf1703.txt",sep="^", nrows=1300, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)

bhcData = test %>%
  select(bankName = RSSD9017,
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
