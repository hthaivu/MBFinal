library(tidyverse)
library(ggplot2)
library(GGally)
library(lubridate)
library(tufte)
library(ggthemes)


####### Download all files and read from file and save (I did not save the master file, but save the post-processed file)

bhcDataAll = data.frame() # empty data
pb <- txtProgressBar(min = 2006, max = 2017, style = 3) # visualizing the progress just for fun :)
for (year in 2006:2017){
  setTxtProgressBar(pb, year)
  for (quarter in 1:4){
    temp <- tempfile()
    # Create url
    url = paste("https://www.chicagofed.org/api/sitecore/BHCHome/GetFile?searchbox=Search&searchbox=&people_filter_name=-+Name+-&people_filter_role=-+Role+-
                &people_filter_team=-+Team+-&SelectedYear=",year,sep="")
    url = paste(url,"&SelectedQuarter=",sep="")
    url = paste(url,quarter,sep="")
    # Download and read every quarter file
    temp = "temp.zip"
    download.file(url,temp,mode="wb",quiet=TRUE)
    tempData <- read.table(unzip(temp),sep="^", nrows=1300, comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
    unlink(temp)  
    # Select wanted variables and save to temp
    bhcDataTemp = tempData %>%
      select(bankName = RSSD9017,
             totalAssets = BHCK2170,
             totalLoans = BHCK2122,
             loansCI_US = BHCK1763,
             loansCI_foreign = BHCK1764,
             cdsSold = BHCKC968,
             cdsBought = BHCKC969
             # Look into the sample-dictionary.xlsx to see if you would like to include any other variables
      ) %>%
      filter(!is.na(totalAssets)) %>%
      mutate(year = year, quarter = quarter, totalCI = loansCI_US + loansCI_foreign, cdsNetBuy = cdsBought - cdsSold)
    # Combine with the master dataset
    bhcDataAll = rbind(bhcDataAll,bhcDataTemp)
  }
}
close(pb)

####### Post-processing to select only banks with CDS 

bhcDataAll <- bhcDataAll %>% 
  mutate(mon = quarter * 3) %>%
  mutate(date=ymd(sprintf('%04d%02d%02d', year, mon, 1)))

cdsHolder = bhcDataAll %>% 
  filter(cdsNetBuy != 0) %>%
  group_by(bankName) %>%
  summarise(count = n())

cdsTimeSeries = bhcDataAll %>%
  filter(bankName %in% cdsHolder$bankName)

# Barclays aliases (Barclays is one of the banks that got renamed. Some years it does not have CDS, but we should include them for the completeness of the dataset)

barclays = bhcDataAll %>%
  filter(grepl('BARCLAYS', bankName))  # Find any banks that has "Barclays" in their name %>%

cdsTimeSeries = rbind(cdsTimeSeries,barclays) %>% # combine the master data set with any extra "Barclays"
  group_by_all() %>% summarise() %>% ungroup() # this combination gets rid of any duplicate rows


# Adding new metrics
cdsTimeSeries = cdsTimeSeries %>%
  ungroup %>%
  mutate(leverage = totalLoans/totalAssets, 
         loansCI_to_totalAssets = loansCI_US/totalAssets,
         loansCI_to_totalLoans = loansCI_US/totalLoans,
         netCDS_to_loansCI = cdsNetBuy/loansCI_US) %>%
  filter(loansCI_US != 0)

# Save to csv
write_csv(cdsTimeSeries,"data/cdsTimeSeries.csv")

