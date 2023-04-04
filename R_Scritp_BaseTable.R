################
## BASE TABLE ##
################


###### INSTRUCTION #####
#In the last statement of this file, the output file path of the Base_Table_R.RData (RDATA file of Base Table) 
#must be specified after which the Rdata file should be loaded in the Shiny and the Markdown 
#files to run them. 


#loading all libraries

library(DT)
library(RColorBrewer)
library(mapproj)
library(sf)
library(RgoogleMaps)
library(scales)
library(rworldmap)
library(maps)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(maptools)
library(leaflet)
library(sf)
library(tmap)
library(here)
library(rgdal)
library(scales)
library(flextable)
library(plyr)
library(shiny)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)

################
# Load Data    #
################

load ("C:/Users/svellingirikowsalya/Desktop/R Programming/Group assignment folder-20211213/DataGroupAssignment.Rdata")
countries <- read_excel("C:/Users/svellingirikowsalya/Desktop/R Programming/Group assignment folder-20211213/Appendices Group Assignment.xlsx", sheet = "Appendix 2", col_names = c('Country','CountryName'), skip = 1)
languages <- read_excel("C:/Users/svellingirikowsalya/Desktop/R Programming/Group assignment folder-20211213/Appendices Group Assignment.xlsx", sheet = "Appendix 3", col_names = c('Language','LanguageName'), skip = 1)
Product<- read_excel("C:/Users/svellingirikowsalya/Desktop/R Programming/Group assignment folder-20211213/Appendices Group Assignment.xlsx", sheet = "Appendix 1", col_names = c('ProductID','App_name'), skip = 1)
Application<- read_excel("C:/Users/svellingirikowsalya/Desktop/R Programming/Group assignment folder-20211213/Appendices Group Assignment.xlsx", sheet = "Appendix 4", col_names = c('ApplicationID','App_name'), skip = 1)

################
# Demographics #
################

demog <- Demographics

# converting characters to dates

demog$RegDate   <- as.Date(demog$RegDate, format = "%Y-%m-%d")
demog$FirstPay  <- as.Date(demog$FirstPay,format = "%Y%m%d") 
demog$FirstAct  <- as.Date(demog$FirstAct, format = "%Y%m%d") 
demog$FirstSp   <- as.Date(demog$FirstSp, format = "%Y%m%d") 
demog$FirstCa   <- as.Date(demog$FirstCa, format = "%Y%m%d") 
demog$FirstGa   <- as.Date(demog$FirstGa, format = "%Y%m%d") 
demog$FirstPo   <- as.Date(demog$FirstPo, format = "%Y%m%d")

# Dropping rows with missing values in the FirstAct column

demog <- demog %>% 
  filter(!is.na(FirstAct))

# Verifying if the missing value rows were dropped 

sum(is.na(demog$FirstAct))

# Create dummy variables, if the person has played Casino, Games or Poker (Yes/No)
demog <- demog %>% 
  mutate(CasinoPlayer = ifelse(is.na(demog$FirstCa), 0, 1)) %>%
  mutate(GamesPlayer = ifelse(is.na(demog$FirstGa), 0, 1)) %>%
  mutate(PokerPlayer = ifelse(is.na(demog$FirstPo), 0, 1))

# Calculating the Length of Relationship 

demog$LOR <- difftime(as.Date('2005-09-30'),demog$FirstPay , units = c("days"))

# Create a new table selecting only the necessary variables

demog_new <- demog %>%
  select(UserID, FirstPay, LOR, CasinoPlayer, GamesPlayer, PokerPlayer, Country, Language, Gender, ApplicationID)

########################
# UserDailyAggregation #
########################

#Merging Demographic information to find the first pay

UserDailyAggregation1 = merge(x = UserDailyAggregation, y = Demographics[ , c("UserID", "FirstPay")], by = "UserID", all.x=TRUE)

#Filtering/verifying whether the betting information dates is greater than first pay 

UserDailyAggregation1 = UserDailyAggregation1[UserDailyAggregation1$Date > UserDailyAggregation1$FirstPay, ]

#Aggregating the table to find the mean and sum of stakes, winnings and bets

Average_Values1 = UserDailyAggregation1 %>%
  group_by(UserID) %>%
  dplyr::summarise_at(c("Stakes", "Winnings","Bets"), list(mean,sum), na.rm = TRUE)

#Renaming Variables

Average_Values1 = Average_Values1 %>%
  dplyr::rename(
    Average_Stakes_day =Stakes_fn1,
    Average_winnings_day=Winnings_fn1,
    Average_bets_day=Bets_fn1,
    Total_Stakes=Stakes_fn2,
    Total_winnings=Winnings_fn2,
    Total_bets=Bets_fn2
  )

#Calculating Profits per customer

Average_Values1$Total_profits_Customer = Average_Values1$Total_winnings - Average_Values1$Total_Stakes

#Summarising the sum of bets and stakes grouped by product

per_product = UserDailyAggregation1 %>%
  group_by(UserID,ProductID) %>%
  dplyr::summarise_at(c("Bets","Stakes"), list(sum), na.rm = TRUE) %>%
  pivot_wider(names_from = ProductID,values_from = c(Bets, Stakes))

#Filling NA with 0

per_product[is.na(per_product)] = 0

#Merging the tables created above

UserDailyAggregation_new = left_join(Average_Values1,per_product,by="UserID")

#Finding whether the date is weekday or weekend

UserDailyAggregation1$Date  <- as.Date(UserDailyAggregation1$Date,format = "%Y%m%d")
UserDailyAggregation1$wDay <- ifelse(weekdays(UserDailyAggregation1$Date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

#Grouping and summarising based on weekday or weekend

week_bets = UserDailyAggregation1 %>%
  group_by(UserID,wDay) %>%
  dplyr::summarise_at(c("Bets"), list(sum), na.rm = TRUE) %>%
  pivot_wider(names_from = wDay,values_from = c(Bets)) %>%
  dplyr::rename(
    Bets_weekdays =weekday,
    Bets_weekends =weekend)

#Merging and renaming the newly created variables

UserDailyAggregation_new = left_join(UserDailyAggregation_new,week_bets,by="UserID")

UserDailyAggregation_new = UserDailyAggregation_new %>%
  dplyr::rename(
    Nbr_Bets_Sbook_fixedodd =Bets_1,
    Nbr_Bets_Sbook_liveaction=Bets_2,
    Nbr_Bets_CasinoBossMedia=Bets_4,
    Nbr_Bets_Supertoto=Bets_5,
    Nbr_Bets_Games_VS=Bets_6,
    Nbr_Bets_Games_bwin=Bets_7,
    Nbr_Bets_CasinoChartwel=Bets_8,
    Stakes_Sbook_fixedodd =Stakes_1,
    Stakes_Sbook_liveaction=Stakes_2,
    Stakes_CasinoBossMedia=Stakes_4,
    Stakes_Supertoto=Stakes_5,
    Stakes_Games_VS=Stakes_6,
    Stakes_Games_bwin=Stakes_7,
    Stakes_CasinoChartwel=Stakes_8
  )

#calculating the recency for the customer

UserDailyAggregation1$Date <- as.Date(UserDailyAggregation1$Date, format = "%Y%m%d")
UserDailyAggregation1$recency = difftime(as.Date('2005-09-30'),UserDailyAggregation1$Date , units = c("days"))

#calculating the frequency for the customer

frequency = UserDailyAggregation1 %>%
  dplyr::count(UserID)

#Merging the recency and frequency with the table

recency_frequency = merge(UserDailyAggregation1,frequency)
recency_frequency = recency_frequency %>%
  group_by(UserID) %>%
  dplyr::summarise_at(c("recency","n"), list(min), na.rm = TRUE)

#Final user aggregation table 

UserDailyAggregation_new = merge(UserDailyAggregation_new,recency_frequency, by="UserID")

UserDailyAggregation_new = UserDailyAggregation_new %>%
  dplyr::rename(frequency =n)

########################
# PokerChipConversions #
########################


chips <- PokerChipConversions
chips$date <- date(chips$TransDateTime)
chips$TransDateTime <- ymd_hms(chips$TransDateTime)
chips <- subset(chips,date <"2005-09-30")

#Summarising values based on the transaction type (Buy or sell)

chips2 <- chips %>%
  group_by(UserID,date) %>%
  dplyr::summarise(Total_N_Sell = length(TransType[TransType==24]),
                   Total_N_Buy = length(TransType[TransType==124]),
                   Total_Amount_Sell = sum(TransAmount[TransType==24]),
                   Total_Amount_Buy = sum(TransAmount[TransType==124]),
                   Revenue_per_day = Total_Amount_Buy - Total_Amount_Sell
  )%>%
  group_by(UserID)%>%
  dplyr::summarise(AVG_Total_N_Sell = round(mean(Total_N_Sell),2),
                   AVG_Total_N_Buy = round(mean(Total_N_Buy),2),
                   Total_Amount_Sell = sum(Total_Amount_Sell),
                   Total_Amount_Buy = sum(Total_Amount_Buy),
                   Revenue_per_Day = round(mean(Revenue_per_day),2)
  )
head(chips2,30)

#calculating the lag time between two sessions on the same date for chips bought

chipsbuy <- chips %>%
  filter(TransType == 124) %>%
  group_by(UserID, date) %>%
  dplyr::summarise(Lag_Buy = TransDateTime - lag(TransDateTime))%>%
  dplyr::summarise(Avg_Lag_Buy = mean(Lag_Buy,na.rm=TRUE))%>%
  group_by(UserID)%>%
  dplyr::summarise(Avg_Lag_Buy = mean(Avg_Lag_Buy,na.rm=TRUE))
chipsbuy$Avg_Lag_Buy <- seconds_to_period(chipsbuy$Avg_Lag_Buy)

chipsbuy


#calculating the lag time between two sessions on the same date for chips sold

chipsSell <- chips %>%
  filter(TransType == 24) %>%
  group_by(UserID, date) %>%
  dplyr::summarise(Lag_Sell = TransDateTime - lag(TransDateTime))%>%
  dplyr::summarise(Avg_Lag_Sell = mean(Lag_Sell,na.rm=TRUE))%>%
  group_by(UserID)%>%
  dplyr::summarise(Avg_Lag_Sell = mean(Avg_Lag_Sell,na.rm=TRUE))
chipsSell$Avg_Lag_Sell <- seconds_to_period(chipsSell$Avg_Lag_Sell)
chipsSell

#Merging the tables created above

chips3 <- merge(chips2,chipsbuy,by = "UserID")
chips_dirty <- merge(chips3,chipsSell,by="UserID")

#Calculating the profit for the company 

chips_dirty$Total_Profit = chips_dirty$Total_Amount_Buy - chips_dirty$Total_Amount_Sell
head(chips_dirty,30)

#################
# The basetable #
#################

demog_Aggr <- merge(demog_new,UserDailyAggregation_new,by = "UserID", all.x=TRUE)
df <- merge(demog_Aggr, chips_dirty, by = "UserID", all.x=TRUE)

#Working on the outliers

#Outliers are bought back to 95% percentile if they are greater and 0th percentile if they are on the lower limit
df <- df%>%
  mutate_at(vars(AVG_Total_N_Sell,AVG_Total_N_Buy,Total_Amount_Sell,Total_Amount_Buy,Total_Profit,Revenue_per_Day),~replace_na(.,0))
df$Average_Stakes_day <- squish(df$Average_Stakes_day, quantile(df$Average_Stakes_day, c(.0, .95),na.rm= TRUE))
df$Average_winnings_day <- squish(df$Average_winnings_day, quantile(df$Average_winnings_day, c(.0, .95),na.rm= TRUE))
df$Average_bets_day  <- squish(df$Average_bets_day , quantile(df$Average_bets_day , c(.0, .95),na.rm= TRUE))
df$Total_Stakes  <- squish(df$Total_Stakes , quantile(df$Total_Stakes , c(.0, .95),na.rm= TRUE))
df$Total_winnings  <- squish(df$Total_winnings , quantile(df$Total_winnings  , c(.0, .95),na.rm= TRUE))
df$Total_bets  <- squish(df$Total_bets  , quantile(df$Total_bets  , c(.0, .95),na.rm= TRUE))
df$Total_profits_Customer  <- squish(df$Total_profits_Customer  , quantile(df$Total_profits_Customer  , c(.05, .95),na.rm= TRUE))
df$AllPlayer = 1

#Renaming the variables created

df<-  df%>%
  dplyr::rename(Avg_Stakes = Average_Stakes_day, Avg_winnings = Average_winnings_day,Avg_Bets = Average_bets_day, Total_Bets = Total_bets  )

#Merging the appendix with the basetable 

df <- merge(df,countries,by = "Country")
df <- merge(df,languages,by = "Language")
df <- merge(df,Application,by = "ApplicationID")

#removing unneccesary columns

df = subset(df, select = -c(Bets_NA,Stakes_NA))


#Creating Table for different Plots

#revenue based on country

Best_Country_Revenue <- df%>%
  group_by(`CountryName`)%>%
  dplyr::summarize(Country_Profit_Comp = sum(Total_Amount_Buy), Country_Loss_Comp = sum(Total_Amount_Sell))
Best_Country_Revenue1 <- Best_Country_Revenue[order(-Best_Country_Revenue$Country_Profit_Comp),]
Best_Country_Revenue1 <- Best_Country_Revenue1[1:10,]
Best_Country_Revenue1$Country_Profit_Comp <- round(Best_Country_Revenue1$Country_Profit_Comp,2)

#Finding the days of the week 

weekdays_ = UserDailyAggregation1
weekdays_$day <- weekdays(UserDailyAggregation1$Date)

#Summarising the values based on the days of the week

weekdays1_ = weekdays_ %>%
  group_by(day) %>%
  dplyr::summarise(Total_Bets = sum(Bets),Total_Stakes = sum(Stakes),Total_Winnings=sum(Winnings), Avg_Bets = mean(Bets), Avg_Stakes = mean(Stakes),Avg_Winnings = mean(Winnings))
weekdays1_ = na.omit(weekdays1_)

#Summarising based on the product

Pivot_product <- df%>%
  dplyr::summarise(Sbook_fixedodd = sum(Nbr_Bets_Sbook_fixedodd, na.rm = TRUE), Sbook_liveaction = sum(Nbr_Bets_Sbook_liveaction, na.rm = TRUE),
                   CasinoChartwel = sum(Nbr_Bets_CasinoChartwel, na.rm = TRUE),Games_VS = sum(Nbr_Bets_Games_VS, na.rm = TRUE),
                   Games_bwin = sum(Nbr_Bets_Games_bwin, na.rm = TRUE), CasinoBossMedia = sum(Nbr_Bets_CasinoBossMedia, na.rm = TRUE),
                   Supertoto = sum(Nbr_Bets_Supertoto, na.rm = TRUE)
  )
Pivot_product<- as.data.frame(t(Pivot_product))
Pivot_product<- rownames_to_column(Pivot_product)

#In the last statement in the file R_Scritp_BaseTable.R, the output file path of the Base_Table_R.RData (RDATA file of Base Table) 
#must be specified after which the Rdata file should be loaded in the Shiny and the Markdown 
#files to run them. 
#Exporting the final Table to load it for Shiny and Markdown

save(df, Best_Country_Revenue1, Pivot_product, weekdays1_, file="C:/Users/svellingirikowsalya/Desktop/R Programming/R_Project_Final/R_Project_Final/Base_Table_R.RData")
