##############################################
#NAME: data_pull.R
#PURPOSE: Get stock data, format for shiny app
#DATE: November 2017
##############################################

rm(list=ls())
options(scipen=999)
options(tibble.width = Inf)
gc()

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(jsonlite)
library(httr)
library(RSelenium)
library(V8)
library(rvest)
library(knitr)
library(scales)
library(reshape)
library(stringr)
library(Quandl)

##################################
#DATA PULL
##################################

#Set path for output
path<-'/Users/walkerag/Documents/finance/data/'

# Get your API key from quandl.com
quandl_api = "YOURKEYHERE"

# Add the key to the Quandl keychain
Quandl.api_key(quandl_api)

# Company list
# http://www.nasdaq.com/screening/company-list.aspx
company.list<-read_csv(file = paste0(path,"companylist.csv"))

symbols<-unique(company.list$Symbol)

five_years<-Sys.Date()-(365*5)
stock.all<-NULL
for(i in 1:length(symbols)){
  
  print(i)
  Sys.sleep(1)
  
  stock<-tryCatch(Quandl(code=paste0("WIKI/",symbols[i]),start_date=five_years),error = function(e) NULL)  
  
  
  if(!is.null(stock)){
    
    print('Found')
    stock$symbol<-symbols[i]
    stock.all<-rbind(stock.all,stock)
    
  }
  
  rm(stock)
  
  if((i%%10)==0){
    
    saveRDS(stock.all,paste0(path,'stock_history.RDS'))
    
  }
  
}

#####################################
#DATA PREP
#####################################

#Read back in if restarting
#stock.all<-readRDS(paste0(path,'stock_history.RDS'))

#Lookup
company.list<-read_csv(file = paste0(path,"companylist.csv"))
head(company.list)

#Fix Name
company.list[company.list$Symbol=="ORLY","Name"]<-"OReilly Automotive, Inc."

#Match names
names(stock.all)[14]<-'Symbol'

#Check data
company.list %>% group_by(industry) %>% summarise(total=n()) %>% filter(total>20)

#Get market cap
company.list$MarketCapNumDenom<-gsub("[[:digit:]]","",company.list$MarketCap)
company.list$MarketCapNumDenom<-gsub("[$.]","",company.list$MarketCapNumDenom)
company.list<-company.list %>% filter(MarketCapNumDenom %in% c('B','M'))
company.list$MarketCapNum<-as.numeric(gsub("[^0-9.]","",company.list$MarketCap))
company.list$MarketCapFinal<-ifelse(company.list$MarketCapNumDenom=="M",company.list$MarketCapNum*(10^6),company.list$MarketCapNum*(10^9))
summary(company.list$MarketCapFinal)

#Create some rankings
#Remove NA sectors
company.list<-company.list %>% 
  filter(Sector!="n/a") %>%
  mutate(market_cap_rank=rank(desc(MarketCapFinal))) %>%
  group_by(Sector) %>%
  mutate(sector_rank=rank(desc(MarketCapFinal)))

#First date by company
stock.all<-stock.all %>% group_by(Symbol) %>% mutate(min_date=min(Date),max_date=max(Date))
summary(stock.all$max_date)

#Needs to have been around for at least two years
stock.all<-stock.all %>% filter(max_date=="2017-11-17" & min_date<"2015-11-17" & Date>="2015-11-17")

#Check for splits
stock.all<-stock.all %>% mutate(total_splits=sum(`Split Ratio`!=1)) %>% filter(total_splits==0)

#Merge
stock.join<-stock.all %>% inner_join(company.list,by=c("Symbol"))

#####################################
#CREATE PAIRINGS
#####################################

#Two year perc_change
stock.join.edit<-stock.join %>% filter(Date=="2015-11-17") %>% 
  dplyr::select(Symbol,Close) %>% 
  dplyr::rename(Baseline=Close) %>%
  inner_join(stock.join,by=c("Symbol")) %>% mutate(Perc_Change=Close/Baseline)

symbols<-stock.join %>% filter(market_cap_rank<=100 & Sector!="Semiconductors") %>% pull(Symbol)
symbols<-unique(symbols)

big_changers<-stock.join.edit %>% filter(Perc_Change>2.50 | Perc_Change<0.3) %>% pull(Symbol)
big_changers<-unique(big_changers)

symbols<-setdiff(symbols,'GOOGL')
symbols<-setdiff(symbols,'LBRDK')
symbols<-setdiff(symbols,'FOXA')
symbols<-setdiff(symbols,big_changers)
sort(symbols)

company.list.filter<-company.list %>% filter(Symbol %in% symbols)
company.list.filter$NameChars<-nchar(company.list.filter$Name)
head(company.list.filter)

#Needs to be 23 or under
company.list.filter$NamePrepped<-gsub(" Incorporated","",company.list.filter$Name)
company.list.filter$NamePrepped<-gsub(", Inc.","",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub(" Inc.","",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub(", Inc","",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub(" Corporation"," Corp.",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Cognizant Technology Solutions Corp.","Cognizant Technology",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Express Scripts Holding Company","Express Scripts",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("TD Ameritrade Holding Corp.","TD Ameritrade",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Twenty-First Century Fox","21st Century Fox",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Regeneron Pharmaceuticals","Regeneron Pharma.",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Automatic Data Processing","Automatic Data Process.",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Interactive Brokers Group","Interactive Brokers Grp",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("Walgreens Boots Alliance","Walgreens/Boots",company.list.filter$NamePrepped)
company.list.filter$NamePrepped<-gsub("SBA Communications Corp.","SBA Communications",company.list.filter$NamePrepped)
company.list.filter$NamePreppedChars<-nchar(company.list.filter$NamePrepped)
company.list.filter %>% dplyr::select(Symbol,NamePrepped,NamePreppedChars) %>% arrange(desc(NamePreppedChars))

#Replace name
company.list.filter<-company.list.filter %>% dplyr::select(-Name,-NamePreppedChars,-NameChars) %>% dplyr::rename(Name=NamePrepped)

colors<-c("#009E73","#f6ee8d")
i<-1
while(i<1000){
  
  s1<-sample(symbols,size=1)
  s2<-sample(symbols,size=1)
  
  if(s1!=s2){
    
    #Sample colors, this randomyl chooses the winner
    c<-sample(colors,size=2,replace=FALSE)
    
    #Get s1
    s1.stock<-stock.join.edit %>% filter(Symbol==s1) %>% select(Symbol,Date,Perc_Change) %>% mutate(GroupColor=c[1])
    #Get s2
    s2.stock<-stock.join.edit %>% filter(Symbol==s2) %>% select(Symbol,Date,Perc_Change) %>% mutate(GroupColor=c[2])
    
    #Combine
    comb.stock<-rbind(s1.stock,s2.stock)
    
    #Only keep matching days
    comb.stock<-comb.stock %>% group_by(Date) %>% mutate(total_days=n()) %>% filter(total_days==2) %>% ungroup()
    
    #Create lookups
    s1.lookup<-comb.stock %>% filter(Symbol==s1) %>% group_by(Symbol) %>% summarise(total=n(),max_value=max(Perc_Change)) %>% 
      mutate(GroupColor=c[1]) %>% inner_join(company.list.filter,by=c("Symbol"))
    s2.lookup<-comb.stock %>% filter(Symbol==s2) %>% group_by(Symbol) %>% summarise(total=n(),max_value=max(Perc_Change)) %>% 
      mutate(GroupColor=c[2]) %>% inner_join(company.list.filter,by=c("Symbol"))
    
    comb.lookup<-rbind(s1.lookup,s2.lookup)
    
    #Convert to dataframe
    comb.stock<-data.frame(comb.stock)
    
    #Save out
    saveRDS(comb.stock,paste0(path,'comb.stock.',i,'.RDS'))
    saveRDS(comb.lookup,paste0(path,'comb.lookup.',i,'.RDS'))
    
    #Increment counter
    i<-i+1
    print(i)

    rm(comb.stock,comb.lookup,s1.stock,s1.lookup,s2.stock,s2.lookup,c,s1,s2)

  }
  
}