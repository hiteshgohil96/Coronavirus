## INSTALL CORONAVIRUS PACKAGE ##

library(coronavirus)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

## INSTALL UPDATED PACKAGE ##


devtools::install_github("RamiKrispin/coronavirus")
data('coronavirus')


head(coronavirus)
summary(coronavirus)
colnames(coronavirus)


## get total number of cases registered in each country in decreasing order

summary_df <- coronavirus %>% group_by(Country.Region) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)
summary_df %>% head(20)

## Some cases are negative may be an error ##

coronavirus$cases <- abs(coronavirus$cases)

## log transform ##

#coronavirus$cases <- log( coronavirus$cases)

## plot function ###

plotcountry <- function(data,country)
{
  
  ## FILTER THE COUNTRY FROM THE ORIGINAL DATA 
  dx <- filter(data, Country.Region == country)
  
  if (country == 'Mainland China' | country == 'South Korea') {
    dx$cases <- log(dx$cases)
    
  }
  
  ## MAKE A NEW DATAFRAME CONSISTING THE SUM OF CASES CONFIRMED, RECOVERED, DEATH as per DATES
  df <- data.frame(dx %>% select(country = Country.Region, type, date, cases) %>% group_by(type,date)
   %>% summarise(total_cases = sum(cases)))##
  
  ## PLOT THE STACK BARCHART
  ggplot(data= df, aes(x=date, y=total_cases, fill=factor(type, levels = c("recovered","confirmed","death")))) + 
    geom_bar(stat="identity") + 
    scale_fill_manual("type", values = c("confirmed" = "orange", "death" = "darkred", "recovered" = "darkgreen")) +
    ggtitle(paste("Coronavirus in ", country))
}


plotcountry(coronavirus,"Mainland China")  
plotcountry(coronavirus, 'Italy')
plotcountry(coronavirus,'South Korea')
plotcountry(coronavirus, 'Iran')
plotcountry(coronavirus, 'Singapore')
plotcountry(coronavirus,'Japan')





