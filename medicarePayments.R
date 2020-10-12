# Medicare payments data analysis. 
library(readr)
library(readxl)
library(haven)
library(tidyverse)
library(zipcode)
setwd("/Users/hanifahotelwala/Documents/CodeBase/Rstudio/msda/dataviz/MedicarePayments/")
getwd()
md = read_tsv("data/Medicare_Provider_Util_Payment_PUF_CY2017.txt")
View(md)
dim(md)
class(md)
summary(md)
head(md)
md_data <- as_tibble(md) #conversion into a tibble data frame for easier data analysis
md_data
md_distinct <- md_data %>% distinct() #no repeated data -  same observations 


#dataframes based on provider types + Medicare average columns
typesOfProviders <- md_data$provider_type %>% distinct()
typesOfProviders 
summary(typesOfProviders)

df_providerType_by_averagesOfMedicare <- md_data %>% 
  group_by(provider_type) %>% 
  summarize(mean_average_Medicare_allowed_amt = mean(average_Medicare_allowed_amt, na.rm=TRUE),
            mean_average_submitted_chart_amt= mean(average_submitted_chrg_amt, na.rm=TRUE), 
            mean_average_Medicare_payment_amt = mean(average_Medicare_payment_amt, na.rm=TRUE),
            mean_average_Medicare_standard_amt = mean(average_Medicare_standard_amt, na.rm=TRUE), 
            .groups="drop")
View(df_providerType_by_averagesOfMedicare)
summary(df_providerType_by_averagesOfMedicare)
sapply(df_providerType_by_averagesOfMedicare, class) # view variables of dataframe

ggplot(data = df_providerType_by_averagesOfMedicare,  ## too many providers listed in the x axis. 
      mapping = aes(x = provider_type, y = mean_average_Medicare_allowed_amt)) + 
      geom_point() +  
      geom_line()


#### October 10 Analysis

## Dataframe with only Texas. 
df_TexasOnly <- filter(md_data, nppes_provider_state=="TX") ##filtering by state.
View(df_byStateOnly)

#df consisting of the columns of interest only
df_selectedColumns <- df_TexasOnly %>% select(nppes_provider_gender, nppes_provider_city,nppes_provider_state,nppes_provider_zip, provider_type,place_of_service, medicare_participation_indicator,hcpcs_code, hcpcs_description, line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt, average_submitted_chrg_amt, average_Medicare_payment_amt)
View(df_selectedColumns)
