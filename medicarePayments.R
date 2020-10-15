# Medicare payments data analysis. 
library(readr)
library(readxl)
library(haven)
library(tidyverse)
library(zipcode)
setwd("/Users/hanifahotelwala/Documents/CodeBase/Rstudio/msda/dataviz/MedicarePayments/")
getwd()
md = read_tsv("data/Medicare_Provider_Util_Payment_PUF_CY2017.txt")
View(md)bn
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
View(df_TexasOnly)

#df consisting of the columns of interest only
df_selectedColumns <- df_TexasOnly %>% select(nppes_provider_gender, nppes_provider_city,nppes_provider_state,nppes_provider_zip, provider_type,place_of_service, medicare_participation_indicator,hcpcs_code, hcpcs_description, line_srvc_cnt, bene_unique_cnt, bene_day_srvc_cnt, average_submitted_chrg_amt, average_Medicare_payment_amt)
View(df_selectedColumns)

##### October 13 analysis. 
# grouping by hcpcs codes with letters in front of them 
df_hcpcs_alphaNumerica <- df_selectedColumns %>%  filter(str_detect(hcpcs_code, "[:alpha:]"))
View(df_hcpcs_alphaNumerica)

##Occurances of hcpcs codes + cpt codes mixed in 
HCPCSandCPT_occurances<- table(unlist(df_selectedColumns$hcpcs_code))
View(HCPCSandCPT_occurances) ##click on frequency to assess what is found the most. 

##Occurances of hcpcs codes only 
HCPCS_occurances<- table(unlist(df_hcpcs_alphaNumerica$hcpcs_code))
View(HCPCS_occurances) ##click on frequency to assess what is found the most. 

##data frame with most coded hcpcs code: G0008 - flu
df_g0008 <- df_hcpcs_alphaNumerica %>% filter(hcpcs_code == "G0008") 
df_columnsOfInterest <- df_g0008 %>% select(nppes_provider_gender, average_submitted_chrg_amt, nppes_provider_city)
df_genderAndChargeByCity_forFlu <- df_columnsOfInterest %>% 
                            group_by(nppes_provider_gender)  %>% 
                            summarize(mean_average_submitted_chart_amt= mean(average_submitted_chrg_amt, na.rm=TRUE), 
                            .groups="drop")
  
View(df_genderAndChargeByCity_forFlu)

ggplot(df_genderAndChargeByCity_forFlu, aes(nppes_provider_gender)) +
  geom_bar(stat="count", aes(fill=nppes_provider_gender, color="yellow")) +
  theme(legend.position="none") ##not working the way i want it to. 





