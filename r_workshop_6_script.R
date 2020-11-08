#install.packages('tidyverse')
library(tidyverse)

### Importing Data
data.df <- read_csv('https://data.ca.gov/dataset/1b31c08e-b1a7-4459-8aef-41cfff61fc5e/resource/5c42e9f6-e172-4db4-9a51-ca1256b03a26/download/fleet-asset-management-system-open-data-2015-2019.csv')
summary(data.df)
#data dictionary https://data.ca.gov/dataset/california-state-fleet/resource/bf852ee6-49c5-42e5-9b2c-f531b9143b85 

### Dealing with missing data
## we need to replace "No Data", "Null" to NA
data.df[data.df == 'No Data'] <- NA
data.df[data.df == 'Null'] <- NA

### Dealing with numbers stored as characters
columnsToConvert <- c(
    "Model_Year",
    "Payload_Rating",
    "Shipping_Weight",
    "Purchase_Price",
    "Disposition_Mileage",
    "Disposition_Sold_Amount",
    "Total_Miles"
  )

for (colName in columnsToConvert) {
  data.df[colName] <- parse_number(data.df[[colName]])
}

summary(data.df)

negative_miles <- data.df %>% #why do these vehicles have negative miles?
  filter(Total_Miles < 0)

#lets get rid of the vehciles with negative miles
data.df <- anti_join(data.df, negative_miles, by='Total_Miles') #anti-join is more SQL based


#using built in plotting functions
modelYears <- data.df %>% filter(!is.na(Model_Year), Model_Year > 1000)

hist(modelYears$Model_Year,
     breaks = 100,
     xlim = c(min(modelYears$Model_Year), max(modelYears$Model_Year))
     )

barplot(data.df$Make_Model)






