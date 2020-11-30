
### Dependencies ---------------------------------------------------------------
rm(list = ls()) # clear

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(naniar)) install.packages("naniar")
if (!require(styler)) install.packages("styler")
if (!require(GGally)) install.packages("GGally")
if (!require(skimr)) install.packages("skimr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(dplyr)) install.packages("dplyr")

library(tidyverse)
library(naniar)
library(styler)
library(GGally)
library(skimr)
library(ggplot2)
library(tidyr)
library(dplyr)

### Load the initial data ------------------------------------------------------

# Put data files outside of the git folder in order to avoid pushing too large
# files to repository
# path_to_data <- 'D:/..../payment_dates_final.csv'
# path_to_data <- "D:/01 Skola VSE Statistika/DataX/zaverecny projekt/payment_dates_final.csv"
path_to_data <- "C:/Users/honza/Desktop/projects/R projects/payment_dates_final.csv"

# import dataset to collection
data_collection <- read.csv(path_to_data)

# delete NA's in columns payment_order and payment_date
data_collection <- data_collection %>% drop_na(payment_order, payment_date)


#grouping by contract_id, payment_order and due_date and adding values of first row
data_group = data_collection %>%
  select(contract_id, payment_order, due_date, living_area, different_contact_area, kc_flag, cf_val) %>%
  group_by(contract_id, payment_order, due_date ) %>%
  filter(row_number()==1)

# grouping by contract_id, payment_order and due_date and adding values to column "payment_date"
# to be the value of last row in the group.
# This ensures, that the gorup will have payment date set to the date that happend last
data_group2 = data_collection %>%
  select(contract_id, payment_order, due_date, payment_date) %>%
  group_by(contract_id, payment_order, due_date ) %>%
  summarize(payment_date=last(payment_date))

# grouping via same conditions as in first grouping but adding remaining columns
data_group3 = data_collection %>%
  select(contract_id, payment_order, due_date, product_type, contract_status, business_discount, gender,
         marital_status, number_of_children, number_other_product, clients_phone, client_mobile, client_email, total_earnings,
         birth_year, birth_month, kzmz_flag, due_amount, payed_ammount) %>%
  group_by(contract_id, payment_order, due_date ) %>%
  filter(row_number()==1)


#joining frist two data_groups
data_total = left_join(data_group1, data_group2) 

# joining remaining
data_total = left_join(data_total, data_group3)  

#controlling that there is no N/A's in dataset
apply(data_total, 2, function(x) any(is.na(x)))

#graphically show how many N/A's we have in our new Dataset
vis_miss(data_total, warn_large_data=FALSE)

#droping N/A's in remaining three columns, because it was less then 0.1% of dataset
data_final <- data_total %>% drop_na(living_area, different_contact_area, cf_val)

#test that data_final doesn't contain any N/A's
apply(data_total2, 2, function(x) any(is.na(x)))
