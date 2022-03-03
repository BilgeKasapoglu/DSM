# Load the necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr,
               tibble,
               tidyverse,
               smacof,
               data.table,
               softImpute,
               PMA,
               missMDA,
               stargazer,
               cluster,
               NbClust,
               tidyverse,
               data.table,
               dplyr,
               mFilter)

# Load the data
dta <- read_csv("Desktop/DSM/A1/2020_NL_Region_Mobility_Report.csv")

summary(dta)

# Selecting the part of the data with numerical values
d <- dta %>% select(retail_and_recreation_percent_change_from_baseline, 
                    grocery_and_pharmacy_percent_change_from_baseline, 
                    parks_percent_change_from_baseline, 
                    transit_stations_percent_change_from_baseline, 
                    workplaces_percent_change_from_baseline, 
                    residential_percent_change_from_baseline)

# Hp-filtering
d <- hpfilter(d, freq=200)

d1 <- d <- dta %>% select(transit_stations_percent_change_from_baseline, 
                          workplaces_percent_change_from_baseline)
d1 <- hpfilter(d1, freq=200)


