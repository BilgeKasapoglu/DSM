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

<<<<<<< HEAD
## Q1 (d)

m <- cbind(c(4,1,0,2,6,4),c(1,3,4,1,2,0))

cluster <- kmeans(m,2,nstart=5)
cluster$cluster

=======
>>>>>>> 7f7b6e51bdf4b8a22eee14f5a1106828e6965821

# Create a directory to store downloaded data
dir.create("A1/data")

# Download data
download_data <- function(url, filename){
  download.file(url = url, destfile = paste0(filename, ".csv"))
}

url <- "https://drive.google.com/uc?id=1DaYBBo_qohz-QiMOIPu08m0QXbjRsu0Q&export=download"

download_data(url, "A1/data/2020_NL_Region_Mobility_Report")

# Load the data
dta <- read.csv("A1/data/2020_NL_Region_Mobility_Report.csv")

d = dta[!(is.na(dta$sub_region_1) | dta$sub_region_1 =="") & (is.na(dta$sub_region_2) | dta$sub_region_2==""),]

d <- d %>% select(date,sub_region_1,sub_region_2,
                    transit_stations_percent_change_from_baseline, 
                    workplaces_percent_change_from_baseline, 
    )


transit = d %>% pivot_wider(
  id_cols = "date",
  names_from = "sub_region_1",names_prefix = "transit",
  names_sep = "_",
  values_from = c(transit_stations_percent_change_from_baseline),
)

work = d %>% pivot_wider(
  id_cols = "date",
  names_from = "sub_region_1",names_prefix = "work",
  names_sep = "_",
  values_from = c(workplaces_percent_change_from_baseline),
)


combined = merge(transit,work,by='date',all=T)
combined$date=as.Date(combined$date)

##
#ts=ts(combined$workDrenthe,start=min(combined$date),end=max(combined$date),frequency = 1)

# Hp-filtering
hp <- hpfilter(ts, freq=200)


d1 <- hpfilter(d1, freq=200)





