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
               mFilter,
               stats)

## Q1 (d)

m <- cbind(c(4,1,0,2,6,4),c(1,3,4,1,2,0))

cluster <- kmeans(m,2,nstart=5)
cluster$cluster


# Create a directory to store downloaded data
dir.create("A1/data")

# Download data
download_data <- function(url, filename){
  download.file(url = url, destfile = paste0(filename, ".csv"))
}

url <- "https://drive.google.com/uc?id=1DaYBBo_qohz-QiMOIPu08m0QXbjRsu0Q&export=download"

download_data(url, "2020_NL_Region_Mobility_Report")

# Load the data
dta <- read.csv("2020_NL_Region_Mobility_Report.csv")

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

# (b)
t <- transit[,2:ncol(transit)]
t <- na.omit(t)
t <- scale(t)
pc_transit<-princomp(t,cor=TRUE,scores=TRUE)

loadings_transit_2 <- pc_transit$loadings[,1:2]
loadings_transit_2


PC1 <- loadings[2] 
PC2 <- loadings[3] 

checkPC1 <- as.data.frame(loadings_transit_2) %>% arrange(abs(loadings_transit_2[,1]))
checkPC2 <- as.data.frame(loadings_transit_2) %>% arrange(abs(loadings_transit_2[,2]))

# (c)

pr.var=pc_transit$sdev^2
pve = pr.var/sum(pr.var)

# Put two plots side by side
par(mfrow=c(1,2))
plot(pve,xlab="PC",ylab="% Variance Explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="PC",ylab="Cumulative PVE",ylim=c(0,1),type='b')

# (d)

# The first component of the transit
loadings_transit_1 <- pc_transit$loadings[,1]

# The first component of the work
w <- work[,2:ncol(transit)]
w <- na.omit(w)
w <- scale(w)
pc_work<-princomp(w,cor=TRUE,scores=TRUE)

loadings_work_1 <- pc_work$loadings[,1]
loadings_work_1

# The first component of the whole
whole <- combined[,2:ncol(combined)]
whole <- na.omit(whole)
whole <- scale(whole)

pc_whole<-princomp(whole,cor=TRUE,scores=TRUE)

loadings_whole_1 <- pc_whole$loadings[,1]
loadings_whole_1


temp1 <- cbind(loadings_transit_1, loadings_work_1)
temp2 <- cbind(loadings_transit_1, loadings_whole_1)
temp3 <- cbind(loadings_work_1, loadings_whole_1)

region <- row.names(temp1)
region <- substring(region, 8)
region

plot(loadings_transit_1, loadings_work_1, type ="p")
plot(loadings_transit_1, loadings_whole_1, type ="p")
plot(loadings_work_1, loadings_whole_1, type ="p")

comb_trans_work <- append(loadings_transit_1, loadings_work_1)

par(mfrow(1, 2))
# work vs transit
plot(loadings_transit_1, loadings_work_1, type ="p")
abline(lm(loadings_work_1 ~ loadings_transit_1), lty = 2, col = "green")

# work + transit vs whole
plot(comb_trans_work, loadings_whole_1, type ="p")
abline(lm(loadings_whole_1 ~ comb_trans_work), lty = 2, col = "green")

# (e)?

# (f)

