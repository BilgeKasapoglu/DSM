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
               stats,
               lubridate)

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

# # omitting the missing values
# combined <- na.omit(combined)
# transit <- na.omit(transit)
# work <- na.omit(work)

# apply hp filter on each column
combined_new = combined[,1] %>% as.data.frame()
names(combined_new)[1] <- "date"
regions <- names(combined)[2:ncol(combined)]
date = combined[,1]  %>% as.Date  
for (i in c(2:ncol(combined))){
  ts1 <- combined[,c(1,i)]
  ts1 = na.omit(ts1)
  ts = ts1[,2]
  smoothed <- hpfilter(ts, freq=200)
  ts1[,2] = as.matrix(smoothed$trend)
  combined_new = combined_new
  combined_new <- merge(ts1, combined_new, by = "date", all.y = TRUE)
  names(combined_new)[ncol(combined_new)] <- regions[i-1]
} 
summary(combined_new)
names(combined_new)[2:ncol(combined_new)] = regions

transit_hp = combined_new[,c(1:ncol(transit))]
work_hp = select(combined_new,c(date, workDrenthe:workZeeland))

cl <- rainbow(12)

plot(transit_hp$date, transit_hp$transitDrenthe, type="l", col = cl[1], ylim = c(-80,50))
lines(transit_hp$date, transit_hp$transitFlevoland, type = "l", col = cl[2])
lines(transit_hp$date, transit_hp$transitFriesland, type = "l", col = cl[3])
lines(transit_hp$date, transit_hp$transitGelderland, type = "l", col = cl[4])
lines(transit_hp$date, transit_hp$transitGroningen, type = "l", col = cl[5])
lines(transit_hp$date, transit_hp$transitLimburg, type = "l", col = cl[6])
lines(transit_hp$date, transit_hp$transitNorth Brabant, type = "l", col = cl[7])
lines(transit_hp$date, transit_hp$transitNorth Holland, type = "l", col = cl[8])
lines(transit_hp$date, transit_hp$transitOverijssel, type = "l", col = cl[9])
lines(transit_hp$date, transit_hp$transitSouth Holland, type = "l", col = cl[10])
lines(transit_hp$date, transit_hp$transitUtrecht, type = "l", col = cl[11])
lines(transit_hp$date, transit_hp$transitZeeland, type = "l", col = cl[12])

plot(work_hp$date, work_hp$workDrenthe, type="l", col = cl[1], ylim = c(-80,50))
lines(work_hp$date, work_hp$workFlevoland, type = "l", col = cl[2])
lines(work_hp$date, work_hp$workFriesland, type = "l", col = cl[3])
lines(work_hp$date, work_hp$workGelderland, type = "l", col = cl[4])
lines(work_hp$date, work_hp$workGroningen, type = "l", col = cl[5])
lines(work_hp$date, work_hp$workLimburg, type = "l", col = cl[6])
lines(work_hp$date, work_hp$workNorth Brabant, type = "l", col = cl[7])
lines(work_hp$date, work_hp$workNorth Holland, type = "l", col = cl[8])
lines(work_hp$date, work_hp$workOverijssel, type = "l", col = cl[9])
lines(work_hp$date, work_hp$workSouth Holland, type = "l", col = cl[10])
lines(work_hp$date, work_hp$workUtrecht, type = "l", col = cl[11])
lines(work_hp$date, work_hp$workZeeland, type = "l", col = cl[12])




# (b)
t <- transit_hp[,2:ncol(transit_hp)]
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
w <- work_hp[,2:ncol(work_hp)]
w <- na.omit(w)
w <- scale(w)
pc_work<-princomp(w,cor=TRUE,scores=TRUE)

loadings_work_1 <- pc_work$loadings[,1]
loadings_work_1

# The first component of the whole
whole <- combined_new[,2:ncol(combined_new)]
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

