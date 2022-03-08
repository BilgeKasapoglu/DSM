##############################
#### DATA SCIENCE METHODS ####
##############################

#######################
#### ASSIGNMENT 01 ####
#######################

# GROUP 01
# Bilge Kasapoğlu – u941664
# Hoan Van Nguyen – u1274449
# Jiahe Wang – u489199
# Roshini Sudhaharan – u725261

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

#####################
#### QUESTION 01 ####
#####################

#1a.

#create matrix with n = 6 observations and p = 2 features; third column indicating cluster: 1 is cluster A, 2 is cluster B
set.seed(1)
m = cbind(x1 = c(4, 1, 0, 2, 6, 4), x2 = c(1, 3, 4, 1, 2, 0), clusters = c(2,1,1,2,1,2))
m

#plot x1 against x2
plot(m[,1], m[,2], col = ifelse(m[,3] == 1, "blue", "red"))
legend(3,4,legend = c("Cluster A", "Cluster B"), col = c("Blue", "Red"), pch = 1)

#1b.

#calculate centroids for two clusters A & B and plot them
centroidA = c(mean(m[m[,3]==1, 1]), mean(m[m[,3]==1, 2]))
centroidB = c(mean(m[m[,3]==2, 1]), mean(m[m[,3]==2, 2]))

points(x = centroidA[1], y = centroidA[2], col = "blue", pch = 16, cex = 2)
points(x = centroidB[1], y = centroidB[2], col = "red", pch = 16, cex = 2)

#1c.

#assign each observations to the centroid to which it is the closest in terms of Euclidean distance, then plot the observations with the new cluster membership
euclidean = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_clusters = function(m, centroidA, centroidB) {
  new_clusters = rep(NA, nrow(m))
  for (i in 1:nrow(m)) {
    if (euclidean(m[i,], centroidA) < euclidean(m[i,], centroidB)) {
      new_clusters[i] = 1
    } else {
      new_clusters[i] = 2
    }
  }
  return(clusters)
}
new_clusters = assign_clusters(m, centroidA, centroidB)
new_clusters

#new matrix including new cluster membership
m1 <- cbind(m, new_clusters)
m1

plot(m1[,1], m1[,2], col = ifelse(m1[,4] == 1, "blue", "red"))
legend(3,4,legend = c("Cluster A", "Cluster B"), col = c("Blue", "Red"), pch = 1)

#1d.

#repeat 1c until the answer stops changing
last_clusters = rep(-1, 6)
while (!all(last_clusters == new_clusters)) {
  last_clusters = new_clusters
  centroidA = c(mean(m1[m1[,4] == 1, 1]), mean(m1[m1[,4] == 1, 2]))
  centroidB = c(mean(m1[m1[,4] == 2, 1]), mean(m1[m1[,4] == 2, 2]))
  print(centroidA)
  print(centroidB)
  new_clusters = assign_clusters(m1, centroidA, centroidB)
}
new_clusters

#####################
#### QUESTION 02 ####
#####################

#2a. 

#create a directory to store downloaded data
dir.create("A1/data")

#download data
download_data <- function(url, filename){
  download.file(url = url, destfile = paste0(filename, ".csv"))
}

url <- "https://drive.google.com/uc?id=1DaYBBo_qohz-QiMOIPu08m0QXbjRsu0Q&export=download"

download_data(url, "A1/data/2020_NL_Region_Mobility_Report")

#load the data
dta <- read.csv("A1/data/2020_NL_Region_Mobility_Report.csv")

#inspect the data
dim(dta)
head(dta)
View(dta)
summary(dta)

#clean and select desired variables
d = dta[!(is.na(dta$sub_region_1) | dta$sub_region_1 =="") & (is.na(dta$sub_region_2) | dta$sub_region_2==""),]
d <- d %>% select(date,sub_region_1,sub_region_2,
                  transit_stations_percent_change_from_baseline, 
                  workplaces_percent_change_from_baseline, 
)

# reshaping the dataset
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

#2a.

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

plot(transit_hp$date, transit_hp$transitDrenthe, type="l", col = cl[1], ylim = c(-80,50), xlab = "Date", ylab = "Trend in Transit")
lines(transit_hp$date, transit_hp$transitFlevoland, type = "l", col = cl[2])
lines(transit_hp$date, transit_hp$transitFriesland, type = "l", col = cl[3])
lines(transit_hp$date, transit_hp$transitGelderland, type = "l", col = cl[4])
lines(transit_hp$date, transit_hp$transitGroningen, type = "l", col = cl[5])
lines(transit_hp$date, transit_hp$transitLimburg, type = "l", col = cl[6])
lines(transit_hp$date, transit_hp$transitNorthBrabant, type = "l", col = cl[7])
lines(transit_hp$date, transit_hp$transitNorthHolland, type = "l", col = cl[8])
lines(transit_hp$date, transit_hp$transitOverijssel, type = "l", col = cl[9])
lines(transit_hp$date, transit_hp$transitSouthHolland, type = "l", col = cl[10])
lines(transit_hp$date, transit_hp$transitUtrecht, type = "l", col = cl[11])
lines(transit_hp$date, transit_hp$transitZeeland, type = "l", col = cl[12])

#We see a comovement in transit among different regions. 

plot(work_hp$date, work_hp$workDrenthe, type="l", col = cl[1], ylim = c(-80,50), xlab = "Date", ylab = "Trend in Work")
lines(work_hp$date, work_hp$workFlevoland, type = "l", col = cl[2])
lines(work_hp$date, work_hp$workFriesland, type = "l", col = cl[3])
lines(work_hp$date, work_hp$workGelderland, type = "l", col = cl[4])
lines(work_hp$date, work_hp$workGroningen, type = "l", col = cl[5])
lines(work_hp$date, work_hp$workLimburg, type = "l", col = cl[6])
lines(work_hp$date, work_hp$workNorthBrabant, type = "l", col = cl[7])
lines(work_hp$date, work_hp$workNorthHolland, type = "l", col = cl[8])
lines(work_hp$date, work_hp$workOverijssel, type = "l", col = cl[9])
lines(work_hp$date, work_hp$workSouthHolland, type = "l", col = cl[10])
lines(work_hp$date, work_hp$workUtrecht, type = "l", col = cl[11])
lines(work_hp$date, work_hp$workZeeland, type = "l", col = cl[12])

#We see a comovement in work among different regions. However, we think the comovement is weaker in comparison to transit. 

#2b.

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

#loadings plot
plot(loadings_transit_2)

#We see that provinces cluster together. It seems like they cluster together on the second loadings.

#2c.

pr.var=pc_transit$sdev^2
pve = pr.var/sum(pr.var)

#put two plots side by side
par(mfrow=c(1,2))
plot(pve,xlab="PC",ylab="% Variance Explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="PC",ylab="Cumulative PVE",ylim=c(0,1),type='b')

#The first two components seem to explain the most of the variation in the data.
#Thus, the first two components seems to be the appropriate solution for the 
#PCA problem.

#2d.

#The first component of the transit
loadings_transit_1 <- pc_transit$loadings[,1]

#The first component of the work
w <- work_hp[,2:ncol(work_hp)]
w <- na.omit(w)
w <- scale(w)
pc_work<-princomp(w,cor=TRUE,scores=TRUE)

loadings_work_1 <- pc_work$loadings[,1]
loadings_work_1

#The first component of the whole
whole <- combined_new[,2:ncol(combined_new)]
whole <- na.omit(whole)
whole <- scale(whole)

pc_whole<-princomp(whole,cor=TRUE,scores=TRUE)

loadings_whole_1 <- pc_whole$loadings[,1]
loadings_whole_1

plot(loadings_transit_1, loadings_work_1, type ="p")

comb_trans_work <- append(loadings_transit_1, loadings_work_1)

par(mfrow = c(1, 2))
#the plot : work vs transit
plot(loadings_transit_1, loadings_work_1, type ="p")
abline(lm(loadings_work_1 ~ loadings_transit_1), lty = 2, col = "green")
#It seems like the first PCs of Transit and Work do not seem to comove with
#each other. 

#the plot : work + transit vs whole
plot(comb_trans_work, loadings_whole_1, type ="p")
abline(lm(loadings_whole_1 ~ comb_trans_work), lty = 2, col = "green")
#This is the plot of the first PCs of Work and Transit vs the combined data. 
#It seems like they comove with each other pretty much. 

#It makes more intuitive sense that the second one has comovement because the 
#points are close to the green line (45 degrees line). We do not observe the 
#same thing from the first plot.

#2e.

#Explained variance in %  - transit
pr.var=pc_transit$sdev^2
pve = pr.var/sum(pr.var) # the first PC explains 96% var. in the data


#Explained variance in % - work
pr.var_work=pc_work$sdev^2
pve_work = pr.var_work/sum(pr.var_work) # the first PC explains 89% var. in the data

#Explained variance in % - combined
pr.var_combined=pc_whole$sdev^2
pve_combined = pr.var_combined/sum(pr.var_combined) # the first PC explains 85% var. in the data

#We think that work and transit are closer to a consistent estimate of the true
#underlying mobility since their first PC explains a higher variation in the data
#in comparison to the combined one. 

#When we compare work and transit, transit seems to be closer to the consistent
#estimate since it explains a higher variation in the data.

#2f.

#The scores of the first component of the whole
scores_whole_1 <- pc_whole$scores[,1]

#the plot : the combined PC from the whole dataset over time, with two lockdowns
plot(1:296, scores_whole_1, type ="p", xlab = "Date", ylab = "The Combined PC (Scores)")
abline(v = c(31, 265), lty = c(2, 2), lwd = c(2, 2), col = c("green", "red"))
text(c(60, 236), c(-10, -10), c("2020-03-16", "2020-12-15"), col = c("green", "red"))
#The first lockdown on 2020-03-16 was most successful. In the second one, it was 
#already low compared to the first one, so the marginal effect was much stronger 
#in the first one.