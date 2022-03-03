## Question 1

# Using "tidyr" and "dplyr".
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
# 1st iteration
data.1 <- data.frame(x1 = c(4, 1, 0, 2, 6, 4), x2 = c(1, 3, 4, 1, 2, 0), 
                     cluster = c(2, 1, 1, 2, 1, 2))
plot(x = data.1$x1, y = data.1$x2, las = 1, xlab = expression("x"[1]), 
     ylab = expression("x"[2]))
cluster.1 <- data.1 %>%
  group_by(cluster) %>%
  summarize(mean(x1), mean(x2))
plot(x = data.1$x1, y = data.1$x2, las = 1, xlab = expression("x"[1]), 
     ylab = expression("x"[2]), col = ifelse(data.1$cluster == 1, "blue", "red"))
points(x = cluster.1[1, 2], y = cluster.1[1, 3], col = "blue", pch = 16)
points(x = cluster.1[2, 2], y = cluster.1[2, 3], col = "red", pch = 16)
data.1$dis_to_1 <- sqrt((data.1$x1 - as.numeric(cluster.1[1, 2]))^2 + 
                          (data.1$x2 - as.numeric(cluster.1[1, 3]))^2)
data.1$dis_to_2 <- sqrt((data.1$x1 - as.numeric(cluster.1[2, 2]))^2 + 
                          (data.1$x2 - as.numeric(cluster.1[2, 3]))^2)
data.1$cluster <- ifelse(data.1$dis_to_1 < data.1$dis_to_2, 1, 2)
# Following iteration
cluster.1 <- data.1 %>%
  group_by(cluster) %>%
  summarize(mean(x1), mean(x2))
data.1$dis_to_1 <- sqrt((data.1$x1 - as.numeric(cluster.1[1, 2]))^2 + 
                          (data.1$x2 - as.numeric(cluster.1[1, 3]))^2)
data.1$dis_to_2 <- sqrt((data.1$x1 - as.numeric(cluster.1[2, 2]))^2 + 
                          (data.1$x2 - as.numeric(cluster.1[2, 3]))^2)
data.1$cluster <- ifelse(data.1$dis_to_1 < data.1$dis_to_2, 1, 2)
data.1
# cluster.1 includes 2 and 3, and cluster.2 includes 1, 4, 5, and 6.