#### Table 15.2

utilities.df <- read.csv("Utilities.csv")
utilities.df

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = manhattan)
d <- dist(utilities.df, method = "euclidean")
d

#### Table 15.4

# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)
utilities.df.norm

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales (large scale) and FuelCost (small scale)
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")



#### Table 15.9


# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6)

# show cluster membership
km$cluster



#### Table 15.10
# centroids
km$centers
km$withinss
km$size


#### Figure 15.5

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 3, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x =0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))



#### Table 15.11

dist(km$centers)






