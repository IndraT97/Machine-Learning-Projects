#### Table 15.2
dir1 <- "C:/Users/Indra/OneDrive/Desktop/BA-R/Data_sets"
setwd(dir1)
utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
utilities.df
# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df, method = "euclidean")
d

#### Table 15.4

# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)
utilities.df.norm
# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")
d.norm


#### Figure 15.3
# compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm, method = "euclidean")

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)

hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

library(cluster)
agnes(d.norm, method = "single")$ac
agnes(d.norm, method = "average")$ac

hc3 <- hclust(d.norm, method = "ward.D")
plot(hc3, hang = -1, ann = FALSE)


#### Table 15.6

memb <- cutree(hc1, k = 6)
memb

memb <- cutree(hc2, k = 6)
memb

# better grouping, dendexten
plot(hc2, cex = 1)
rect.hclust(hc2, k = 6, border = 2:5)

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

library(dendextend)
tanglegram(dend1, dend2)

#### Figure 15.4

# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
