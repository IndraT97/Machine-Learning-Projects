#### Table 2.3 

housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
View(housing.df)# show all the data in a new tab
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows


# Practice showing different subsets of the data
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns 
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column
housing.df$TOTAL.VALUE[1:10]  # show the first 10 rows of the TOTAL.VALUE column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column


#### Table 2.4

# random sample of 5 observations
s <- sample(row.names(housing.df), 5) # Here you will get the random row number from the entire dataset
housing.df[s,]

# oversample houses with over 10 rooms
# oversample --> Used to balance the data set
s <- sample(row.names(housing.df), 15, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]


#### Table 2.5

names(housing.df)  # print a list of variables to the screen.
colnames(housing.df)[1] <- c("TOTAL_VALUE")  # change the first column's name
class(housing.df$REMODEL) # REMODEL is a factor variable
class(housing.df[ ,14]) # Same. 
class(housing.df$BEDROOMS)  # BEDROOMS is an integer variable
class(housing.df[, 1])  # Total_Value is a numeric variable


# use model.matrix() to convert all categorical variables in the data frame into a set of dummy variables. We must then turn the resulting data matrix back into 
# a data frame for further work.
unique(housing.df$REMODEL)
xtotal <- model.matrix(~ 0 + REMODEL, data = housing.df)
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal)))  # check the names of the dummy variables
head(xtotal)
xtotal <- xtotal[, -3]  # drop one of the dummy variables. 
# In this case, drop REMODELRecent.

t(t(names(housing.df))) 
housing.df <- cbind(housing.df[, -c(14)], xtotal) 

#### Table 2.7

# To illustrate missing data procedures, we first convert a few entries for 
# bedrooms to NA's. Then we impute these missing values using the median of the 
# remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing, ] # It will return the row numbers from the whole data set
housing.df[rows.to.missing, ]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)  # Now we have 10 NA's and the median of the 
# remaining values is 3.

# replace the missing values using the median of the remaining values.
# use median() with na.rm = TRUE to ignore missing NA values when computing the median.
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS) 

#### Figure 3.1

Amtrak.df <- read.csv("Amtrak.csv")
head(Amtrak.df)
tail(Amtrak.df)
View(Amtrak.df)

# use time series analysis
#freq = It is 12 months a year
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))

#### Table 3.2

## Boston housing data
housing.df <- read.csv("BostonHousing.csv")
View(housing.df)
library(forecast)

## scatter plot with axes names
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "MDEV", ylab = "LSTAT")
# alternative plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.7)

## barchart of CHAS vs. mean MEDV
# compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV,  names.arg = data.for.plot$CHAS, 
        xlab = "CHAS", ylab = "Avg. MEDV")
# alternative plot with ggplot
ggplot(data.for.plot) + geom_bar(aes(x = CHAS, y = MeanMEDV), stat = "identity")

#### Figure 3.2

## histogram of MEDV
hist(housing.df$MEDV,xlab = "MEDV")
hist(housing.df$MEDV, breaks=20, xlab = "MEDV", freq = FALSE)
# alternative plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_histogram(aes(x = MEDV), binwidth = 5)

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")
# alternative plot with ggplot
ggplot(housing.df) + geom_boxplot(aes(x = as.factor(CHAS), y = MEDV)) + xlab("CHAS")


#### Figure 3.3

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX") #nitric oxides concentration
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT") #percentage values of lower status population
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO") # pupil-teacher ratios
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS") #proportions of non-retail business

#### Figure 3.4
## simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv = NA, Colv = NA)

## heatmap with values
library(gplots)
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(housing.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# alternative plot with ggplot

library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(housing.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))


#### Figure 3.6

## color plot
par(mfcol = c(1,1), xpd=TRUE) # allow legend to be displayed outside of plot area
plot(housing.df$NOX ~ housing.df$LSTAT, ylab = "NOX", xlab = "LSTAT",
     col = ifelse(housing.df$CAT..MEDV == 1, "black", "gray"))
# add legend outside of plotting area
# In legend() use argument inset =  to control the location of the legend relative
# to the plot.
legend("topleft", inset=c(0, -0.2), 
       legend = c("CAT.MEDV = 1", "CAT.MEDV = 0"), col = c("black", "gray"), 
       pch = 1, cex = 0.5)
# alternative plot with ggplot
library(ggplot2)
ggplot(housing.df, aes(y = NOX, x = LSTAT, colour= CAT..MEDV)) +
  geom_point(alpha = 0.6) 

## panel plots
# compute mean MEDV per RAD and CHAS
# In aggregate() use argument drop = FALSE to include all combinations
# (exiting and missing) of RAD X CHAS.
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, housing.df$CHAS), 
                           FUN = mean, drop = FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "meanMEDV") # accessibility to radial highways
# plot the data
par(mfcol = c(2,1))
barplot(height = data.for.plot$meanMEDV[data.for.plot$CHAS == 0], 
        names.arg = data.for.plot$RAD[data.for.plot$CHAS == 0], 
        xlab = "RAD", ylab = "Avg. MEDV", main = "CHAS = 0")
barplot(height = data.for.plot$meanMEDV[data.for.plot$CHAS == 1], 
        names.arg = data.for.plot$RAD[data.for.plot$CHAS == 1], 
        xlab = "RAD", ylab = "Avg. MEDV", main = "CHAS = 1")
# alternative plot with ggplot
ggplot(data.for.plot) +
  geom_bar(aes(x = as.factor(RAD), y = `meanMEDV`), stat = "identity") +
  xlab("RAD") + facet_grid(CHAS ~ .)



#### Figure 3.7

## simple plot
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal, 
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

# alternative, nicer plot (displayed)
library(GGally)
ggpairs(housing.df[, c(1, 3, 12, 13)])




#### Figure 3.8

options(scipen=999) # avoid scientific notation

## scatter plot: regular and log scale
plot(housing.df$MEDV ~ housing.df$CRIM, xlab = "CRIM", ylab = "MEDV")
# to use logarithmic scale set argument log = to either 'x', 'y', or 'xy'. 
plot(housing.df$MEDV ~ housing.df$CRIM, 
     xlab = "CRIM", ylab = "MEDV", log = 'xy')
# alternative log-scale plot with ggplot
library(ggplot2)
ggplot(housing.df) + geom_point(aes(x = CRIM, y = MEDV)) + 
  scale_x_log10(breaks = 10^(-2:2),
                labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  scale_y_log10(breaks = c(5, 10, 20, 40))

## boxplot: regular and log scale
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, 
        xlab = "CAT.MEDV", ylab = "CRIM")
boxplot(housing.df$CRIM ~ housing.df$CAT..MEDV, 
        xlab = "CAT.MEDV", ylab = "CRIM", log = 'y')


#### Figure 3.9

library(forecast)
Amtrak.df <- read.csv("Amtrak.csv")
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

## fit curve
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
lines(ridership.lm$fitted, lwd = 2)
# alternative plot with ggplot
library(ggplot2)
ggplot(Amtrak.df, aes(y = Ridership, x = Month, group = 12)) +
  geom_line() + geom_smooth(formula = y ~ poly(x, 1), method= "lm",
                            colour = "navy", se = FALSE, na.rm = TRUE)

#### Figure 3.12

library(MASS)
par(mfcol = c(2,1))
parcoord(housing.df[housing.df$CAT..MEDV == 0, -14], main = "CAT.MEDV = 0")
parcoord(housing.df[housing.df$CAT..MEDV == 1, -14], main = "CAT.MEDV = 1")