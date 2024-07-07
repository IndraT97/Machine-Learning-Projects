#### Table 14.4

setwd("C:/Users/Indra/OneDrive/Desktop/BA-R/Data_sets")
library(arules)
fp.df <- read.csv("Faceplate.csv")
fp.df
fp.df1<-fp.df[,-1]
fp.df1
as(fp.df1, "transactions")

# remove first column and convert to matrix
fp.mat <- as.matrix(fp.df1)

# convert the binary incidence matrix into a transactions database
fp.trans <- as(fp.mat, "transactions")
inspect(fp.trans)

# plot data
itemFrequencyPlot(fp.trans)

## get rules
# when running apriori(), include the minimum support, minimum confidence, and target
# as arguments. 
rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules",minlen=2))
inspect(rules)

# inspect the first six rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 10))



#### Table 14.6

rules.tbl <- inspect(rules)
rules.tbl[rules.tbl$support >= 0.04 & rules.tbl$confidence >= 0.7,]

#### Table 14.8

all.books.df <- read.csv("CharlesBookClub.csv")

# create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
incid.books.mat <- as.matrix(incid.books.df)

#  convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)

# plot data
itemFrequencyPlot(books.trans)

# run apriori function
rules <- apriori(books.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

# inspect rules
inspect(sort(rules, by = "lift"))

inspect(sort(rules, by = "support"))

rules.tbl<- inspect(rules)
rules.tbl<-rules.tbl[rules.tbl$support >= 0.1 & rules.tbl$confidence >= 0.55,]
