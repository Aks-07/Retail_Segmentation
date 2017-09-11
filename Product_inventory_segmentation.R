setwd("C:/Users/Akshay/Documents")
getwd()

install.packages("ggplot2")
install.packages("readxl")

options(java.parameters = "-Xmx4g" )

library(readxl)
library(ggplot2)

raw.data <- read_excel("Use.xlsx")
View(raw.data)

data <- raw.data 
str(data)

#missing product
length(unique(data$PART_NUMBER))
sum(is.na(data$PART_NUMBER))
data <- subset(data, !is.na(data$PART_NUMBER))

# Identify returns
#data$item.return <- grepl("CM", data$INVC_NUMBER, fixed=TRUE) 
#data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)

############################################################# Create product-level dataset #

product <- as.data.frame(unique(data$PART_NUMBER))
names(product) <- "PART_NUMBER"

data$recency <- as.Date("2017-08-10") - as.Date(data$INVC_DATE)

temp <- subset(data, purchase.invoice == 1)
# Obtain # of days since most recent purchase
recency <- aggregate(recency ~ PART_NUMBER, data=temp, FUN=min, na.rm=TRUE)
remove(temp)

# Add recency to product data
product <- merge(product, recency, by="PART_NUMBER", all=TRUE, sort=TRUE) 
remove(recency)

product$recency <- as.numeric(product$recency)

product.invoices <- subset(data, select = c("PART_NUMBER","INVC_NUMBER","purchase.invoice"))
product.invoices <- product.invoices[!duplicated(product.invoices), ]
product.invoices <- product.invoices[order(product.invoices$PART_NUMBER),]
row.names(product.invoices) <- NULL

# Number of invoices/year (purchases only)
annual.invoices <- aggregate(purchase.invoice ~ PART_NUMBER, data=product.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

product <- merge(product,annual.invoices, by="PART_NUMBER", all=TRUE, sort=TRUE) 
remove(product.invoices)

# Remove customers who have not made any purchases in the past year
product <- subset(product, frequency > 0) 
remove(annual.invoices)

#Total spent on each item on an invoice
data$Amount <- data$EXT_PRICE

# Aggregated total sales to product
annual.sales <- aggregate(Amount ~ PART_NUMBER, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to product dataset
product <- merge(product, annual.sales, by="PART_NUMBER", all.x=TRUE, sort=TRUE)
remove(annual.sales)

hist(product$monetary)
product$monetary <- ifelse(product$monetary < 0, 0, product$monetary) # reset negative numbers to zero
hist(product$monetary)

data$Qty <- data$QTY_SOLD

# Aggregated total qty to customer
annual.qty <- aggregate(Qty ~ PART_NUMBER, data=data, FUN=sum, na.rm=TRUE)
names(annual.qty)[names(annual.qty)=="Qty"] <- "TotQty"

# Add qty value to customer dataset
product <- merge(product, annual.qty, by="PART_NUMBER", all.x=TRUE, sort=TRUE)
remove(annual.qty)

head(product)

hist(product$TotQty)
product$TotQty <- ifelse(product$TotQty < 0, 0, product$TotQty)
hist(product$TotQty)

data$Profit <- data$EXT_PROFIT

annual.prof <- aggregate(Profit ~ PART_NUMBER, data=data, FUN=sum, na.rm=TRUE)
names(annual.prof)[names(annual.prof)=="Profit"] <- "Profit"

product <- merge(product, annual.prof, by="PART_NUMBER", all.x=TRUE, sort=TRUE)
remove(annual.prof)

hist(product$Profit)
product$Profit <- ifelse(product$Profit < 0, 0, product$Profit) # reset negative numbers to zero
hist(product$Profit)

#product$ratio2 = with(product, frequency/recency)

head(product)

data$CELL <- data$INV_CELL

annual.prof <- aggregate(CELL ~ PART_NUMBER, data=data, FUN=sum, na.rm=TRUE)
names(annual.prof)[names(annual.prof)=="CELL"] <- "CELL"

product <- merge(product, annual.prof, by="PART_NUMBER", all.x=TRUE, sort=TRUE)
remove(annual.prof)

data$VINT <- data$VINTAGE

annual.prof <- aggregate(VINT ~ PART_NUMBER, data=data, FUN=sum, na.rm=TRUE)
names(annual.prof)[names(annual.prof)=="VINT"] <- "VINT"

product <- merge(product, annual.prof, by="PART_NUMBER", all.x=TRUE, sort=TRUE)
remove(annual.prof)

head(product)

###########################################################3# Log-transform positively-skewed variables
product$recency.log <- log(product$recency)
product$frequency.log <- log(product$frequency)
product$monetary.log <- product$monetary + 0.1
product$monetary.log <- log(product$monetary.log)
product$TotQty.log <- product$TotQty +0.1
product$TotQty.log <- log(product$TotQty.log)
product$Profit.log <- product$Profit + 0.2
product$Profit.log <- log(product$Profit.log)
product$CELL.log <- product$CELL + 0.1
product$CELL.log <- log(product$CELL.log)
product$VINT.log <- product$VINT + 0.1
product$VINT.log <- log(product$VINT.log)


########################################################################################## Z-scores
product$recency.z <- scale(product$recency.log, center=TRUE, scale=TRUE)
product$frequency.z <- scale(product$frequency.log, center=TRUE, scale=TRUE)
product$monetary.z <- scale(product$monetary.log, center=TRUE, scale=TRUE)
product$TotQty.z <- scale(product$TotQty.log, center=TRUE, scale=TRUE)
product$Profit.z <- scale(product$Profit.log, center=TRUE, scale=TRUE)
product$CELL.z <- scale(product$CELL.log, center=TRUE, scale=TRUE)
product$VINT.z <- scale(product$VINT.log, center=TRUE, scale=TRUE)

#product$Profit.log = NULL
#head(product)
#sum(is.na(product$Profit.log))
#product[is.na(product)] = -2.302585

#find_na <- cbind(product$monetary, preprocessed$monetary.z)
#find_na[is.na(find_na[,2]),]

pre <- product[,8:10]
head(pre)
#na.omit(pre)
set.seed(50)
#View(preprocessed)
#pre = na.omit(pre)

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pre, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


lm.out <- kmeans(pre, centers = 4) # View the resulting model
lm.out
lm.out$cluster
product <- cbind(product,clusternumber6 = lm.out$cluster)

View(pre)
View(product)

clus <- hclust(dist(product[,12:16]))
View(clus)
plot(clus)
clusterCut <- cutree(clus, 10)
#table(clusterCut, iris$Species)
clusterCut
product <- cbind(product,clusternumber = clusterCut)

#customers$clusternumber = NULL

write.csv(file="product_seg.csv", product,row.names = F)

