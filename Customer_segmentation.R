setwd("C:/Users/Akshay/Documents")
getwd()

install.packages("ggplot2")
install.packages("readxl")

options(java.parameters = "-Xmx4g" )

library(readxl)
library(ggplot2)

raw.data <- read_excel("BSI.xlsx")
View(raw.data)

data <- raw.data 
str(data)

length(unique(data$CUSTOMER))
sum(is.na(data$CUSTOMER))
data <- subset(data, !is.na(data$CUSTOMER))

data$item.return <- grepl("CM", data$INVC_NUMBER, fixed=TRUE) 
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)

View(data)

customers <- as.data.frame(unique(data$CUSTOMER))
names(customers) <- "CUSTOMER"

data$recency <- as.Date("2017-08-10") - as.Date(data$INVC_DATE)

temp <- subset(data, purchase.invoice == 1)

# Obtain # of days since most recent purchase
recency <- aggregate(recency ~ CUSTOMER, data=temp, FUN=min, na.rm=TRUE)
remove(temp)

# Add recency to customer data
customers <- merge(customers, recency, by="CUSTOMER", all=TRUE, sort=TRUE) 
remove(recency)

customers$recency <- as.numeric(customers$recency)

customer.invoices <- subset(data, select = c("CUSTOMER","INVC_NUMBER","purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CUSTOMER),]
row.names(customer.invoices) <- NULL

annual.invoices <- aggregate(purchase.invoice ~ CUSTOMER, data=customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

View(annual.invoices)

# Add # of purchase invoices to customers data
customers <- merge(customers, annual.invoices, by="CUSTOMER", all=TRUE, sort=TRUE) 
remove(customer.invoices)
remove(annual.invoices)

View(customers)
range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the past year
customers <- subset(customers, frequency > 0) 

# Total spent on each item on an invoice
data$Amount <- data$EXT_PRICE

annual.sales <- aggregate(Amount ~ CUSTOMER, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

customers <- merge(customers, annual.sales, by="CUSTOMER", all.x=TRUE, sort=TRUE)
remove(annual.sales)

hist(customers$monetary)
customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary)
hist(customers$monetary)

View(customers)

customers = unique(customers)

data$Qty <- data$QTY_SOLD

annual.qty <- aggregate(Qty ~ CUSTOMER, data=data, FUN=sum, na.rm=TRUE)
names(annual.qty)[names(annual.qty)=="Qty"] <- "TotQty"

customers <- merge(customers, annual.qty, by="CUSTOMER", all.x=TRUE, sort=TRUE)
remove(annual.qty)

View(customers)

hist(customers$TotQty)
customers$TotQty <- ifelse(customers$TotQty < 0, 0, customers$TotQty) # reset negative numbers to zero
hist(customers$TotQty)

data$Profit <- data$EXT_PROFIT

# Aggregated total qty to customer
annual.prof <- aggregate(Profit ~ CUSTOMER, data=data, FUN=sum, na.rm=TRUE)
names(annual.prof)[names(annual.prof)=="Profit"] <- "Profit"

customers <- merge(customers, annual.prof, by="CUSTOMER", all.x=TRUE, sort=TRUE)
remove(annual.prof)

#customers$ratio2 = with(customers, frequency/recency)

head(customers)

########################################################### Log-transform positively-skewed variables
customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 
customers$monetary.log <- log(customers$monetary.log)
customers$TotQty.log <- customers$TotQty +0.1
customers$TotQty.log <- log(customers$TotQty.log)
customers$Profit.log <- customers$Profit + 0.1
customers$Profit.log <- log(customers$Profit.log)

########################################################################################### Z-scores
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)
customers$TotQty.z <- scale(customers$TotQty.log, center=TRUE, scale=TRUE)
customers$Profit.z <- scale(customers$Profit.log, center=TRUE, scale=TRUE)


#find_na <- cbind(product$monetary, preprocessed$monetary.z)
#find_na[is.na(find_na[,2]),]

pre <- customers[,8:10]
head(pre)
#na.omit(pre)
set.seed(50)
#View(preprocessed)
#pre = na.omit(pre)

#kmeans
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


lm.out <- kmeans(pre, centers =6, nstart = 20)
lm.out
lm.out$cluster
customers <- cbind(customers,clusternumber = lm.out$cluster)

View(pre)
View(customers)

clus <- hclust(dist(customers[,12:16]))
View(clus)
plot(clus)
clusterCut <- cutree(clus, 10)
clusterCut
customers <- cbind(customers,clusternumber = clusterCut)

#customers$clusternumber = NULL

write.csv(file="cust_segmentation.csv",customers,row.names=F)


