View(crm)
mydata <- crm[,(2:5)]

# Load data as mydata
View(mydata)
attach(mydata)

# Data exploration
summary(mydata)
str(mydata)

install.packages("moments")
library(moments)

# Graphical exploration
hist(Murder)
summary(Murder)
skewness(Murder)
kurtosis(Murder)
qqnorm(Murder)
qqline(Murder)

hist(Assault)
summary(Assault)
skewness(Assault)
kurtosis(Assault)
qqnorm(Assault)
qqline(Assault)

hist(UrbanPop)
summary(UrbanPop)
skewness(UrbanPop)
kurtosis(UrbanPop)
qqnorm(UrbanPop)
qqline(UrbanPop)

hist(Rape)
summary(Rape)
skewness(Rape)
kurtosis(Rape)
qqnorm(Rape)
qqline(Rape)

#Normalizing the data
normalized_data <- scale(mydata) 
View(normalized_data)

?dist
# Calculating the distance
d <- dist(normalized_data, method = "euclidean") 

# Hierarchial clustering the data

# Single method
fit <- hclust(d, method="single")
fit

# display dendrogram
plot(fit) 
plot(fit, hang=-1)

# Cutting the dendogram
groups <- cutree(fit, k=5)# cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

membership<-as.matrix(groups)

aggregate(mydata, by=list(membership), FUN=mean)


# Complete method
fit1 <- hclust(d, method="complete")
fit1

# display dendrogram
plot(fit1) 
plot(fit1, hang=-1)

# Cutting the dendogram
groups1 <- cutree(fit1, k=4)

rect.hclust(fit1, k=4, border="red")

membership1<-as.matrix(groups1)

aggregate(mydata, by=list(membership1), FUN=mean)


# Centroid method
fit2 <- hclust(d, method="centroid")
fit2

# display dendrogram
plot(fit2) 
plot(fit2, hang=-1)

# Cutting the dendogram
groups2 <- cutree(fit2, k=5)

rect.hclust(fit2, k=5, border="red")

membership2<-as.matrix(groups2)

aggregate(mydata, by=list(membership2), FUN=mean)

# ward.D method
fit3 <- hclust(d, method="ward.D")
fit3

# display dendrogram
plot(fit3) 
plot(fit3, hang=-1)

# Cutting the dendogram
groups <- cutree(fit3, k=4)

rect.hclust(fit3, k=4, border="red")

membership<-as.matrix(groups)

aggregate(mydata, by=list(membership), FUN=mean)

# Complete type is selescted for effective clustering
final <- data.frame(crm, membership1)
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)



#"K means clustering"
View(crm)
mydata <- crm[,c(2:5)] 
View(mydata)

# Normalizing the data
normalized_data <- scale(mydata)
View(normalized_data)

#elbow curve to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:9) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:9, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# K means clustering
fit <- kmeans(normalized_data, 4) 
str(fit)

final<- data.frame(crm, fit$cluster) # append cluster membership
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final)

aggregate(mydata[,1:4], by=list(fit$cluster), FUN=mean)
