View(air)
mydata <- air[,(2:12)]
attach(mydata)

# Load data as mydata
View(mydata)

# Data Exploration
summary(mydata)
str(mydata)

install.packages("moments")
library(moments)

# Graphical exploration
hist(Bonus_trans)
summary(Bonus_trans)
skewness(Bonus_trans)
kurtosis(Bonus_trans)
qqnorm(Bonus_trans)
qqline(Bonus_trans)

hist(Days_since_enroll)
summary(Days_since_enroll)
skewness(Days_since_enroll)
kurtosis(Days_since_enroll)
qqnorm(Days_since_enroll)
qqline(Days_since_enroll)

# Hierarchial clustering
normalized_data <- scale(mydata) 

# Calculating the distance
d <- dist(normalized_data, method = "euclidean") 

# Single method
fit <- hclust(d, method="single")
fit

# display dendrogram
plot(fit) 
plot(fit, hang=-1)


# Complete method
fit1 <- hclust(d, method="complete")
fit1

# display dendrogram
plot(fit1) 
plot(fit1, hang=-1)


# Centroid method
fit2 <- hclust(d, method="centroid")
fit2

# display dendrogram
plot(fit2) 
plot(fit2, hang=-1)


# Ward.D method
fit3 <- hclust(d, method="ward.D")
fit3

# display dendrogram
plot(fit3) 
plot(fit3, hang=-1)

# Cutting the dendogram
groups3 <- cutree(fit3, k=3)

rect.hclust(fit3, k=3, border="red")

membership3 <-as.matrix(groups3)

aggregate(mydata, by=list(membership3), FUN=mean)

# Clusters 
final <- data.frame(air, membership3)
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)



#"K means clustering"
View(air)
mydata <- air[,c(2:12)] 

View(mydata)

# Normalizing the data
normalized_data <- scale(mydata)
View(normalized_data)

install.packages("kselection")
library(kselection)

# selecting K for kmeans clustering using kselection
k <- kselection(air[,-1], parallel = TRUE, k_threshold = .95, )
k

fit <- kmeans(normalized_data, 2) 
str(fit)
final2<- data.frame(air, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate(mydata[,1:11], by=list(fit$cluster), FUN=mean)

#elbow curve 
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:8) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_data, 3) 
str(fit)
final2<- data.frame(air, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
View(final3)
aggregate(mydata[,1:11], by=list(fit$cluster), FUN=mean)



