#library
library(cluster)
library(factoextra)
library(gridExtra)

#export data from csv and convert to dataframe
#data=read.csv(file.choose(), header=T, sep = ";")
#View(data)

customerMallData <- read.csv('D:\\Mall_Customers.csv')
df <- data.frame(customerMallData)
print(df)
summary(customerData)

#search for optimal k
data_fix<-scale(customerMallData)
data_fix
#metode elbow
fviz_nbclust(data_fix, kmeans, method = "wss")
#metode Sillhoutte
fviz_nbclust(data_fix, kmeans, method = "silhouette")
#Metode Gap Statistic
dim(df)
set.seed(9999)      #Mengunci data, nilainya ditentukan sendiri
#analisis cluster sampai keluar output
gap_stat <- clusGap(data_fix, FUN = kmeans, nstart = 20, K.max = 10, B = 200) #library cluster
fviz_gap_stat(gap_stat)

#mapping data to data frame
#factoextra
age <- df[,3] 
annualIncome <- df[,4]
spendingScore <- df[,5]
distance <- df[,6]
customerMallData <- data.frame(age, annualIncome, spendingScore, distance)

#k-means process
#library cluster
kmeans1 <- kmeans(data_fix, centers = 7, nstart = 30)
kmeans(customerMallData, centers = 7, nstart = 30)
fviz_cluster(kmeans1, data = data_fix) #factoextra & gridextra -> bentuk grafik

fviz_cluster(kmeans1, geom = "point", data = data_fix)+ggtitle("k=7")
final = data.frame(data_fix, kmeans1$cluster)
View(final)
