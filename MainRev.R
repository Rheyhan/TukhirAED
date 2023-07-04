#ini untuk k clusterr--------------
#libraries
library(metan)
library(tidyr)
library(NbClust)
library(factoextra)
library(ggplot2)
library(cluster)
library(writexl)
library(dplyr)
library(ggpubr)

#getting dataframe
df<-read.csv("https://raw.githubusercontent.com/Rheyhan/TukhirAED/main/Mentah.csv")
df <- drop_na(df)
View(df)


#Exploration
  #distribution
par(mfrow = c(3, 3))
hist(df$Life.exp);hist(df$GDPPC);hist(df$Export);hist(df$Import);hist(df$Fertility);hist(df$ChildMortality); hist(df$Inflation)
par(mfrow = c(1, 1))
  #matrix corr
plot(corr_coef(df))

#standarization
df1<-df[2:8]
row.names(df1)<-df$Country.Name
df1<-scale(df1) #scale
View(df1)

#clustering 1 (Use this)
  #finding the optimal cluster
  #silhouette
  fviz_nbclust(df1, kmeans, method = "silhouette") #k=3
  #elbow
  fviz_nbclust(df1, kmeans, method = "wss") #k= 3||7
  #gap stat
  fviz_nbclust(x = df1, FUNcluster = kmeans, method = "gap_stat") #k=10
  #nbclust
  nb<-NbClust(data = df1, distance = "euclidean", method="kmeans") #k=4

  #1st visualization (use this one)
pam1 <- pam(df1, k=3, metric="euclidean") #using k=3
    #text
fviz_cluster(pam1, data=df1, geom="text", ellipse.type="norm", repel=TRUE, ggtheme=theme_bw()) +
  labs(title="Cluster plot", subtitle="k=3 clusters", x="Dim1, 20% variance", y="Dim2, 20% variance" )
    #point
fviz_cluster(pam1, data=df1, geom="point", ellipse.type="norm", repel=TRUE, ggtheme=theme_bw())

  #2nd visualization
set.seed(456)
km1 <- kmeans(x = df1, centers = 3) #k=3
km1
fviz_cluster(km1, geom = "point", data = df1)+ggtitle("k=3")
fviz_cluster(object = km1, data = df1, palette = "jco", ggtheme = theme_minimal())

fviz_silhouette(pam1)
pam1$medoids  #median based from the silhoutte


#analysis--------------------------------------------------------------------
countrybycluster1<- data.frame(cluster = km1$cluster
)
countrybycluster1
countrybycluster1["cluster"][countrybycluster1["cluster"] == 1] <- 0
countrybycluster1["cluster"][countrybycluster1["cluster"] == 3] <- 1
countrybycluster1
countrybycluster1<-data.frame(df1,
                              cluster=as.factor(countrybycluster1$cluster))
countrybycluster1


a=scale_color_manual(values = c("0" = "blue", "1"="green", "2"="red")) 

plot1<- ggplot(countrybycluster1, aes(y = Life.exp, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot2<-ggplot(countrybycluster1, aes(y = GDPPC, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot3<-ggplot(countrybycluster1, aes(y = Export, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot4<-ggplot(countrybycluster1, aes(y = Import, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot5<-ggplot(countrybycluster1, aes(y = Fertility, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3)+a
plot6<-ggplot(countrybycluster1, aes(y = ChildMortality, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3) +a
plot7<-ggplot(countrybycluster1, aes(y = Inflation, x = cluster, color=cluster)) + geom_boxplot(alpha=0.3) +a

ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, NULL, NULL,
          ncol = 3, nrow = 3,  align = "hv", 
          common.legend = TRUE)




#to check the data on excel
forexcel1 <- data.frame(country = row.names(df1),
                        countrybycluster1
)
write_xlsx(forexcel1, "D:/Kuliah/!yes/R/AED/dummydf/all.xlsx")




#Further Exploration if 4 variable were used instead (export, import, life exp, and GDPPCpc)-----------------
#clustering 2 (Don't use this)
#use 4 variable (export, import, life exp, and GDPPCpc)
df2<-df[2:5]
row.names(df2)<-df$Country.Name
df2<-scale(df2) #scale
#finding the optimal cluster
#silhouette
fviz_nbclust(df2, kmeans, method = "silhouette") #k=2
#elbow
fviz_nbclust(df2, kmeans, method = "wss") #k= 4
#gap stat
fviz_nbclust(x = df2, FUNcluster = kmeans, method = "gap_stat") #k=2
#nbclust
nb<-NbClust(data = df2, distance = "euclidean", method="kmeans") #k=2

#1st visualization
pam2 <- pam(df2, k=2, metric="euclidean") #using k=2
#text
fviz_cluster(pam2, data=df2, geom="text", ellipse.type="norm", repel=TRUE, ggtheme=theme_bw()) +
  labs(title="Cluster plot", subtitle="k=2 clusters", x="Dim1, 20% variance", y="Dim2, 20% variance" )
#point
fviz_cluster(pam2, data=df2, geom="point", ellipse.type="norm", repel=TRUE, ggtheme=theme_bw())

#2nd visualization  
set.seed(456)
km2 <- kmeans(x = df2, centers = 2) #k=2
km2
fviz_cluster(km2, geom = "point", data = df2)+ggtitle("k=3")
fviz_cluster(object = km2, data = df2, palette = "jco", ggtheme = theme_minimal())

fviz_silhouette(pam2)
pam2$medoids  #median based from silhoutte