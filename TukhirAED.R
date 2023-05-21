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
setwd("D:/Kuliah/!yes/R/AED")
df<-read.csv("mentah.csv")
df <- drop_na(df)
View(df)


#Exploration
  #distribution
par(mfrow = c(3, 3))
hist(df$Life.exp);hist(df$GDPPCPC);hist(df$Export);hist(df$Import);hist(df$Fertility);hist(df$ChildMortality); hist(df$Inflation)
par(mfrow = c(1, 1))
  #matrix corr
plot(corr_coef(df))

#standarization
df1<-df[2:8]
row.names(df1)<-df$Country.Name
df1<-scale(df1) #scale
View(df)

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


#clustering 2
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

  #1st visualization (use this one)
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

#analysis--------------------------------------------------------------------
countrybycluster1<- data.frame(df1,
                               cluster = as.factor(km1$cluster)
)
countrybycluster2<- data.frame(df2,
                               cluster = as.factor(km2$cluster)
)
  #check on excel
forexcel1 <- data.frame(country = row.names(df1),
                       countrybycluster1
)
forexcel2 <- data.frame(country = row.names(df2),
                        countrybycluster2
)

write_xlsx(forexcel1, "D:/Kuliah/!yes/R/AED/dummydf/all.xlsx")
write_xlsx(forexcel2, "D:/Kuliah/!yes/R/AED/dummydf/4var.xlsx")


  #1 developed
  #2 developing
  #3 underdeveloped

plot1<- ggplot(countrybycluster1, aes(y = Life.exp, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
plot2<-ggplot(countrybycluster1, aes(y = GDPPC, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
plot3<-ggplot(countrybycluster1, aes(y = Export, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
plot4<-ggplot(countrybycluster1, aes(y = Import, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
plot5<-ggplot(countrybycluster1, aes(y = Fertility, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
plot6<-ggplot(countrybycluster1, aes(y = ChildMortality, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
plot7<-ggplot(countrybycluster1, aes(y = Inflation, x = cluster, fill=cluster)) + geom_boxplot(alpha=0.3)
ggarrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, NULL, NULL,
          ncol = 3, nrow = 3,  align = "hv", 
          common.legend = TRUE)
