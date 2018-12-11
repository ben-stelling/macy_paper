setwd("C:/Users/bds4513/Dropbox/stats/final_proj/Final_project")


#insert data and transpose to give us format we want
boem1 = read.csv("Boem_1.csv", header = T, row.names = 1)
boem2 = read.csv("Boem_2.csv", header = T, row.names = 1)
#transpose the matrix to give us what we want
boem.t1 = t(boem1)
boem.t1 = as.data.frame(boem.t1)
boem.t1[1,5]

boem.t2 = t(boem2)
boem.t2 = as.data.frame(boem.t2)
boem.t1
boem.t2
dim(boem.t1)
str(boem.t1)
dim(boem.t2)
str(boem.t2)
#make col names the first row

#remove the dates, etc only leaving numbers
boem.t1_num = boem.t1[,-c(1:9)]
boem.t2_num = boem.t2[,-c(1:9)]
str(boem.t2)

# appears to be a character matrix so...got to make it numerical
#apply function magically does this for us
?apply

boem.num1 = apply(boem.t1_num,2,as.numeric)
boem.num1 = as.data.frame(boem.num1)
boem.num2 = apply(boem.t2_num,2,as.numeric)
boem.num2 = as.data.frame(boem.num2)

range(boem.num1)


#If you want to subset the 
#boem.sub = subset(boem.num, , -c(`Alexandrium monilatum`,`Amphidinium carterae`))

#look and make sure the apply turned to numeric
str(boem.num)
max(boem.num)
boem.num
#log transformation may be useful
#we want it to be from 0 to about 10
log.boem.num1 = log1p(boem.num1)
log.boem.num2 = log1p(boem.num2)
range(log.boem.num1)
range(log.boem.num2)
?log1p

install.packages("MVA")
install.packages("psych")
install.packages("Hmisc")
install.packages("StatMatch")
install.packages("MASS")
install.packages("raster")
install.packages("pvclust")
install.packages("vegan")
install.packages("cluster")
install.packages("vegan")
install.packages("ca")
install.packages("gplots")

library(raster)
library(cluster)
library(Hmisc)
library(pvclust)
library(vegan)
library(ca)
library(MVA)
library(psych)
library(Hmisc)
library(StatMatch)
library(MASS)
library(gplots)

#gives you description of each variable
describe(boem.num)
?desc

#compute the dissimilarity matrix for both the log and normal data
?vegdist
#dist.boem1 = vegdist(boem.num1, "bray")
log.dist.boem1 = vegdist(log.boem.num1, "bray")
#dist.boem2 = vegdist(boem.num1, "bray")
log.dist.boem2 = vegdist(log.boem.num2, "bray")
#for future use____________________________
#use the factor part and bind to numerical portion
boem.t.vars1 = boem.t1[,1:9]
boem.t.vars2 = boem.t2[,1:9]

#recreate table with character variables
boem.final1 = cbind(boem.t.vars1,boem.num1)
boem.final2 = cbind(boem.t.vars2,boem.num2)




#################################################################################
#K-MEANS attempt

#z-score of both data sets
?kmeans
boem.z1 = as.data.frame(scale(log.boem.num1))
boem.z2 = as.data.frame(scale(log.boem.num2))

range(boem.z1)
range(boem.z2)

#number of obs minus 1 (0, N-1), i in 1:N-1
#Visualize untransformed k-means 
#to determine # of K groups
#Takes a while
#Within group sum of squares
wss <- rep(0, 255)
for (i in 1:255)
  wss[i] <- sum(kmeans(boem.z1, centers = i,nstart=25)$withinss) 
wss
plot(1:255, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares") 

#Silhouette width comparison - distance btwn groups
sil <- rep(0,255)
for (i in 2:255)
  sil[i] <- summary(silhouette(kmeans(boem.z1, centers=i, iter.max = 100, nstart=25)$cluster, dist(boem.num)))$avg.width
plot(1:255, sil[1:255], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")
#maybe 2?


#boem.z if of the log transformed k-means b/c used z-standardized data
wss2 <- rep(0, 255)
for (i in 1:255)
  wss2[i] <- sum(kmeans(boem.z2, centers = i,nstart=25)$withinss) 
wss2
plot(1:255, wss2, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares") 


sil2 <- rep(0,255)
for (i in 2:255)
  sil2[i] <- summary(silhouette(kmeans(boem.z2, centers=i, iter.max = 100, nstart=25)$cluster, dist(log.boem.num)))$avg.width
plot(2:255, sil2[2:255], type = "b", xlab = "Number of groups", ylab = "average silhouette width ")
# I would say 3!



#Take the k-means of the data sets
#z-standardized data
log.boem.kop1 <- kmeans(log.boem.num1, centers=3, iter.max=10, nstart=25)
log.boem.kop2 <- kmeans(log.boem.num2, centers=3, iter.max=10, nstart=25)
#boem.kop]] = kmeans(boem.z.2, centers=3, iter.max=10, nstart=25)
kmeans.z1 = kmeans(boem.z1, centers=3, iter.max=10, nstart=25)
kmeans.z2 = kmeans(boem.z1, centers=3, iter.max=10, nstart=25)

#______________________________________________________________________________#
#make a color vector using boem.num because it doesnt matter as long as there is
# 256 items
#______________________________________________________________________________#

#k-means untransformed boem data colors
#my.color.vector <- rep("green", times=nrow(boem.num))
#my.color.vector[boem.kop$cluster==1] <- "blue"
#my.color.vector[boem.kop$cluster==2] <- "green"
#my.color.vector[boem.kop$cluster==3] <- "red"
#my.color.vector[boem.kop$cluster==4] <- "orange"
#my.color.vector[boem.kop$cluster==5] <- "brown"
#my.color.vector[boem.kop$cluster==6] <- "cyan"
#my.color.vector[boem.kop$cluster==7] <- "black"

#Log k-means colors - BOEM 1
my.color.vector1 <- rep("green", times=nrow(boem.num1))
my.color.vector1[log.boem.kop1$cluster==1] <- "blue"
my.color.vector1[log.boem.kop1$cluster==2] <- "green"
my.color.vector1[log.boem.kop1$cluster==3] <- "red"
#my.color.vector2[log.boem.kop$cluster==4] <- "orange"
#my.color.vector2[log.boem.kop$cluster==5] <- "brown"

my.color.vector2 <- rep("green", times=nrow(boem.num2))
my.color.vector2[log.boem.kop2$cluster==1] <- "blue"
my.color.vector2[log.boem.kop2$cluster==2] <- "green"
my.color.vector2[log.boem.kop2$cluster==3] <- "red"

#Monthly colors
unique(boem.final1$Month)
my.color.vector3 <- rep("green", times=nrow(boem.num1))
my.color.vector3[boem.final1$Month=="October"] <- "blue"
my.color.vector3[boem.final1$Month=="February"] <- "green"
my.color.vector3[boem.final1$Month=="May"] <- "red"
my.color.vector3[boem.final1$Month=="July"] <- "orange"
my.color.vector3[boem.final1$Month=="September"] <- "brown"

unique(boem.final2$Month)
my.color.vector4 <- rep("green", times=nrow(boem.num2))
my.color.vector4[boem.final2$Month=="November"] <- "blue"
my.color.vector4[boem.final2$Month=="March"] <- "green"
my.color.vector4[boem.final2$Month=="May"] <- "red"
my.color.vector4[boem.final2$Month=="July"] <- "orange"


?adonis
#Seasonal colors 
unique(boem.final1$Season)
my.color.vector5 <- rep("green", times=nrow(boem.num1))
my.color.vector5[boem.final1$Season=="Winter"] <- "blue"
my.color.vector5[boem.final1$Season=="Spring"] <- "green"
my.color.vector5[boem.final1$Season=="Fall"] <- "brown"
my.color.vector5[boem.final1$Season=="Summer"] <- "orange"

unique(boem.final2$Season)
my.color.vector6 <- rep("green", times=nrow(boem.num2))
my.color.vector6[boem.final2$Season=="Winter"] <- "blue"
my.color.vector6[boem.final2$Season=="Spring"] <- "green"
my.color.vector6[boem.final2$Season=="Fall"] <- "brown"
my.color.vector6[boem.final2$Season=="Summer"] <- "orange"

#Int vs Bottom colors
my.color.vector7 <- rep("green", times=nrow(boem.num1))
my.color.vector7[boem.final1$Depth=="Bottom"] <- "blue"
my.color.vector7[boem.final1$Depth=="Integrated"] <- "green"

my.color.vector8 <- rep("green", times=nrow(boem.num2))
my.color.vector8[boem.final2$Depth=="Bottom"] <- "blue"
my.color.vector8[boem.final2$Depth=="Integrated"] <- "green"


#Time
my.color.vector9<- rep("green", times=nrow(boem.num1))
my.color.vector9[boem.final1$Time=="Night"] <- "blue"
my.color.vector9[boem.final1$Time=="Day"] <- "green"


my.color.vector10<- rep("green", times=nrow(boem.num2))
my.color.vector10[boem.final2$Time=="Night"] <- "blue"
my.color.vector10[boem.final2$Time=="Day"] <- "green"

#Z-COLOR kmeans 
my.color.vector.z1 <- rep("green", times=nrow(boem.num1))
my.color.vector.z1[kmeans.z1$cluster==1] <- "blue"
my.color.vector.z1[kmeans.z1$cluster==2] <- "green"
my.color.vector.z1[kmeans.z1$cluster==3] <- "red"
#my.color.vector2[log.boem.kop$cluster==4] <- "orange"
#my.color.vector2[log.boem.kop$cluster==5] <- "brown"

my.color.vector.z2 <- rep("green", times=nrow(boem.num1))
my.color.vector.z2[kmeans.z2$cluster==1] <- "blue"
my.color.vector.z2[kmeans.z2$cluster==2] <- "green"
my.color.vector.z2[kmeans.z2$cluster==3] <- "red"

#______________________________________________________________________________#
#-----------------------------NMDS---------------------------------------------#
#______________________________________________________________________________#
#perform the NMDS, k=2 because we want 2 axes = common
#nmds.boem = metaMDS(dist.boem,k=2, trace=T)
log.nmds.boem1 = metaMDS(log.dist.boem1, k=2, trace = T)
log.nmds.boem2 = metaMDS(log.dist.boem2, k=2, trace = T)

log.nmds.boem1
log.nmds.boem2

#plot the stress plots
stressplot(nmds.boem)
stressplot(log.nmds.boem2)




#-PLOT FOR UNTRANSFORMED DATA - K-means grouping-
#ordiplot(nmds.boem,type="n",xlim=c(-0.2,0.2),ylim=c(-0.5,0.5))
#orditorp(nmds.boem, display="sites",col=my.color.vector,air=0.10,cex=1)
#legend(-.55,.5, c("Historical","Current"), cex=0.8, 
      # col=c("green","blue"), pch=15:15)

#PLOT SEASONS
ordiplot(nmds.boem,type="n",xlim=c(-1,1),ylim=c(-1,1))
orditorp(nmds.boem, display="sites",col=my.color.vector4, air=0.10,cex=1)


#PLOT FOR TRANSFORMED (LOG) DATA - k-means groups
ordiplot(log.nmds.boem1,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3), main = "K-means clusters")
orditorp(log.nmds.boem1, display="sites",col=my.color.vector2,air=0.10,cex=1)

ordiplot(log.nmds.boem2,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3), main = "K-means clusters")
orditorp(log.nmds.boem2, display="sites",col=my.color.vector2,air=0.10,cex=1)

#kmeans on Ztransformed data
ordiplot(log.nmds.boem1,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3), main = "K-means clusters")
orditorp(log.nmds.boem1, display="sites",col=my.color.vector.z1,air=0.10,cex=1)

ordiplot(log.nmds.boem2,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3), main = "K-means clusters")
orditorp(log.nmds.boem2, display="sites",col=my.color.vector.z2,air=0.10,cex=1)


#PLOT SEASONS TRANSFORMED
ordiplot(log.nmds.boem1,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3), main ="Seasonal Plot Clusters")
orditorp(log.nmds.boem1, display="sites",col=my.color.vector4, air=0.10,cex=1)
legend(-0.65,0.3, c("Fall","Winter", "Spring", "Summer"), cex=0.5, 
       col=c("brown","blue","green","orange"), pch=10:10)

ordiplot(log.nmds.boem2,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3), main ="Seasonal Plot Clusters")
orditorp(log.nmds.boem2, display="sites",col=my.color.vector4, air=0.10,cex=1)
legend(-0.65,0.3, c("Fall","Winter", "Spring", "Summer"), cex=0.5, 
       col=c("brown","blue","green","orange"), pch=10:10)


#PLOT BOT VS INT
ordiplot(log.nmds.boem1,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3))
orditorp(log.nmds.boem1, display="sites",col=my.color.vector5, air=0.10,cex=1)

ordiplot(log.nmds.boem2,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3))
orditorp(log.nmds.boem2, display="sites",col=my.color.vector5, air=0.10,cex=1)


#TIME OF DAY
ordiplot(log.nmds.boem1,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3))
orditorp(log.nmds.boem1, display="sites",col=my.color.vector6, air=0.10,cex=1)

ordiplot(log.nmds.boem2,type="n",xlim=c(-0.2,0.2),ylim=c(-0.3,0.3))
orditorp(log.nmds.boem2, display="sites",col=my.color.vector6, air=0.10,cex=1)

# --------------------TESTING FOR GROUPS--------------------------------------$
#-----------------------------------------------------------------------------$
groups = boem.kop$cluster
log.groups = log.boem.kop1$cluster
log.groups



#PerMANOVA
perm.boem<-adonis( boem.num ~ groups, permutations=1000)
log.perm.boem = adonis(log.boem.num1 ~ log.groups, permutations = 1000)
perm.boem
#The one to use
log.perm.boem
