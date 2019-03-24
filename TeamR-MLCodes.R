all<-read.csv(file.choose())
str(all$pointdt)

all<-spmergedmain_V3
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
install.packages("ModelMetrics")
install.packages("data.table")
install.packages("gower")
install.packages("ipred")
install.packages("prodlim")
install.packages("lubridate")
library(gridExtra)
library(scales)
install.packages("Rmisc")
library(Rmisc)
library(ggrepel)
library(psych)
library(dplyr)
library(ggplot2)
library(readr)
install.packages("readr")
library(extrafont)
install.packages("Rttf2pt1")
library(ggthemes)
library(caret)
library(cluster) # For preprocessing
library(factoextra) # for elbow plots
library(amap) # For distance metrics
install.packages("amap")
library(stylo)
install.packages("stylo")
install.packages("ape")
install.packages("pamr")
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(MASS)
library(dplyr)

all<-spmergedmain_V3.2

all$pointdt<-as.Date(all$pointdt)
#not working
all$BlackEarnLastDt<- as.Date(all$BlackEarnLastDt)
all$LastDt<-as.Date(all$LastDt)


## Change negative points to zero
all$points[all$points<0] <- 0
plot(all$points)
summary(all$points)
## Change negative PointsTotal to zero

all$PointsTotal[all$PointsTotal<0] <- 0
plot(all$points)


NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)


## Replace NA in TransAmount, using median
all$TransAmount[is.na(all$TransAmount)] <- median(all$TransAmount, na.rm=TRUE)
table(all$TransAmount)
str(all$TransAmount)
sum(is.na(all$TransAmount))

## Location names
names(sort(-table(all$LocationName)))
## Many are city names but cineplex location names
## Replace NA in City to mode
all$City[is.na(all$City)] <- names(sort(-table(all$City)))[1]

## All NA in Province is associated with Toronto, so replace all with Ontario
names(sort(-table(all$Province)))
all$Province[is.na(all$Province)] <- 'ON'
all$Province[all$Province== "-"]<- 'ON'

## LocationName, replace NA with mode
names(sort(-table(all$LocationName)))
all$LocationName[is.na(all$LocationName)] <- names(sort(-table(all$LocationName)))[1]

## LocationType, replace NA with mode (Cineplex)
names(sort(-table(all$LocationType)))
all$LocationType[is.na(all$LocationType)] <- names(sort(-table(all$LocationType)))[1]

## ex_sourceid: not sure what to do here given the data dictionary. I am not clear
str(all$ex_sourceid)

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

## Replace NA of PointsTotal with median

all$PointsTotal[is.na(all$PointsTotal)] <- median(all$PointsTotal, na.rm=TRUE)
table(all$PointsTotal)
str(all$all$PointsTotal)
sum(is.na(all$PointsTotal))

## Replace gender NA with Not Specified(NS). We can also change to Prefer not to answer
as.character(all$gender)
all$gender[is.na(all$gender)] <- names(sort(-table(all$gender)))[3]
str(all$gender)
names(sort(-table(all$gender)))

## Replace NA in age class with mode
all$age_class[is.na(all$age_class)] <- names(sort(-table(all$age_class)))[1]

## Replace NA in merchant name with mode
str(all$merchant_name)
all$merchant_name[is.na(all$merchant_name)] <- names(sort(-table(all$merchant_name)))[1]
table(all$merchant_name)

## Replace NA in ex_transactiondescription with mode

str(all$ex_transactiondescription)
all$ex_transactiondescription[is.na(all$ex_transactiondescription)] <- names(sort(-table(all$ex_transactiondescription)))[1]

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

all$MerchantID <- NULL
all$Partner_Ex_Sourceid <- NULL
all$PartnerID <- NULL
all$isDeleted <- NULL
all$Unique_member_identifier..35 <- NULL
all$Unique_member_identifier..15 <- NULL
all$Unique_member_identifier..42 <- NULL
all$Unique_member_identifier<- NULL
all$X <- NULL
all$X.1 <- NULL
all$BlackEarnLastDt<-NULL
all$LastDt<-NULL

## replace Activity month NA with mode

all$ActivityMonth[is.na(all$ActivityMonth)] <- names(sort(-table(all$ActivityMonth)))[1]
## replace isQuality isMarektable isReachable isSMS NA with mode
all$isQuality[is.na(all$isQuality)] <- names(sort(-table(all$isQuality)))[1]
all$isMarketable[is.na(all$isMarketable)] <- names(sort(-table(all$isMarketable)))[1]
all$isReachable[is.na(all$isReachable)] <- names(sort(-table(all$isReachable)))[1]
all$isSMS[is.na(all$isSMS)] <- names(sort(-table(all$isSMS)))[1]
all$hasActivity[is.na(all$hasActivity)] <- names(sort(-table(all$hasActivity)))[1]

## replace Partner Name  NA with Cineplex
all$PartnerName[is.na(all$PartnerName)] <- 'Cineplex'
summary(all$PartnerName)
all <- all %>%
  mutate(
    PartnerName = recode(
      PartnerName,
      "Carrot Rewards" = 1,
      "N/A" = 2,
      "Sport Chek" = 3,
      "Telus" = 0,
      "Live Entertainment"=4,
      "PRESTO" = 5
    )
  )



str(all)
summary(all$PartnerName)
write.csv(all, "Clean_Scene.csv")


all <- all %>%
  mutate(
    OpensEmail_value = recode(
      OpensEmail_value,
      "HIGH" = 3,
      "MED" = 2,
      "LOW"=1
    ),
    ClicksEmail_value = recode(
      ClicksEmail_value,
      "HIGH" = 3,
      "MED" = 2,
      "LOW" = 1
    ),
    StateProv = recode(
      StateProv,
      "AB" = 1,
      "BC" = 2,
      "MB" = 3,
      "NB" = 4,
      "NL" = 5,
      "NS" = 6,
      "NT" = 7,
      "NU" = 8,
      "ON" = 9,
      "PE" = 10,
      "QC" = 11,
      "SK" = 12
    ),
    gender = recode(
      gender,
      "Male" = 1,
      "Female" = 2,
      "Prefer not to answer" = 3
    ),
    age_class = recode(
      age_class,
      "14-17" = 1,
      "18-24" = 2,
      "25-34" = 3,
      "35-44" = 4,
      "45-54" = 5,
      "55+" = 6
    )
  )
str(all)

all$pointdt<-NULL
all$pointid<-NULL
all$ex_transactiondescription<-NULL
all$ActivityMonth<-NULL
all$ex_transactiondescription<-NULL
all$BlackBurnLastDt<-NULL
all$LoadTime<-NULL
all$ConcessionLastDt<-NULL
all$MusicStoreLastDt<-NULL
all$OrderLastDt<-NULL
all$CnplxOnlineBonusLastDt<-NULL
all$CnplxEarnTuesdayLastDt<-NULL
all$ChildTicketLastDt<-NULL



all$isQuality<-as.logical(all$isQuality)
all$isMarketable<-as.logical(all$isMarketable)
all$isReachable<-as.logical(all$isReachable)
all$isSMS<-as.logical(all$isSMS)
all$hasActivity<-as.logical(all$hasActivity)


all2$OpensEmail_value<-as.numeric(all2$OpensEmail_value)
all2$ClicksEmail_value<-as.numeric(all2$ClicksEmail_value)

summary(all$OpensEmail_value)

all3<-all2

cols <- sapply(all3, is.logical)
all3[,cols] <- lapply(all3[,cols], as.numeric)
head(all2)

str(all)


install.packages("sqldf")
install.packages("RSQLite")
install.packages("bit64")
install.packages("bit")
install.packages("chron")
library(RSQLite)
library(sqldf)

all3<-all2


all3$ex_sourceid[all3$ex_sourceid  == 7777] <- 32
all3$ex_sourceid[all3$ex_sourceid  == 7778] <- 33
all3$ex_sourceid[all3$ex_sourceid  == 7769] <- 34
all3$ex_sourceid[all3$ex_sourceid  == 7766] <- 35
all3$ex_sourceid[all3$ex_sourceid  == 7789] <- 36
all3$ex_sourceid[all3$ex_sourceid  == 7790] <- 37
all3$ex_sourceid[all3$ex_sourceid  == 7791] <- 38
all3$ex_sourceid[all3$ex_sourceid  == 7792] <- 39
all3$ex_sourceid[all3$ex_sourceid  == 7793] <- 40
all3$ex_sourceid[all3$ex_sourceid  == 7794] <- 41
all3$ex_sourceid[all3$ex_sourceid  == 7795] <- 42
all3$ex_sourceid[all3$ex_sourceid  == 7796] <- 43
all3$ex_sourceid[all3$ex_sourceid  == 7797] <- 44
all3$ex_sourceid[all3$ex_sourceid  == 7798] <- 45
all3$ex_sourceid[all3$ex_sourceid  == 7800] <- 46
all3$ex_sourceid[all3$ex_sourceid  == 7801] <- 47
all3$ex_sourceid[all3$ex_sourceid  == 7802] <- 48
all3$ex_sourceid[all3$ex_sourceid  == 7803] <- 49
all3$ex_sourceid[all3$ex_sourceid  == 7804] <- 50
all3$ex_sourceid[all3$ex_sourceid  == 7772] <- 51
all3$ex_sourceid[all3$ex_sourceid  == 7815] <- 52
all3$ex_sourceid[all3$ex_sourceid  == 7799] <- 53
all3$ex_sourceid[all3$ex_sourceid  == 7807] <- 54
all3$ex_sourceid[all3$ex_sourceid  == 1] <- 1
all3$ex_sourceid[all3$ex_sourceid  == 2] <- 2
all3$ex_sourceid[all3$ex_sourceid  == 3] <- 3
all3$ex_sourceid[all3$ex_sourceid  == 4] <- 4
all3$ex_sourceid[all3$ex_sourceid  == 5] <- 5
all3$ex_sourceid[all3$ex_sourceid  == 6] <- 6
all3$ex_sourceid[all3$ex_sourceid  == 7] <- 7
all3$ex_sourceid[all3$ex_sourceid  == 8] <- 8
all3$ex_sourceid[all3$ex_sourceid  == 9] <- 9
all3$ex_sourceid[all3$ex_sourceid  == 10] <- 10
all3$ex_sourceid[all3$ex_sourceid  == 11] <- 11
all3$ex_sourceid[all3$ex_sourceid  == 12] <- 12
all3$ex_sourceid[all3$ex_sourceid  == 13] <- 13
all3$ex_sourceid[all3$ex_sourceid  == 14] <- 14
all3$ex_sourceid[all3$ex_sourceid  == 15] <- 15
all3$ex_sourceid[all3$ex_sourceid  == 16] <- 16
all3$ex_sourceid[all3$ex_sourceid  == 17] <- 17
all3$ex_sourceid[all3$ex_sourceid  == 18] <- 18
all3$ex_sourceid[all3$ex_sourceid  == 19] <- 19
all3$ex_sourceid[all3$ex_sourceid  == 20] <- 20
all3$ex_sourceid[all3$ex_sourceid  == 21] <- 21
all3$ex_sourceid[all3$ex_sourceid  == 22] <- 22
all3$ex_sourceid[all3$ex_sourceid  == 23] <- 23
all3$ex_sourceid[all3$ex_sourceid  == 24] <- 24
all3$ex_sourceid[all3$ex_sourceid  == 25] <- 25
all3$ex_sourceid[all3$ex_sourceid  == 26] <- 26
all3$ex_sourceid[all3$ex_sourceid  == 27] <- 27
all3$ex_sourceid[all3$ex_sourceid  == 28] <- 28
all3$ex_sourceid[all3$ex_sourceid  == 29] <- 29
all3$ex_sourceid[all3$ex_sourceid  == 30] <- 30
all3$ex_sourceid[all3$ex_sourceid  == 31] <- 31

summary(all3$ex_sourceid)

all3$Ex_sourceid_new<-NULL
str(all3)


all3$OpensEmail_value<-as.numeric(all3$OpensEmail_value)
all3$ClicksEmail_value<-as.numeric(all3$ClicksEmail_value)

res.pca<-PCA(all3, graph=FALSE)




fviz_screeplot(res.pca,addlabels=TRUE,ylim=c(0,50))
fviz_pca_var(res.pca,col.var="contrib",
             repel=TRUE)

all3$RedemptionYearToDate <- NULL
all3$BlackBurnCount <- NULL
all3$BlackEarnCount <- NULL
all3$isQuality <- NULL
all3$isMarketable <- NULL
all3$OpensEmail_tendancy <- NULL
all3$isReachable <- NULL
all3$OpensEmail_tendancy <- NULL
all3$isActive <- NULL
all3$hasActivity <- NULL
all3$ClicksEmail_value <- NULL

#motivation variables only

#motivational <- all2[,c("all2$PointMonth","all2$ToD","all2$TransAmount","all2$ex_transactiondescription","all2$count_UMITransactions","all2$OnlineTicketPurchaser_tendancy","all2$OnlineTicketPurchaser_value","all2$TuesdayAttendee_tendancy","all2$TuesdayAttendee_value","all2$ConcessionPurchaser_tendancy","all2$ConcessionPurchaser_value","all2$OpensEmail_tendancy","all2$OpensEmail_value","all2$AttendsWithChild_tendancy","all2$AttendsWithChild_value","all2$ClicksEmail_tendancy","all2$ClicksEmail_value","all2$RedemptionYearToDate","all2$RedemptionProgramToDate","all2Activity$isEmailReachable","all2Activity$isSMS","all2Activity$hasActivity","all2$BlackEarnCount","all2$BlackEarnPointTotal","all2$BlackBurnCount","all2$BlackBurnPointTotal","all2$OrderCount","all2$OrderLastDtSince","all2$OrderPointTotal","all2$ConcessionLastDtSince","all2$BlackActivityDays","all2$CnplxOnlineBonusLastDtSince","all2$CnplxEarnTuesdayLastDtSince","all2$LastDtSince","all2$ChildTicketsLastDtSince","all2$PrintCardFl")]
#motivational <- data.frame(all2$PointMonth,all2$ToD,all2$TransAmount,all2$ex_transactiondescription,all2$count_UMITransactions,all2$OnlineTicketPurchaser_tendancy,all2$OnlineTicketPurchaser_value,all2$TuesdayAttendee_tendancy,all2$TuesdayAttendee_value,all2$ConcessionPurchaser_tendancy,all2$ConcessionPurchaser_value,all2$OpensEmail_tendancy,all2$OpensEmail_value,all2$AttendsWithChild_tendancy,all2$AttendsWithChild_value,all2$ClicksEmail_tendancy,all2$ClicksEmail_value,all2$RedemptionYearToDate,all2$RedemptionProgramToDate,all2$isEmailReachable,all2$isSMS,all2$hasActivity,all2$BlackEarnCount,all2$BlackEarnPointTotal,all2$BlackBurnCount,all2$BlackBurnPointTotal,all2$OrderCount,all2$OrderLastDtSince,all2$OrderPointTotal,all2$ConcessionLastDtSince,all2$BlackActivityDays,all2$CnplxOnlineBonusLastDtSince,all2$CnplxEarnTuesdayLastDtSince,all2$LastDtSince,all2$ChildTicketsLastDtSince,all2$PrintCardFl)
#motivational <- all2[,c(all2$PointMonth,all2$ToD,all2$TransAmount,all2$ex_transactiondescription,all2$count_UMITransactions,all2$OnlineTicketPurchaser_tendancy,all2$OnlineTicketPurchaser_value,all2$TuesdayAttendee_tendancy,all2$TuesdayAttendee_value,all2$ConcessionPurchaser_tendancy,all2$ConcessionPurchaser_value,all2$OpensEmail_tendancy,all2$OpensEmail_value,all2$AttendsWithChild_tendancy,all2$AttendsWithChild_value,all2$ClicksEmail_tendancy,all2$ClicksEmail_value,all2$RedemptionYearToDate,all2$RedemptionProgramToDate,all2$isEmailReachable,all2$isSMS,all2$hasActivity,all2$BlackEarnCount,all2$BlackEarnPointTotal,all2$BlackBurnCount,all2$BlackBurnPointTotal,all2$OrderCount,all2$OrderLastDtSince,all2$OrderPointTotal,all2$ConcessionLastDtSince,all2$BlackActivityDays,all2$CnplxOnlineBonusLastDtSince,all2$CnplxEarnTuesdayLastDtSince,all2$LastDtSince,all2$ChildTicketsLastDtSince,all2$PrintCardFl)]
#str(all2)

#res.pca<-PCA(all2, graph=FALSE)

#fviz_screeplot(res.pca,addlabels=TRUE,ylim=c(0,50))
#fviz_pca_var(res.pca,col.var="contrib",
#             repel=TRUE)
#all2

all3$WeekendMatineeViewer_tendancy <- NULL
all3$AttendsWithChild_tendancy <- NULL
all3$TuesdayAttendee_tendancy <- NULL
all3$WeekdayMatineeViewer_tendancy <- NULL
all3$OnlineTicketPurchaser_tendancy <- NULL
all3$ConcessionPurchaser_tendancy <- NULL

res.pca<-PCA(all3, graph=FALSE)

fviz_screeplot(res.pca,addlabels=TRUE,ylim=c(0,50))
fviz_pca_var(res.pca,col.var="contrib",
             repel=TRUE)
##############################################
all4<-all3
str(all4)

wss <- (nrow(all4)-1)*sum(apply(all4,2,var))

for (i in 1:30) wss[i] <- sum(kmeans(all4, 
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")




#pre = preProcess(all4, method="range")
#all4_scaled = predict(pre, all4, method=c("range"))

set.seed(96743) 
fviz_nbclust(all4_scaled, kmeans, method = "silhouette", k.max=15)
#ggsave(file="out/kmeans_sil.png", width=6, height=4)
fviz_nbclust(all4_scaled, kmeans, method = "wss", k.max=15)
#ggsave(file="out/kmeans_wss.png", width=6, height=4)



set.seed(96743) # set seed to make sure the random assignment starts at the same point
all4clust <- kmeans(all4, centers=5,n=5)

#This begins the K means clustering with 4 centers

# inspect it
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(all4, all4clust$cluster)
summary(all4$ex_sourceid)

summary(all4$PartnerName)


#############additional analysis

all5<-sample_n(all4, 10000)

res.km <- eclust(all5, "kmeans", nstart = 10)

res.km<-eclust(all5, "kmeans", k = 5)

fviz_cluster(res.km)
fviz_silhouette(res.km)
fviz_gap_stat(res.km$gap_stat)
fviz_dist(res.km, lab_size = 8)

library(caret)
install.packages("Rtsne")
library(Rtsne)

tsne_model_1 = Rtsne(as.matrix(all5), check_duplicates=FALSE, pca=TRUE, perplexity=30, theta=0.5, dims=2)

## getting the two dimension matrix
d_tsne_1 = as.data.frame(tsne_model_1$Y)
ggplot(d_tsne_1, aes(x=V1, y=V2)) +
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=6))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE") +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  scale_colour_brewer(palette = "Set2")


## keeping original data
d_tsne_1_original=d_tsne_1

## Creating k-means clustering model, and assigning the result to the data used to create the tsne
fit_cluster_kmeans=kmeans(scale(d_tsne_1), 5)
d_tsne_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)

## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne
#fit_cluster_hierarchical=hclust(dist(scale(d_tsne_1)))

## setting 3 clusters as output
#d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=5))

plot_cluster=function(data, var_cluster, palette)
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=0.25) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_colour_brewer(palette = palette) 
}


plot_k=plot_cluster(d_tsne_1_original, "cl_kmeans", "Accent")
plot_k

res.hc <- eclust(all5, "hclust",)