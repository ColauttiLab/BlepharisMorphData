# Simplified/clean theme for plotting
theme_simple <- function (base_size = 12, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size=18),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=18,angle=90),
      axis.text.y = element_text(size=12),
      axis.ticks = element_blank(), 
      panel.background = element_rect(fill="white"),
      panel.border = element_blank(),
      plot.title=element_text(face="bold", size=24),
      legend.position="none"
    )   
}

# Libraries
library(ggplot2) # For plotting
library(proxy) # For similarity matrix calculations using Jaccard's coefficient
library(reshape) # For reformatting similarity matrices


## Import and inspect PCA coordinates and ISSR data
PCA<-read.csv("PCAdata.csv",header=T,row.names="IndID")
ISSR<-read.csv("MuhaidatEtAl_ISSRData.csv",header=T,row.names="IndID")

str(PCA)
str(ISSR)

## Calculate Jaccard's distance for molecular and morphological traits
ISSRdist<-melt(as.matrix(dist(ISSR, method = "Jaccard",upper=F)))
Morphdist<-melt(as.matrix(dist(PCA[,grep("PC",names(PCA))],method = "eJaccard",upper=F)))

### Merge distance matrices
Dist<-merge(ISSRdist,Morphdist,by=c("X1","X2"))
names(Dist)<-c("ID1","ID2","ISSR","Morph")
#### Remove duplicates
for (row in 1:nrow(Dist)){
  Dist[row,grep("ID",names(Dist))]<-sort(Dist[row,grep("ID",names(Dist))])
}
Dist<-Dist[!duplicated(Dist),]
Dist<-Dist[Dist$ID1!=Dist$ID2,]

### Categorize comparisons for colour coding
Dist$Comp<-paste0(gsub("[0-9]*","",Dist$ID1),"-",gsub("[0-9]*","",Dist$ID2))
Dist$Type<-Dist$Comp
Dist$Type[Dist$Comp %in% c("A-A","D-D","K-K")]<-"Within"
#Dist$Type[Dist$Comp %in% c("A-D","A-K","D-K")]<-"Between"

## Plot similarity
qplot(ISSR,log(Morph),colour=Comp,data=Dist)
qplot(ISSR,Morph,colour=Type,data=Dist) + geom_smooth()

anova(lm(Morph~ISSR+I(ISSR^2),data=Dist))

p<-ggplot(data=Dist,aes(x=ISSR,y=Morph)) + geom_smooth(method="lm") +
  geom_point(aes(colour=Type)) +  
  xlab("Genetic Distance")+ylab("Morphological Difference")+theme_simple() 
p



