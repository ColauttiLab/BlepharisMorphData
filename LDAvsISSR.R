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


## Import and inspect LDA coordinates and ISSR data
LDA<-read.csv("LDAData.csv",header=T,row.names="IndID")
ISSR<-read.csv("MuhaidatEtAl_ISSRData.csv",header=T,row.names="IndID")

str(LDA)
str(ISSR)

## Nonmetric Multidimensional Scaling (NMDS) of ISSR Data
NMDS<-cmdscale(dist(ISSR, method = "Jaccard",upper=F),k=2)
str(NMDS)

#### Merge LDA & NMDS
Coords<-merge(LDA[,c("Loc","LD1","LD2")],NMDS,by="row.names",all=T)
names(Coords)[grep("V[0-9]$",names(Coords))]<-c("NMDS1","NMDS2")
head(Coords)

## Plot NMDS
p<-ggplot(data=Coords,aes(x=NMDS1,y=NMDS2)) +
  stat_ellipse(geom="polygon",aes(colour=Loc),fill=NA,size=1.2,alpha=0.3)+
  stat_ellipse(geom="polygon",aes(fill=Loc,colour=Loc),size=1.2,alpha=0.3)+
  geom_point(aes(colour=Loc),alpha=0.5,size=I(4)) +  
  xlab("NMDS Axis1")+ylab("NMDS Axis2")+theme_simple() 
print(p)

### Output as pdf
pdf("NMDSplot.pdf",width=6,height=6)
  print(p)
dev.off()

## Plot LDA
p2<-ggplot(data=Coords,aes(x=LD1,y=LD2)) +
  stat_ellipse(geom="polygon",aes(colour=Loc),fill=NA,size=1.2,alpha=0.3)+
  stat_ellipse(geom="polygon",aes(fill=Loc,colour=Loc),size=1.2,alpha=0.3)+
  geom_point(aes(colour=Loc),alpha=0.5,size=I(4)) +  
  xlab("LD Axis1")+ylab("LD Axis2")+theme_simple() 
print(p2)

pdf("NMDSvsLDAplot.pdf",width=6,height=6)
  print(p2)
dev.off()


## Calculate Jaccard's distance for molecular and morphological traits
ISSRdist<-melt(as.matrix(dist(ISSR, method = "Jaccard",upper=F)))
Morphdist<-melt(as.matrix(dist(LDA[,grep("LD",names(LDA))],method = "eJaccard",upper=F)))

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
#Dist$Type[Dist$Comp %in% c("A-A","D-D","K-K")]<-"Within"
#Dist$Type[Dist$Comp %in% c("A-D","A-K","D-K")]<-"A-D"

## Plot similarity
qplot(ISSR,Morph,colour=Comp,data=Dist)
qplot(ISSR,Morph,colour=Type,data=Dist) + geom_smooth(method="lm")

anova(lm(Morph~ISSR+I(ISSR^2),data=Dist))

p<-ggplot(data=Dist,aes(x=ISSR,y=Morph)) + geom_smooth(method="lm") +
  geom_point(aes(colour=Type),alpha=0.5,size=3) +  
  xlab("Genetic Distance")+ylab("Morphological Difference")+theme_simple() 
p

## Software version info
sessionInfo()


