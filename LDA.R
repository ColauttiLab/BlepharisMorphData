# SUMMARY:
## Al-Yotm distinct from Dead Sea and Kufranjah valley sites
## Dead Sea and Kufranjah are different ON AVERAGE, but a lot of overlap

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


# NOTES:
## Plotting PC results using ggplot2
## https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

library(ggplot2)
library(ggfortify)
library(MASS)

## Import Data
MorphData<-read.csv("MuhaidatEtAl_MorphData.csv",header=T)
str(MorphData)

## Add midvalue for missing data
for(Row in 1:nrow(MorphData)){
  for(Col in 2:ncol(MorphData)){
    if(is.na(MorphData[Row,Col])){
      MorphData[Row,Col]<-mean(MorphData[MorphData$Loc==MorphData$Loc[Row],Col],na.rm=T)
    }
  }
}

## Inspect pairwise scatterplots
pairs(MorphData[,3:ncol(MorphData)],col=rgb(0,0,0,0.3),pch=16)

## Scale data to mean and sd prior to analysis
scale<-function(x){
  return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}
MorphScaled<-data.frame(sapply(MorphData[,3:ncol(MorphData)],scale))
MorphScaled$Loc<-as.factor(MorphData$Loc)

# Linear discriminant function analysis
(BlephLDA<-lda(Loc ~ ., data=MorphScaled))

## Extract scaling vectors
scalvec<-data.frame(BlephLDA$scaling)

## Extract predictions
BlephLDAval <- data.frame(predict(BlephLDA)$x)
ldahist(data = BlephLDAval[,1], g=MorphScaled$Loc)

BlephLDAval$Loc<-MorphScaled$Loc

## Plot results
p<-ggplot(data=BlephLDAval,aes(x=LD1,y=LD2,group=Loc))+
  stat_ellipse(geom="polygon",aes(colour=Loc),fill=NA,size=1.2,alpha=0.3)+
  stat_ellipse(geom="polygon",aes(fill=Loc,colour=Loc),size=1.2,alpha=0.3)+
  geom_point(aes(shape=Loc,fill=Loc,colour=Loc),size=I(4),alpha=I(0.8))+
  xlab("LDA1")+ylab("LDA2")+theme_simple() 
  
print(p)

## Quickplot to see legend
qplot(LD1,LD2,data=BlephLDAval)

pdf("LDAplot.pdf",width=6,height=6)
  print(p)
dev.off()

## Create Table of LDA scales + significance from lm model
### LDA scales
BlephLDAscales<-data.frame(round(BlephLDA$scaling,3))
BlephLDAscales$desc<-c("Bract Length","Bract Width","Veins per bract","Lateral spines","Longest spine length","Internode length",
                       "Leaf length","Leaf width","Teeth per leaf","Filament length","Anther length","Appendage length",
                       "Filament length","Anther length")
### Significance testing
BlephLDAscales$F<-NA
BlephLDAscales$P<-NA
names(MorphScaled)
for(col in 1:(ncol(MorphScaled)-1)){
  BlephLDAscales$F[grep(names(MorphScaled)[col],row.names(BlephLDAscales))]<-anova(lm(MorphScaled[,col]~MorphScaled$Loc))$F[1]
  BlephLDAscales$P[grep(names(MorphScaled)[col],row.names(BlephLDAscales))]<-anova(lm(MorphScaled[,col]~MorphScaled$Loc))$P[1]
}
BlephLDAscales$F<-round(BlephLDAscales$F,2)
BlephLDAscales$P<-round(BlephLDAscales$P,3)

### Output table to csv
write.csv(BlephLDAscales,"LDAscales.csv",row.names=T)

## Output data with LDA scores
FullData<-cbind(MorphData,BlephLDAval[,c("LD1","LD2")])
str(FullData)
write.csv(FullData,"LDAData.csv",row.names=F)

## Test significance of LD Axes
anova(lm(BlephLDAval$LD1~MorphScaled$Loc))
anova(lm(BlephLDAval$LD2~MorphScaled$Loc))

## Plot trait vectors
BlephLDAscales$Loc<-NA
s<-2 # scale for vector
k<-0.4 # Keep this proportion of largest vectors

keep<-abs(sqrt(BlephLDAscales$LD1^2+BlephLDAscales$LD2^2))>=sort(abs(c(sqrt(BlephLDAscales$LD1^2+BlephLDAscales$LD2^2))),decreasing=T)[floor(nrow(BlephLDAscales)*k)] 
p+geom_segment(data=BlephLDAscales[keep,],aes(x=0,xend=LD1*s,y=0,yend=LD2*s),
                 arrow = arrow(length = unit(0.5, "cm")),colour="grey",inherit_aes=FALSE)+
    geom_text(data=BlephLDAscales[keep,],aes(x=LD1*s,y=LD2*s,label=desc),size=5)#+coord_fixed() + geom_text(data=BlephLDAval,aes(x=LD1,y=LD2,label=Loc))

## Software version info
sessionInfo()

