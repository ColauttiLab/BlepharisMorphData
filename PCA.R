# SUMMARY:
## Al-Yotm distinct from Dead Sea and Kufranjah valley sites
## Dead Sea and Kufranjah are different ON AVERAGE, but a lot of overlap

# NOTES:
## Plotting PC results using ggplot2
## https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html


# ANALYSIS:
## Libraries
library(ggplot2)
library(ggfortify)

## Import Data
MorphData<-read.csv("MuhaidatEtAl_RawData.csv",header=T)
str(MorphData)
## Recode integer as numeric
MorphData$BVeins<-as.numeric(MorphData$BVeins)
MorphData$BSpines<-as.numeric(MorphData$BSpines)
MorphData$LTeeth<-as.numeric(MorphData$LTeeth)

## Add midvalue for missing data
for(Row in 1:nrow(MorphData)){
  for(Col in 2:ncol(MorphData)){
    if(is.na(MorphData[Row,Col])){
      MorphData[Row,Col]<-mean(MorphData[MorphData$Loc==MorphData$Loc[Row],Col],na.rm=T)
    }
  }
}

## Principal Components Analysis
PC<-prcomp(MorphData[,2:ncol(MorphData)],scale=T,center=T)
### Summary
summary(PC)
### ScreePlot
screeplot(PC)
### % Variation explained by PC1 alone
100*sum(summary(PC)[[1]][1])/sum(summary(PC)[[1]])
### % Variation explained by PC1 & PC2
100*sum(summary(PC)[[1]][1:2])/sum(summary(PC)[[1]])
### % Variation explained by first n PCs
n<-10
100*sum(summary(PC)[[1]][1:n])/sum(summary(PC)[[1]])
### Factor Loadings
PC$Rotation

### Plot of PC1 & 2
#### Individuals
autoplot(PC,data=MorphData,colour="Loc",scale=0)+theme_classic()
#### Loadings
autoplot(PC,data=MorphData,colour=NA,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5,scale=0)+theme_classic()

## Linear Model to test ability of PCs to 
MorphPCs<-cbind(MorphData,PC$x)
anova(lm(PC1 ~ Loc,data=MorphPCs)) ## PC1 only significant factor
anova(lm(PC2 ~ Loc,data=MorphPCs))
anova(lm(PC3 ~ Loc,data=MorphPCs))
anova(lm(PC4 ~ Loc,data=MorphPCs))
anova(lm(PC5 ~ Loc,data=MorphPCs))
anova(lm(PC6 ~ Loc,data=MorphPCs))
anova(lm(PC7 ~ Loc,data=MorphPCs))
anova(lm(PC8 ~ Loc,data=MorphPCs))
anova(lm(PC9 ~ Loc,data=MorphPCs))
anova(lm(PC10 ~ Loc,data=MorphPCs))

