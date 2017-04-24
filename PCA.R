MorphData<-read.csv("MuhaidatEtAl_RawData.csv",header=T)
str(MorphData)

MorphData$BVeins<-as.numeric(MorphData$BVeins)

princomp(MorphData[,2:4])
