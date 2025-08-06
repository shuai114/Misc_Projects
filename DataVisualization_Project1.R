#season<-read.csv("ExcelFormattedGISTEMPDataCSV.csv",na.strings=c("***","****"))
zone<-read.csv("ExcelFormattedGISTEMPData2CSV.csv")
zone<-zone[,c(1:2,5:7)]
library(reshape2)
mzone<-melt(zone,id.vars='Year',variable.name='Zone',value.name='Deviation')
mzone$Zone<-factor(mzone$Zone,labels=c("Global","24N-90N","24S-24N","90S-24S"))
mzone$Deviation<-mzone$Deviation/100

library(ggplot2)
#sources:  GHCN-v3 1880-06/2015 + SST: 1880-06/2015 ERSST v4
#using elimination of outliers and homogeneity adjustment
#from the base period: 1951-1980
gggistemp <- ggplot(mzone, aes(Year, Deviation, group=Zone)) +
    theme_bw(base_size = 18) +
    geom_point(aes(color=Zone), size=2) +
    geom_line(aes(color=Zone), size=0.5) +
    scale_colour_manual(values=c("black", "green", "red", "blue")) +
    labs(y="Deviation (degrees Celsius)") +
    ggtitle("Annual Mean Land-Ocean Temperature Deviations for the Globe and Different Latitude Zones")
gggistemp

library(plotly)
py<-plotly()
r<-py$ggplotly(gggistemp)
