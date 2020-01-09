#install.packages("RColorBrewer")
#install.packages("colorspace")
library(RColorBrewer)
library(colorspace)

#coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
#my_palette <- colorRampPalette(c("green", "black", "red"))(n = 1000)

#main_title="genus_harmful WT cocaged_wt Tg"
main_title="NR_genus_Tg_average"
#main_title="new brain cytokines 9M_WT and 9M_Tg 180611"
file_directory<-choose.files(caption = "Select heatmap csv source file")
## choose color:
##http://research.stowers.org/mcm/efg/R/Color/Chart/
##https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

#file format:(g__77likevirus is not allowed, must add "n": g__n77likevirus)
#metabiolite(neg)	a10_2	a11_2	a12_2	a91_2
#Arg Lys His	44960.9211	66698.04785	49138.47991	92558.87864
#PUBCHEM_23667298	1611.92127	3942.338158	2013.704038	4847.86141
#Bixin	4525.583406	3889.037257	3131.577794	3700.95596
#Sphingosine	4067.387465	4606.95321	1915.315037	4995.459866


#install.packages("gtools")
#install.packages("gdata")
#install.packages("gplots")
##ploting:
library(gplots)
#set ColSideColors:
#df1_ColSideColors=rep(c("1","2"),c(5,6))
#df1_ColSideColors=rep(c("1","2"),c(9,6))
#df1_ColSideColors=rep(c("1","2","3","4","10","6"),c(5,4,7,10,3,4))
#df1_ColSideColors=rep(c("3","1","2","4"),c(1,1,1,1))
df1_ColSideColors=rep(c("1","2","3","4","5","6"),c(1,1,1,1,1,1))
#df2_ColSideColors=rep(c("plum","plum1","plum2","plum3","plum4","orange","orange3","orange4","orangered","orangered4"),c(5,4,7,5,8,5,4,7,5,6))
#df3_ColSideColors=rep(c("plum","plum1","plum2","plum3","plum4"),c(5,4,7,5,8))
#df4_ColSideColors=rep(c("plum4","orangered4"),c(8,6))

#load raw data file:
df1<-read.csv(file_directory,row.names=1)
#df3<-df2[,c(1:29)]
#df4<-df2[,c(22:29,51:56)]


distCor <- function(x) as.dist(1-cor(t(x)))
hclustAvg <- function(x) hclust(x, method="average")

#draw heatmap:
#df1.ht<-heatmap.2(as.matrix(df1[c(1:21,28:31)]),col=bluered(100),scale="row",trace="none",
#                  cexCol=0.9,cexRow=0.1,dendrogram="row",Colv=FALSE,main=main_title, 
#                  ColSideColors = df1_ColSideColors, margins=c(5,15),
#                  distfun = distCor,hclustfun = hclustAvg)

#set margins:
#par(oma=c(3,0,0,0))
#par(mar=c(0,4,4,20))
#https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet/

#if font is too large, change font to "cexrow=0.1,cexcol=0.9"
#color 1:
#margins=c(height,)
df1.ht<-heatmap.2(as.matrix(df1),col=bluered(100),trace="none",scale="row",symkey=FALSE,
                  cexCol=1,cexRow=0.1,dendrogram="row",Colv=FALSE,Rowv=TRUE,main=main_title, 
                  ColSideColors = df1_ColSideColors, margins=c(1,20),
                  distfun = distCor,hclustfun = hclustAvg)


#df1.ht<-heatmap.2(as.matrix(df1),col=bluered(100),scale="row",trace="none",cexCol=0.8,cexRow=0.8,dendrogram="none",Colv=FALSE,Rowv = FALSE,
#                  main=main_title, margins=c(5,15),distfun = distCor,hclustfun = hclustAvg,
#                  sepwidth=c(0.05,0.05),
#                  sepcolor="white",
#                  colsep=1:ncol(df1),
#                  rowsep=1:nrow(df1)) #ColSideColors = df1_ColSideColors)

#heatmap.2(as.matrix(df2),col=bluered(100),scale="row",trace="none",cexCol=0.9,cexRow=0.8,dendrogram="row",Colv=FALSE,main=main_title, ColSideColors = df2_ColSideColors, margins=c(5,15),distfun = distCor,hclustfun = hclustAvg)
#df3.ht<-heatmap.2(as.matrix(df3),col=bluered(100),scale="row",trace="none",cexCol=0.9,cexRow=0.8,dendrogram="row",Colv=FALSE,main=main_title, ColSideColors = df3_ColSideColors, margins=c(5,15),distfun = distCor,hclustfun = hclustAvg)
#df4.ht<-heatmap.2(as.matrix(df4),col=bluered(100),scale="row",trace="none",cexCol=0.9,cexRow=0.1,dendrogram="row",Colv=FALSE,main=main_title, ColSideColors = df4_ColSideColors, margins=c(5,15),distfun = distCor,hclustfun = hclustAvg)
