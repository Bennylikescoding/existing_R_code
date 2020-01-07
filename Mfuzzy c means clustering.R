#source("https://bioconductor.org/biocLite.R")
#biocLite("Mfuzz")
#
#data structure: AND put them into a txt file:
#(tab deliminated)
#Species	2m	3m	5m	7m	9m
#s__uncultured_bacterium_g__Ruminiclostridium_5	81.5	24.5	36	38	37.2
#s__uncultured_bacterium_g__Tyzzerella_3	2	6.5	7.142857	19.66667	7.2
#s__uncultured_bacterium_g__Coprococcus_1	18	0	2.714286	3.5	3.2

library(Mfuzz)
library(Biobase)
file_directory<-choose.files(caption = "Select readExpressionSet WITH .TXT format")

#repeat to test parameters:
eset_original <- readExpressionSet(file_directory)
eset<-standardise(eset_original)

#default m=1.25
cl <- mfuzz(eset,c=3,m=1.25)
mfuzz.plot(eset,cl=cl,mfrow=c(3,3))

#output clustered data:
acore(eset,cl=cl,min.acore=0.7)
list<-acore(eset,cl=cl,min.acore=0.7)
cat(capture.output(print(list), file=paste("C:\\Users\\wangxinyi\\Desktop\\temp\\acore_results_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")))
print ("New acore results replaced completed!")
