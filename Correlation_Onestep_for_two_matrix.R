#also see:https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

#uncomment line below if transpose is needed on matrix x:
#note that you need to open the t-ed file and delete manually the "X" before X2m
#x<-t(x)
#default: pearson coefficient

# Input:
## matrix 1:
##group	CD4	CD8	B	Mo	NK
##WT	12.79166667	21	12.40666667	2.758333333	17.7
##Tg	29.1	16.1	14.8	7.2	13.4
##Tg_971	20.4	15.1	15.7	7.3	18.8

## matrix 2:
##group	g__Alistipes	g__Coriobacteriaceae_UCG_002	g__Paraprevotella	g__unclassified_f__Muribaculaceae	g__Ruminococcaceae_UCG_005
##WT	842.6666667	1	0	0	1
##Tg	692.7142857	2	0	0	2
##Tg_971	642.8	3	28.8	4.2	3

# Output:
## 1.Two plots, w/ or w/o significant marks
## 2.Three tables, correlation coefficient matrix, pvalue matrix, pvalue/correlation combined table
## 3.Node ID and edges information
## 4.Correlation circus plot

# Code start:
##Load library:
library(ggcorrplot)

#:::::::::::::::::::::::::::::::::::::::::::::
#set title, label size, x and y directory:
title="S4C"
by="group"
#set font size:
x_label_size=6
y_label_size=6
value_label_size=1

#output file name:
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
matrix_file_name=paste(file_path,"correlation matrix_",title,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
pvalue_file_name=paste(file_path,"pvalue_",title,"_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
#::::::::::::::::::::::::::::::::::::::::::::::

#choose files interactively:
x_directory<-choose.files(caption = "Select immune cells (x)")

#x_directory="C:\\Users\\wangxinyi\\Desktop\\相关性图复现\\下图复现\\031——correlat.csv"
y_directory<-choose.files(caption = "Select bacteria (y)")

#y_directory="C:\\Users\\wangxinyi\\Desktop\\相关性图复现\\下图复现\\45_031__与特定菌群对应的971改变菌群趋势.csv"
#load dataset:
x<-read.csv(x_directory,row.names=1)
y<-read.csv(y_directory,row.names=1)

#calculate correlation:
x_y_correlation<- cor(x,y)

#:::::::WRITE coefficient TO CSV::::::
#write correlation matrix:
write.csv(x_y_correlation,matrix_file_name)
#::::::::::::::::::::::::::::

#::::PLOT::::::::

#plot correlation plot without significance:
#!!please modify the output file name!!
without_sig_corre_plot<-ggcorrplot(x_y_correlation,lab=TRUE,lab_size=value_label_size)+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size)) +
  labs(title=title)
without_sig_corre_plot

#RUN to this line if no pvalue is needed


#merge data:
x_for_merge<-read.csv(x_directory)
y_for_merge<-read.csv(y_directory)
outerjoin<-merge(y_for_merge,x_for_merge,by=by,all = TRUE)
row.names(outerjoin)<-outerjoin[,1]
outerjoin[,1]<-NULL
pvalue.1<-cor_pmat(outerjoin)

##after arrange the chart manually:
#pvalue<-read.csv("D:\\鑲犻亾鑿岀兢\\鑿岀兢澶氭牱鎬х敓淇″垎鏋怽\22_鎵€鏈変簲杞笉鍚屾湀榫勫悎骞?-180522\\Correlation analysis\\genus\\Others\\pvalue.csv",row.names=1)
#pvalue.1<-cor_pmat(pvalue)
#write.csv(pvalue.1,"C:\\Users\\wangxinyi\\Desktop\\temp\\matrix_POS.csv")
#slicing index:
row_upper_limits<-ncol(y)+1
pvalue.2<-pvalue.1[c(row_upper_limits:nrow(pvalue.1)),c(1:ncol(y))]

#plot correlation plot with significance:
#!!please modify the output file name!!
with_sig_corre_plot<-ggcorrplot(x_y_correlation,lab=TRUE,lab_size=value_label_size,p.mat= pvalue.2, insig = "blank")+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size)) +
  labs(title=title)
with_sig_corre_plot

#:::::::WRITE pvalue TO CSV::::::
#write pvalue out:
write.csv(pvalue.2,pvalue_file_name)
#:::::::::::::::::::::::::



# get source and target----
source_target_file_name=paste(file_path,title,"_source_target","_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
nodeID_file_name=paste(file_path,title,"_nodeID","_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")

##read original pvalue files:
a<-pvalue.2

##subtract values where p<0.05:
b<-which(a<0.05,arr.ind=T)

##:::::::::::::::::::::::::
##generate source and target files:
c<-data.frame(row.names(b),names(a[b[,2]]))
colnames(c)<-c("source","target")
c$target<-gsub("\\.[0-9]*$", "", c$target) #remove ".*(numbers)" name
head(c)
write.csv(c,source_target_file_name,row.names=FALSE)

##:::::::::::::::::::::::::
##generate node ID file:
d<-data.frame(node=union(c$source,c$target))
head(d,n=20)
e<-d
#e$node<-gsub(pattern=".",replacement = "0000",e$node,fixed = TRUE)
#e$node<-gsub(pattern="0000.*",replacement = "",e$node)
e$node<-gsub("\\.[0-9]*$", "", e$node)
e<-e$node[!duplicated(e$node)] # delete replicate cells
e<-data.frame(e)
e$nodeID<-seq.int(nrow(e))
write.csv(e,nodeID_file_name,row.names=FALSE)
# end of get source and target---------



##arrange figures:
library(grid)
library(gridExtra)
grid.arrange(without_sig_corre_plot, with_sig_corre_plot,nrow=2,ncol = 1)

# additional code:
