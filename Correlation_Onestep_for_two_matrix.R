#also see:https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

#uncomment line below if transpose is needed on matrix x:
#note that you need to open the t-ed file and delete manually the "X" before X2m
#x<-t(x)
#default: pearson coefficient
#include library:
library(ggcorrplot)

#:::::::::::::::::::::::::::::::::::::::::::::
#set x and y directory:
title="S4C"
by="group"
#set font size:
x_label_size=6
y_label_size=6
value_label_size=1
#::::::::::::::::::::::::::::::::::::::::::::::

#output file name:
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
matrix_file_name=paste(file_path,"correlation matrix_",title,".csv",sep="")
pvalue_file_name=paste(file_path,"pvalue_",title,".csv",sep="")

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

#:::::::WRITE TO CSV::::::
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
#pvalue<-read.csv("D:\\鑲犻亾鑿岀兢\\鑿岀兢澶氭牱鎬х敓淇″垎鏋怽\22_鎵�鏈変簲杞笉鍚屾湀榫勫悎骞?-180522\\Correlation analysis\\genus\\Others\\pvalue.csv",row.names=1)
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

#:::::::WRITE TO CSV::::::
#write pvalue out:
write.csv(pvalue.2,pvalue_file_name)
#:::::::::::::::::::::::::

##arrange figures:
library(grid)
library(gridExtra)
grid.arrange(without_sig_corre_plot, with_sig_corre_plot,nrow=2,ncol = 1)

# additional code:
