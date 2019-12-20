#import libarary
#install.packages("ggcorrplot")
library(ggcorrplot)

#data type: (the following will plot correlation between B_cell and CD3T)
#unique_sample_name    B_cell	CD3T
#A	 3.92	3.35
#A.1	3.3	7.41
#A.2	4.32	10.9
#A.3	2.9	26

#-------------------------INPUT-------
#set title and font size:
title="NK"

#col number of unique_sample_name, 1-index. default = 1.
col_usample=1

x_label_size=8
y_label_size=8
value_label_size=1.5

#output file name:
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
matrix_file_name=paste(file_path,"correlation matrix_",title,".csv",sep="")
pvalue_file_name=paste(file_path,"pvalue_",title,".csv",sep="")
#------------------------------------



#load and read file
file_directory<-choose.files(caption="choose cor .csv matrix")
cor_df<-read.csv(file_directory,row.names=col_usample)

#start calculate correlation
corr<-cor(cor_df)
p.mat<-cor_pmat(cor_df)

##WRITE TO CSV
#write correlation matrix:
write.csv(corr,matrix_file_name)

#write pvalue out:
write.csv(p.mat,pvalue_file_name)
###

#plot
cor_plot_with_pvalue<-ggcorrplot(corr,hc.order=FALSE,type="lower",lab=TRUE,p.mat= p.mat, insig = "blank",lab_size=value_label_size)+
  labs(title=title)+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size))

cor_plot_with_pvalue

##plot w/o pvalue:
cor_plot_without_pvalue<-ggcorrplot(corr,hc.order=FALSE,type="lower", lab=TRUE,lab_size=value_label_size)+
  labs(title=title)+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size))

cor_plot_without_pvalue

#arrange figures:
library(grid)
library(gridExtra)
grid.arrange(cor_plot_with_pvalue,cor_plot_without_pvalue,
             nrow=1,ncol = 2)

#end of code

#ref:
##https://rdrr.io/cran/ggcorrplot/man/ggcorrplot.html
##http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
