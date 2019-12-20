##https://rdrr.io/cran/ggcorrplot/man/ggcorrplot.html
##http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
#install.packages("ggcorrplot")
#if not operated well, re-install the packages

library(ggcorrplot)
file_directory<-choose.files(caption="choose matrix")

#data type: (the following will plot correlation between B_cell and CD3T)
#    B_cell	CD3T
#A	 3.92	3.35
#A.1	3.3	7.41
#A.2	4.32	10.9
#A.3	2.9	26

#set title and font size:
title="Tg__different_cells__11_12m_cor"
x_label_size=8
y_label_size=8
value_label_size=1.5
#::::::::::::::::::::::::::::::::::::::::::::::

#output file name:
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
matrix_file_name=paste(file_path,"correlation matrix_",title,".csv",sep="")
pvalue_file_name=paste(file_path,"pvalue_",title,".csv",sep="")


#start calculation:
test_bac<-read.csv(file_directory,row.names=4)

##----1
#start_row = 1
#test_bac<-test_bac[c(start_row:(start_row + 4)),]

#rownames(test_bac)<-test_bac[,1]
#test_bac<-test_bac[,c(2:4)]

#colnames(test_bac) <- c("WT","Tg","cocageWT")
##---end of 1

##2
df_pca <- bxplot_df[which((test_bac$months_when_sampling == 11 | test_bac$months_when_sampling == 12) & 
                        (test_bac$genetype == "Tg" )),]
df_new<-df_pca[,c(5:ncol(df_pca))]
##---end of 2

corr<-cor(df_new)
p.mat<-cor_pmat(df_new)
#simple plot:
#simple_plot<-ggcorrplot(corr,hc.order=TRUE,type="lower",lab=TRUE)+labs(title=title)
#simple_plot

#change to circle plot:
#circle_plot<-ggcorrplot(corr, method = "circle",type="lower",hc.order=TRUE)
#circle_plot
#plot with pvalue:

#:::::::WRITE TO CSV::::::
#write correlation matrix:
write.csv(corr,matrix_file_name)

#write pvalue out:
write.csv(p.mat,pvalue_file_name)
#:::::::::::::::::::::::::


with_pvalue<-ggcorrplot(corr,hc.order=TRUE,type="lower",lab=TRUE,p.mat= p.mat, insig = "blank",lab_size=value_label_size)+labs(title=title)+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size))
with_pvalue

#plot w/o pvalue:

without_pvalue<-ggcorrplot(corr,hc.order=FALSE,type="lower", lab=TRUE,lab_size=value_label_size)+labs(title=title)+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size))
without_pvalue

#dev.off()
#part 2:
#test_bac2<-read.csv(file_directory,row.names=1)[c("2m","3m","5m","7m"),]
#title2="2m 3m 5m 7m"
#corr2<-cor(test_bac2)
#p.mat2<-cor_pmat(test_bac2)

#with_pvalue2<-ggcorrplot(corr2,hc.order=TRUE,type="lower",lab=TRUE,p.mat= p.mat2, insig = "blank",lab_size=value_label_size)+labs(title=title2)+
#  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size))
#with_pvalue2

#without_pvalue2<-ggcorrplot(corr2,hc.order=TRUE,type="lower",lab=TRUE,lab_size=value_label_size)+labs(title=title2)+
#  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size))
#without_pvalue2


#: Geng laoshi:significance level, use yellow stars to identify significant correlations

##arrange figures:
library(grid)
library(gridExtra)
grid.arrange(with_pvalue,without_pvalue,nrow=1,ncol = 2)

#end of code
