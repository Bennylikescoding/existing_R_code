#import libarary
#install.packages("ggcorrplot")
library(ggcorrplot)

#data type: (the following will plot correlation among WT,Tg,cocageWT)
#"NK" is the sample name

#NK	WT	Tg	cocageWT
#7	0.389	0.374	0.2725
#9	0.4475	0.475555556	0.3675
#11	1.1825	0.6175	1.318
#12	1.353333333	0.833333333	0.77
#16	0.854	0.47	0.27


#-------------------------INPUT-------
#set title and font size:
title="NK"
x_label_size=8
y_label_size=8
value_label_size=3

#col number of unique_sample_name, 1-index. default = 1.
col_usample=1

#output file name:
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
matrix_file_name=paste(file_path,"correlation matrix_",title,".csv",sep="")
pvalue_file_name=paste(file_path,"pvalue_",title,".csv",sep="")

trans_matrix_file_name=paste(file_path,"correlation matrix_",title,"_tf.csv",sep="")
trans_pvalue_file_name=paste(file_path,"pvalue_",title,"_tf.csv",sep="")
combined_pvalue_file_name=paste(file_path,"comb_cp",title,"_tf.csv",sep="")
#------------------------------------



#load and read file
file_directory<-choose.files(caption="choose cor .csv matrix")
cor_df<-read.csv(file_directory,row.names=col_usample)

#start calculate correlation
corr<-cor(cor_df)
p.mat<-cor_pmat(cor_df)


##:::::::WRITE TO CSV
#write original correlation matrix:
write.csv(corr,matrix_file_name)

#write pvalue out:
write.csv(p.mat,pvalue_file_name)

#write transformed correlation matrix and pvalue
##transformed cor matrix
trans_corr<-data.frame(row=rownames(corr)[row(corr)[upper.tri(corr)]], 
           col=colnames(corr)[col(corr)[upper.tri(corr)]], 
           corr=corr[upper.tri(corr)])

write.csv(trans_corr,trans_matrix_file_name)

##transformed p value
trans_p.mat<-data.frame(row=rownames(p.mat)[row(p.mat)[upper.tri(p.mat)]], 
                       col=colnames(p.mat)[col(p.mat)[upper.tri(p.mat)]], 
                       p.mat=p.mat[upper.tri(p.mat)])

write.csv(trans_p.mat,trans_pvalue_file_name)

##transformed cor AND p combined
trans_c<-data.frame(row=rownames(p.mat)[row(p.mat)[upper.tri(p.mat)]], 
                    col=colnames(p.mat)[col(p.mat)[upper.tri(p.mat)]], 
                    p.mat=p.mat[upper.tri(p.mat)],
                    corr=corr[upper.tri(corr)])

write.csv(trans_c,combined_pvalue_file_name)
###:::::::::::

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
##https://stackoverflow.com/questions/28035001/transform-correlation-matrix-into-dataframe-with-records-for-each-row-column-pai
