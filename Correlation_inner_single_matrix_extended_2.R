#import libarary
#install.packages("ggcorrplot")
if (!require(reshape2)){
  install.packages('reshape2')
  library(reshape2)
}

library(ggcorrplot)
library(grid)
library(gridExtra)

#data type:

#*don't change*
#*month*	*group*	NK	NKT
#7	cocageWT	0.2725	0.63625
#9	cocageWT	0.3675	0.58125
#11	cocageWT	1.318	1.604
#12	cocageWT	0.77	1.215
#16	cocageWT	0.27	0.371
#7	Tg	0.374	0.562
#9	Tg	0.475555556	0.598888889
#11	Tg	0.6175	0.945
#12	Tg	0.833333333	0.85
#16	Tg	0.47	0.52
#7	WT	0.389	0.627
#9	WT	0.4475	0.91375
#11	WT	1.1825	1.9275
#12	WT	1.353333333	3.025
#16	WT	0.854	1.304

#-------------------------INPUT-------
x_label_size=8
y_label_size=8
value_label_size=3
#------------------------------------



#load and read file
file_directory<-choose.files(caption="choose aggregated .csv matrix")
cor_df<-read.csv(file_directory)

#loop through colnames of different cells
out <- NULL
colname<-colnames(cor_df)[3:ncol(cor_df)]

for (i in seq_along(colname)){
  
  title=colname[i]
  
  # convert long format to wide format
  cell = dcast(cor_df,month~group,value.var = colname[i])
  selected_cell<-cell[,c(2:ncol(cell))]
  
  #ref
  #https://www.listendata.com/2016/01/transpose-data-in-r.html
  
  #start calculate correlation
  corr<-cor(selected_cell)
  p.mat<-cor_pmat(selected_cell)
  
  ##WRITE TO CSV
  #output file name:
  file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
  matrix_file_name=paste(file_path,"correlation matrix_",title,".csv",sep="")
  pvalue_file_name=paste(file_path,"pvalue_",title,".csv",sep="")
  
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
  

  
  out[[i]] <- grid.arrange(cor_plot_with_pvalue,cor_plot_without_pvalue,
                           nrow=1,ncol = 2)
  
}

##GROUP 1 PLOTING...
tp<-grid.arrange(out[[1]], out[[2]], out[[3]], 
             ncol = 1, nrow = 3,
             top = textGrob("correlation", gp=gpar(fontsize=15,font=1)))

#ggsave(plot = tp, "myuplot.pdf",path="C:\\Users\\wangxinyi\\Desktop\\temp\\")
#dev.off()

##BATCH PLOTING...
for (i in seq(1,19,by=3)){
  output_p<-grid.arrange(out[[i]], out[[i+1]], out[[i+2]], 
               ncol = 1, nrow = 3,
               top = textGrob("correlation", gp=gpar(fontsize=15,font=1)))
  
  ggsave(filename = paste("group ",i,".pdf"), 
         plot = output_p, 
         path="C:\\Users\\wangxinyi\\Desktop\\temp\\")
  
  ggsave(filename = paste("group ",i,".png"), 
         plot = output_p, 
         path="C:\\Users\\wangxinyi\\Desktop\\temp\\")
  
  dev.off()
}


#last 2 plots:
last_p<-grid.arrange(out[[19]], out[[20]], 
                 ncol = 1, nrow = 2,
                 top = textGrob("correlation", gp=gpar(fontsize=15,font=1)))

ggsave(plot = last_p, 
       filename = paste("group  19.pdf"),path="C:\\Users\\wangxinyi\\Desktop\\temp\\")

ggsave(plot = last_p, 
       filename = paste("group  19.png"),path="C:\\Users\\wangxinyi\\Desktop\\temp\\")


dev.off()

print("print completed!")
#end of code

#ref:
##https://rdrr.io/cran/ggcorrplot/man/ggcorrplot.html
##http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2
