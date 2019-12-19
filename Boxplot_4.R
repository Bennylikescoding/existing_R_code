###-----code for Multi-BOXPLOT---###

# 1.import library
library(ggplot2)
library(ggsignif)
library(dplyr)
library(grid)
library(gridExtra)

# 2.import files
#孙老师原始肠免疫细胞114-total.csv
file_path<-choose.files()
bxplot_df<-read.csv(file_path)

# 3.set variables
n_col_value = 5

grid_graph_title = "same cell, different groups-7"

# 4.start ploting
colname <- colnames(bxplot_df[c(n_col_value:ncol(bxplot_df))])

out <- NULL

#c(seq_along(colname))
for (i in seq_along(colname)){
  
  #http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels
  p<-ggplot(bxplot_df,aes_string(x="months_when_sampling", y=colname[i], 
                                  group="months_when_sampling"))+
    geom_boxplot()+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+
    stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+
    stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))+
    facet_grid(cols = vars(genetype))
  
  out[[i]] <- p
}


##GROUP 1 PLOTING...
grid.arrange(out[[1]], out[[2]], out[[3]], 
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

##GROUP 2 PLOTING...
grid.arrange(out[[4]], out[[5]], out[[6]], 
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

##GROUP 3 PLOTING...
grid.arrange(out[[7]], out[[8]], out[[9]], 
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

##GROUP 4 PLOTING...
grid.arrange(out[[10]], out[[11]], out[[12]], 
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

##GROUP 5 PLOTING...
grid.arrange(out[[13]], out[[14]], out[[15]], 
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

##GROUP 6 PLOTING...
grid.arrange(out[[16]], out[[17]], out[[18]], 
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

##GROUP 7 PLOTING...
grid.arrange(out[[19]], out[[20]],
             ncol = 1, nrow = 2,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

###-----END OF BOXPLOT---###

