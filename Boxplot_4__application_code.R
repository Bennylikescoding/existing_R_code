###-----code for Multi-BOXPLOT---###

# Input format:
##*genetype*	Level3_Description
##CocageWT	7m
##CocageWT	9m
##CocageWT	11m
##CocageWT	12m
##CocageWT	16m
##CocageWT	17m
##Tg	7m
##Tg	9m
##Tg	11m
##Tg	12m
##Tg	16m
##Tg	17m
##WT	7m
##WT	9m
##WT	11m
##WT	12m
##WT	17m

# 1.import library
library(ggplot2)
library(ggsignif)
library(dplyr)
library(grid)
library(gridExtra)

# 2.import files
#瀛欒€佸笀鍘熷鑲犲厤鐤粏鑳?114-total.csv
file_path<-choose.files()
bxplot_df<-read.csv(file_path)

# 3.set variables
n_col_value = 3

grid_graph_title = "KEGG L3 pathways"

# 4.start ploting
colname <- colnames(bxplot_df[c(n_col_value:ncol(bxplot_df))])

out <- NULL

#c(seq_along(colname))
for (i in seq_along(colname)){
  
  #http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels
  p<-ggplot(bxplot_df,aes_string(x="Level3_Description", y=colname[i], 
                                  group="Level3_Description"))+
    geom_boxplot()+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=6, angle=0),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=6, angle=0),
          axis.title.y = element_text(size=7))+
    scale_x_discrete(name ="Months", 
                       limits=c("7m","9m","11m","12m","16m","17m"))+
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
grid.arrange(out[[19]], out[[20]], out[[21]],
             ncol = 1, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

###-----END OF BOXPLOT---###
for (i in c(78)){
  print(i)
  grid.arrange(out[[i]],out[[i+1]],out[[i+2]],
               ncol = 1, nrow = 3,
               top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))
}

out[[223]]

dev.off()
