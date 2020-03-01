# 0. data type
# project	order_No.	group	sample	T	CD4
# AD-116	1	WT	B3527	65	13
# AD-116	1	WT	B3528	75.4	10.1
# AD-116	1	WT	B3529	66.7	12
# AD-116	1	WT	B3530	61.6	8.33
# AD-116	1	WT	B3531	60.9	12.2
# AD-116	1	WT	B3552	60.3	10.4
# AD-116	1	WT	B3553	43.6	0.33
# AD-116	1	WT	B3554	41.8	13.2
# AD-116	1	WT	B3555	56.5	14.2
# AD-116	1	WT	B3556	54.6	10.4
# AD-116	2	Cocage_WT	X13586	74.7	12.4
# AD-116	2	Cocage_WT	B1675	59.5	7.84
# AD-116	2	Cocage_WT	B1683	55.1	11.2
# AD-116	2	Cocage_WT	B1693	63.9	7.4
# AD-116	2	Cocage_WT	B1700	56.2	9.62
# AD-116	2	Cocage_WT	B1908	72.6	10.2
# AD-116	2	Cocage_WT	B1909	78.5	8.77
# AD-116	2	Cocage_WT	B1910	67.5	10.1
# AD-116	2	Cocage_WT	B1975	69.4	9.47
# AD-116	2	Cocage_WT	B4194	78.6	10.3
# AD-116	3	Tg	B1488	61.4	13.8
# AD-116	3	Tg	B1495	53.6	9.38


# 1.import library
library(ggplot2)
library(ggsignif)
library(dplyr)
library(grid)
library(gridExtra)

# 2.import files
file_path<-choose.files()
bxplot_df<-read.csv(file_path)

# 3.set variables
n_col_value = 5

grid_graph_title = "gut immune cells (116)"

# 4.select df
bxplot_df_selected <- filter(bxplot_df, group == "WT" | group == "Tg" | group =="Tg_971_100mpk")

# 5.start ploting
colname <- colnames(bxplot_df[c(n_col_value:ncol(bxplot_df))])

out <- NULL

#c(seq_along(colname))
for (i in seq_along(colname)){
  
  #http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels
  p<-ggplot(bxplot_df_selected,aes_string(x="group", y=colname[i],
                                          group="group"))+
    geom_boxplot()+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+
    stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+
    stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))+
    scale_x_discrete(name ="Groups", 
                   limits=c("WT","Tg","Tg_971_100mpk"))
  
  out[[i]] <- p
}

##GROUP 1 PLOTING...
grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]], out[[5]],
             out[[6]], out[[7]], out[[8]], out[[9]], out[[10]],
             out[[11]], out[[12]], out[[13]], out[[14]], out[[15]],
             out[[16]], out[[17]], out[[18]], out[[19]], out[[20]], out[[21]],
             ncol = 4, nrow = 6,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

###-----END OF BOXPLOT---###