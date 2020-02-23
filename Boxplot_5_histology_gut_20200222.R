# 0. data type
# Eartag	Segment	Segment_2	Time	Group	Sex	Edema	Infiltration_inflammatory_cell	Goblet_cell_num	Integrity	Damage
# A4051	colon	a	w31_32	Tg	M	1	1	2	1	1
# A4051	colon	b	w31_32	Tg	M	1	1	3	1	0
# A4051	caecum		w31_32	Tg	M	1	1	2	1	0
# A4051	duodenum	a	w31_32	Tg	M	1	2	1	1	1
# A4051	duodenum	b	w31_32	Tg	M	2	2	1	1	2
# A4051	small_intestine	a	w31_32	Tg	M	1	1	1	1	1
# A4051	small_intestine	b	w31_32	Tg	M	1	2	1	2	2
# A4055	colon	a	w31_32	CocageWT	F	1	1	3	1	0
# A4055	colon	b	w31_32	CocageWT	F	1	2	2	1	1
# A4055	caecum		w31_32	CocageWT	F	1	1	2	2	3
# A4055	duodenum	a	w31_32	CocageWT	F	1	1	1	1	1
# A4055	duodenum	b	w31_32	CocageWT	F	2	2	1	1	2
# A4055	small_intestine	a	w31_32	CocageWT	F	1	2	1	2	2
# A4055	small_intestine	b	w31_32	CocageWT	F	1	1	1	1	1
# A4056	colon	a	w31_32	CocageWT	F	1	1	2	1	0
# A4056	colon	b	w31_32	CocageWT	F	1	1	2	1	1
# A4056	caecum		w31_32	CocageWT	F	1	1	2	1	0
# A4056	duodenum	a	w31_32	CocageWT	F	1	2	1	1	1
# A4056	duodenum	b	w31_32	CocageWT	F	2	2	1	2	3
# A4056	small_intestine	a	w31_32	CocageWT	F	1	1	1	1	1
# A4056	small_intestine	b	w31_32	CocageWT	F	1	1	1	1	1

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
n_col_value = 7

grid_graph_title = "duodenum, M"

# 4.select df
bxplot_df_selected <- filter(bxplot_df, Segment == "duodenum", Sex == "M")

# 5.start ploting
colname <- colnames(bxplot_df[c(n_col_value:ncol(bxplot_df))])

out <- NULL

#c(seq_along(colname))
for (i in seq_along(colname)){
  
  #http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels
  p<-ggplot(bxplot_df_selected,aes_string(x="Time", y=colname[i], 
                                 group="Time"))+
    geom_boxplot()+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+
    stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+
    stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))+
    facet_grid(cols = vars(Group))
  
  out[[i]] <- p
}


##GROUP 1 PLOTING...
grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]], out[[5]],
             ncol = 1, nrow = 5,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

###-----END OF BOXPLOT---###