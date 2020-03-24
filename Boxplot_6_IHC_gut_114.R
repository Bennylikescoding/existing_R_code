# 0. data type
# Time	group	sex	Eartag	APP	ZO1	E-Cadherin	SLC1A5
# w31_32	CocageWT	F	A4055	1.35	0.225	1	1.425
# w31_32	CocageWT	F	A4056	0.9	0.75	0.9	1.425
# w31_32	CocageWT	F	A4060	0.85	0.6	0.6	0.675
# w31_32	CocageWT	M	A4063	0.9	0.6	0.45	1.425
# w31_32	CocageWT	M	A4064	0.9	0.45	0.375	1.9
# w31_32	CocageWT	F	A4067	0.85	0.6	1.6	1.425
# w31_32	CocageWT	M	A4081	2	0.075	0.9	1.425
# w31_32	CocageWT	M	A4091	0.95	0.15	0.6	0.95
# w40_41	CocageWT	M	X13502	0.1125	0.4	1.35	1.425
# w40_41	CocageWT	M	X13503	0.3	0.975	1.7	0.95
# w40_41	CocageWT	M	X13510	0.1875	0.4	2.25	0.95
# w40_41	CocageWT	M	X13511	0.0375	0.15	2	0.95
# w40_41	CocageWT	F	X13559	0.1875	0.075	0.8	0.95
# w40_41	CocageWT	F	X13560	0.3	0.2	1.2	0.95
# w40_41	CocageWT	F	X13561	0.6375	0.55	0.8	0.95
# w40_41	CocageWT	F	X13564	0.4125	0.075	1.6	1.425
# w48_49	CocageWT	F	A2830	0.85	0.525	2.25	1.425



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

grid_graph_title = "gut IHC (114)-CocageWT"

# 4.select df
bxplot_df_selected <- filter(bxplot_df, group == "CocageWT")

# 5.start ploting
colname <- colnames(bxplot_df[c(n_col_value:ncol(bxplot_df))])

out <- NULL

#c(seq_along(colname))
for (i in seq_along(colname)){
  
  #http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels
  p<-ggplot(bxplot_df_selected,aes_string(x="Time", y=colname[i],
                                          group="Time"))+
    geom_bar(stat = "identity")+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+
    stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+
    stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))+
    scale_x_discrete(name ="Time", 
                     limits=c("w31_32","w40_41","w48_49","w50_51"))
  
  out[[i]] <- p
}


ggplot(bxplot_df_selected,aes_string(x="Time", y="APP",
                                     group="Time"))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin="APP"-sd, ymax="APP"+sd), width=.2,
                position=position_dodge(.9))


##GROUP 1 PLOTING...
grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]],
             ncol = 2, nrow = 2,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=15,font=1)))

###-----END OF BOXPLOT---###