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

Tg_subset_df <- bxplot_df[which(bxplot_df$genetype == 'Tg'),]
WT_subset_df <- bxplot_df[which(bxplot_df$genetype == 'WT'),]
cocageWT_subset_df <- bxplot_df[which(bxplot_df$genetype == 'cocageWT'),]

grid_graph_title = "Tg_subset,1-9"

# 4.start ploting
colname <- colnames(bxplot_df[c(n_col_value:ncol(bxplot_df))])

out <- NULL
for (i in seq_along(colname)){
  
  #https://dodata.wordpress.com/2012/10/25/ggplot2-in-loops-and-multiple-plots/
  #print(paste("current immune cells is ", colname[i]))
  p<-ggplot(Tg_subset_df,aes_string(x="months_when_sampling", y=colname[i], 
                                          group="months_when_sampling"))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,color="black")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")+
  stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))
  
  out[[i]] <- p
}

grid.arrange(out[[1]], out[[2]], out[[3]], 
             out[[4]], out[[5]], out[[6]], 
             out[[7]], out[[8]], out[[9]], 
             ncol = 3, nrow = 3,
             top = textGrob(grid_graph_title, gp=gpar(fontsize=20,font=1)))
#dev.off()
#ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)

###-----END OF BOXPLOT---###


###--code for single graph-###
###---for debug###
#dev.off()
#a<-ggplot(cocageWT_subset_df,aes(x=months_when_sampling,y=NKT,group=months_when_sampling))+
#    geom_boxplot()+
#  geom_dotplot(binaxis='y', binwidth = 0.2, stackdir='center',dotsize=0.5)+
#  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")+
#  stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))

#a
###----end------###






bxplot_df
bxplot_df[,c(-1,-2,-3,-4)]


#####---PCA:

library(ggfortify)

#https://www.statmethods.net/management/subset.html
df <- bxplot_df[which((bxplot_df$months_when_sampling == 4) & 
                        (bxplot_df$genetype == "Tg" | bxplot_df$genetype == "WT" | bxplot_df$genetype == "cocageWT")),]
df


autoplot(prcomp(df[,c(-1,-2,-3,-4)]), data = df, 
         colour = 'genetype',shape='genetype',label=FALSE,
         frame = TRUE,
         loadings = TRUE,loadings.label = TRUE, loadings.label.size  = 3) + 
  labs(title="Tg_WT_cocageWT, month 4") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))


