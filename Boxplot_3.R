library(ggplot2)
library(ggsignif)
library(dplyr)
library(grid)
library(gridExtra)

file_path<-choose.files()
bxplot_df<-read.csv(file_path)

# colname <- colnames(bxplot_df[c(5:ncol(bxplot_df))])
# Tg: bxplot_df[c(1:38),]
Tg_subset_df <- bxplot_df[which(bxplot_df$genetype == 'Tg'),]
# WT: bxplot_df[c(39:78),]
WT_subset_df <- bxplot_df[which(bxplot_df$genetype == 'WT'),]
# CocageWT: bxplot_df[c(79:105),]
cocageWT_subset_df <- bxplot_df[which(bxplot_df$genetype == 'cocageWT'),]

graph_list <- list()
graph_list<-append(graph_list,"3")

###--code for single graph-###
a<-ggplot(cocageWT_subset_df,aes(x=months_when_sampling,y=Mon_Mφ,group=months_when_sampling))+
    geom_boxplot()+
  geom_dotplot(binaxis='y', binwidth = 0.2, stackdir='center',dotsize=0.4,color="black")+
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")+
  stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))

a
###----end------###


###-----code for Multi-BOXPLOT---###

out <- NULL

for (i in seq_along(colname)){
  
  #https://dodata.wordpress.com/2012/10/25/ggplot2-in-loops-and-multiple-plots/
  #print(paste("current immune cells is ", colname[i]))
    p<-ggplot(cocageWT_subset_df,aes_string(x="months_when_sampling", y=colname[i], group="months_when_sampling"))+
    geom_boxplot()+
    geom_dotplot(binaxis='y', binwidth = 0.2, stackdir='center',dotsize=0.4,color="black")+
    stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")+
    stat_summary(fun.y=mean, geom="line", size=1, color="red", aes(group=1))
  
  out[[i]] <- p
}

grid.arrange(out[[1]], out[[2]], out[[3]], 
             out[[4]], out[[5]], out[[6]], 
             out[[7]], out[[8]], out[[9]], 
             ncol = 3, nrow = 3,
             top = textGrob("cocageWT_subset,1-9",gp=gpar(fontsize=20,font=1)))

#ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)

###-----END OF BOXPLOT---###

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


