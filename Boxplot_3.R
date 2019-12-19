library(ggplot2)
library(ggsignif)
library(dplyr)
library(grid)
library(gridExtra)

file_path<-choose.files()
bxplot_df<-read.csv(file_path)

# colnames(bxplot_df[c(5:24)])
# Tg: bxplot_df[c(1:38),]
# WT: bxplot_df[c(39:78),]
# CocageWT: bxplot_df[c(79:105),]

a<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=Mon_Mφ,group=months_when_sampling))+
    geom_boxplot()+
    geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

b<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=Neutrophils,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

c<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=CD4_CD8_,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

d<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=CD8,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

e<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=Effector_CD8,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

f<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=Memory_CD8,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

g<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=Naive_CD8,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

h<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=B,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

i<-ggplot(bxplot_df[c(79:105),],aes(x=months_when_sampling,y=DC,group=months_when_sampling))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.6)

grid.arrange(a,b,
             nrow=1,ncol = 2)

bxplot_df
bxplot_df[,c(-1,-2,-3,-4)]


#####

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


