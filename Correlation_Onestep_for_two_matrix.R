# last update stamp: 20191224-1353, wang xinyi

# Instructions
## uncomment line below if transpose is needed on matrix x:
## note that you need to open the t-ed file and delete manually the "X" before X2m
## x<-t(x)
## default: pearson coefficient

# Input:
## matrix 1:
## *group*	CD4	CD8	B	Mo	NK
## WT	12.79166667	21	12.40666667	2.758333333	17.7
## Tg	29.1	16.1	14.8	7.2	13.4
## Tg_971	20.4	15.1	15.7	7.3	18.8

## matrix 2:
## *group*	g__Alistipes	g__Coriobacteriaceae_UCG_002	g__Paraprevotella	g__unclassified_f__Muribaculaceae	g__Ruminococcaceae_UCG_005
## WT	842.6666667	1	0	0	1
## Tg	692.7142857	2	0	0	2
## Tg_971	642.8	3	28.8	4.2	3

# Output (9 files):
## 1.Two plots, w/ or w/o significant marks.(in 1 file)
## 2.One Correlation circus plot. (1 file)
## 3.Two tables, correlation coefficient matrix, pvalue matrix.(2 tables)
## 4.Two tables of Node ID and edges information. (2 tables)
## 5.Three tables of Transformed pvalue, coefficient, pvalue/coefficient combined tables. (3 tables)

# Code start:
##Load library:
library(ggcorrplot)

#:::::::::::::::::::::::::::::::::::::::::::::
#set title, label size, x and y directory:
title="S4C"
by="group"
#set font size:
x_label_size=6
y_label_size=6
value_label_size=1

#output file name:
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
matrix_file_name=paste(file_path,title, "_correlation matrix_","_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
pvalue_file_name=paste(file_path,title, "_pvalue_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
source_target_file_name=paste(file_path,title, "_source_target_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
nodeID_file_name=paste(file_path,title, "_nodeID_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")

trans_matrix_file_name=paste(file_path,title,"_correlation matrix_tf_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
trans_pvalue_file_name=paste(file_path,title,"_pvalue_tf_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
combined_pvalue_file_name=paste(file_path,title,"_comb_cp_tf_",format(Sys.time(), "%Y%m%d_%H%M%S"),".csv",sep="")
#::::::::::::::::::::::::::::::::::::::::::::::

#choose files interactively:
x_directory<-choose.files(caption = "Select immune cells (x)")

#x_directory="C:\\Users\\wangxinyi\\Desktop\\相关性图复现\\下图复现\\031——correlat.csv"
y_directory<-choose.files(caption = "Select bacteria (y)")

#y_directory="C:\\Users\\wangxinyi\\Desktop\\相关性图复现\\下图复现\\45_031__与特定菌群对应的971改变菌群趋势.csv"
#load dataset:
x<-read.csv(x_directory,row.names=1)
y<-read.csv(y_directory,row.names=1)

#calculate correlation:
x_y_correlation<- cor(x,y)

#:::::::WRITE coefficient TO CSV::::::
#write correlation matrix:
write.csv(x_y_correlation,matrix_file_name)
#::::::::::::::::::::::::::::

#::::PLOT::::::::

#plot correlation plot without significance:
#!!please modify the output file name!!
without_sig_corre_plot<-ggcorrplot(x_y_correlation,lab=TRUE,lab_size=value_label_size)+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size)) +
  labs(title=title)
without_sig_corre_plot

#RUN to this line if no pvalue is needed


#merge data:
x_for_merge<-read.csv(x_directory)
y_for_merge<-read.csv(y_directory)
outerjoin<-merge(y_for_merge,x_for_merge,by=by,all = TRUE)
row.names(outerjoin)<-outerjoin[,1]
outerjoin[,1]<-NULL
pvalue.1<-cor_pmat(outerjoin)

##after arrange the chart manually:
#pvalue<-read.csv("D:\\鑲犻亾鑿岀兢\\鑿岀兢澶氭牱鎬х敓淇″垎鏋怽\22_鎵€鏈変簲杞笉鍚屾湀榫勫悎骞?-180522\\Correlation analysis\\genus\\Others\\pvalue.csv",row.names=1)
#pvalue.1<-cor_pmat(pvalue)
#write.csv(pvalue.1,"C:\\Users\\wangxinyi\\Desktop\\temp\\matrix_POS.csv")
#slicing index:
row_upper_limits<-ncol(y)+1
pvalue.2<-pvalue.1[c(row_upper_limits:nrow(pvalue.1)),c(1:ncol(y))]

#plot correlation plot with significance:
#!!please modify the output file name!!
with_sig_corre_plot<-ggcorrplot(x_y_correlation,lab=TRUE,lab_size=value_label_size,p.mat= pvalue.2, insig = "blank")+
  theme(axis.text.x = element_text(size=x_label_size),axis.text.y = element_text(size=y_label_size)) +
  labs(title=title)
with_sig_corre_plot

# WRITE pvalue TO CSV::::::
#write pvalue out:
write.csv(pvalue.2,pvalue_file_name)

#:::::::WRITE transformed correlation matrix and pvalue
corr<-x_y_correlation
p.mat<-pvalue.2

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


# GET SOURCE AND TARGET----

##read original pvalue files:
a<-pvalue.2

##subtract values where p<0.05:
b<-which(a<0.05,arr.ind=T)

##:::::::::::::::::::::::::
##generate source and target files:
c<-data.frame(row.names(b),names(a[b[,2]]))
colnames(c)<-c("source","target")
c$target<-gsub("\\.[0-9]*$", "", c$target) #remove ".*(numbers)" name
head(c)
write.csv(c,source_target_file_name,row.names=FALSE)

##:::::::::::::::::::::::::
##generate node ID file:
d<-data.frame(node=union(c$source,c$target))
head(d,n=20)
e<-d
#e$node<-gsub(pattern=".",replacement = "0000",e$node,fixed = TRUE)
#e$node<-gsub(pattern="0000.*",replacement = "",e$node)
e$node<-gsub("\\.[0-9]*$", "", e$node)
e<-e$node[!duplicated(e$node)] # delete replicate cells
e<-data.frame(e)
e$nodeID<-seq.int(nrow(e))
write.csv(e,nodeID_file_name,row.names=FALSE)
# END of GET SOURCE AND TARGET---------


# DRAW CIRCUS MAP---------
# from Correlational Circus Map.R
library(igraph)

#edges == source_target file== c
#nodes == node ID file == e
edges <- c
nodes <- e

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)

V(g)$size <- log(strength(g)) * 4+ 3
#change text size:
V(g)$label.cex <- 0.4

#plot and save graph:
pdf(paste0(file_path,title, "_circus_plot_",format(Sys.time(), "%Y%m%d_%H%M%S"),".pdf"))
plot(g, layout=layout_in_circle, main="Circle")
dev.off()

##arrange figures:
library(grid)
library(gridExtra)
correlation_plot<-grid.arrange(without_sig_corre_plot, with_sig_corre_plot, nrow=1,ncol = 2)

ggsave(plot = correlation_plot, paste0(title, "_correlation_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"),".pdf"), path=file_path)
#dev.off()

# ref: also see:https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
