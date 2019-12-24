##https://stackoverflow.com/questions/28244457/get-row-and-column-name-of-data-frame-according-to-condition-in-r
##gsub to replace ".*"file:https://stackoverflow.com/questions/11849309/delete-a-dot-and-a-number-at-the-end-of-a-character-string

#output file name:
title="Tg_11_12m"
file_path="C:\\Users\\wangxinyi\\Desktop\\temp\\"
source_target_file_name=paste(file_path,title,"_source_target.csv",sep="")
nodeID_file_name=paste(file_path,title,"_nodeID.csv",sep="")



##read original pvalue files:
a<-read.csv(choose.files(caption="choose pvalue file!!"),row.names=1)

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
#end of code
