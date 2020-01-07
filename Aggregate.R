#data type: (the following will plot correlation among WT,Tg,cocageWT)
#c(1,1) must be "group", which is the sample name to be aggregated

#group	NK	NKT	T
#4_Tg	0.97	0.59	45.1
#4_Tg	0.6	0.55	57.9
#4_Tg	0.54	0.26	45.9
#7_Tg	0.22	0.43	83.4
#7_Tg	0.3	0.74	80.6
#7_Tg	0.32	0.65	83.9
#9_Tg	0.69	0.5	61.6
#9_Tg	0.79	0.6	52.4

#-------------------------INPUT-------
#col number of the first value, 1-index. default = 2.
value_start_from_col=2
#--------------

#start aggregate
df_agrgte<-choose.files(caption="choose .csv matrix to be aggregated")
df_agrgte<-read.csv(df_agrgte)
newRow<-aggregate(df_agrgte[, c(value_start_from_col:ncol(df_agrgte))], list(Group=df_agrgte$group), mean)
write.csv(newRow,"C:\\Users\\wangxinyi\\Desktop\\temp\\aggregated_new.csv")

#end of code

#ref
#https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame
