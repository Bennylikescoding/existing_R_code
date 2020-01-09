# metagenomics readcounts->cls

#Data processing
#ref:http://mixomics.org/mixmc/pre-processing/

#install.packages("rgr")
library(rgr)

#
#Species	s__Candidatus_Bathyarchaeota_archaeon_B23	s__Candidatus_Bathyarchaeota_archaeon_B24	s__Candidatus_Bathyarchaeota_archaeon_B25	s__Candidatus_Bathyarchaeota_archaeon_B26-1	s__Candidatus_Bathyarchaeota_archaeon_B26-2
#A2170_A	10	126	24	16	12
#A2842_A	52	26	2	0	8
#A4059_A	176	40	4	4	8
#B3646_A	178	90	0	14	14
#B3649_A	220	66	2	0	14
#A4090_A	36	84	82	4	8
#X13513_A	40	150	114	44	32

#select read counts file, row.names dont have repeat name
file_directory<-choose.files(caption = "Select reads counts file")
rawdf<-read.csv(file_directory,row.names=1)

#rawdf<-t(rawdf)
#set offset=0.01 to avoid 0 points
osdf<-rawdf + 0.01
#sum(which(osdf == 0))

#Log ratio transformation, using central log ratio
clr_df<-clr(osdf)

#output clr-ed (central log ratio transformed) file
write.csv(clr_df,"C:\\Users\\wangxinyi\\Desktop\\clr_df_test.csv")


