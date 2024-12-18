
#Code written by Ryan Oprea, edited by Uri SImonsohn
#2024 12 18

library('groundhog')
pkgs=c('this.path','magrittr','tibble','tidyverse','jsonlite')
date='2024-12-01'
groundhog.library(pkgs,date)

#Paths
  RB=dirname(this.dir())
  data_path=paste0(RB,"/data/")
  other_path=paste0(RB,"/other/")

  fp=paste0(other_path,"mpl_raw.csv")

D=read.csv(fp)

# Clean
D<-D[-(1:2),] #Remove junk row

S<-data.frame()

# Remove any corrupted records
D$clength<-nchar(D$dataSet)
D<-D[D$clength<50000,]

for(s in 1:length(unique(D$ID))){

				x<-D[s,]$dataSet%>% 
				  fromJSON()%>% 
				  map_if(is.data.frame, list)%>% 
				  as_tibble() 

				df<-x
				df$ID<-D[s,]$ID
				df$aveFirst<-D[s,]$aveFirst
				df$errors<-D[s,]$errors
				df$finalPay<-D[s,]$pay
				df$cuM<-D[s,]$cuM_1
				df$cuL<-D[s,]$cuL_1
				df$precisionL<-D[s,]$precision_1
				df$precisionM<-D[s,]$precision_2
				df$similarity<-D[s,]$similarity
				df$attnBoxL<-D[s,]$attnBox_1
				df$attnBoxM<-D[s,]$attnBox_4
				df$attnPayL<-D[s,]$attnPay_1
				df$attnPayM<-D[s,]$attnPay_2
				df$CR1<-D[s,]$CR1
				df$CR2<-D[s,]$CR2
				df$CR3<-D[s,]$CR3
				df$strategy<-D[s,]$strategy
				df$age<-D[s,]$age
				df$sex<-D[s,]$sex
				df$major<-D[s,]$major
				df$math<-D[s,]$math
				df$economics<-D[s,]$economics
				df$comments<-D[s,]$comments

				df<-df%>%
					select(-clicks,-clickTimes)

				if(length(S)>0){
					S<-rbind(S,df)
				}else{
					S<-df
				}

}




  D = D[,c('ID','errors')]
  fp = paste0(other_path,"/errors_mpl.csv")
  write.csv(D , fp,row.names = FALSE)
