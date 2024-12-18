
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


#datapaths
  main = dirname(dirname(dirname(this.dir())))
  data_path = paste0(main,"/Data/")
  fp        = paste0(other_path,"error2_raw.csv")
  D=read.csv(fp)


# Clean
D<-D[-(1:2),] #Remove junk row

S<-data.frame()

# Remove any corrupted records
D$clength<-nchar(D$dataSet)
D<-D[D$clength<50000,]

for(s in 1:length(unique(D$ID))){

				x<-D[s,]$dataSet%>% 
				  # readLines()%>%
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


# Set risk/loss neutral benchmarks
S$pred<-23
S[S$listName=='L2_PWG25',]$pred<-19
S[S$listName=='L3_PWG50',]$pred<-13
S[S$listName=='L15_2PWG50',]$pred<-13
S[S$listName=='L4_PWG75',]$pred<-7
S[S$listName=='L18_ePWG75',]$pred<-5.5
S[S$listName=='L5_PWG90',]$pred<-3
S[S$listName=='L6_VCG25',]$pred<-7.5
S[S$listName=='L7_VCG37',]$pred<-5
S[S$listName=='L8_LA',]$pred<-20.5
S[S$listName=='L9_LA',]$pred<-18
S[S$listName=='L10_PWL10',]$pred<-2
S[S$listName=='L11_PWL25',]$pred<-6
S[S$listName=='L17_ePWL25',]$pred<-4.5
S[S$listName=='L13_PWL50',]$pred<-12
S[S$listName=='L16_2PWL50',]$pred<-12
S[S$listName=='L14_PWL75',]$pred<-18
S[S$listName=='L15_PWL90',]$pred<-22
S[S$listName=='L16_VCL25',]$pred<-16.5
S[S$listName=='L17_VCL37',]$pred<-19
S[S$listName=='L8_LA',]$listName<-'L8_LA1'
S[S$listName=='L9_LA',]$listName<-'L9_LA2'

B<-S
B$boxes<-100; B$hasnoise<-FALSE; B$session<-2; B$imistake<-TRUE

df2=S[!duplicated(S$ID),]

df2$errors=as.numeric(df2$errors)
barplot(table(df2$errors))


df.error = df2[,c('ID','errors')]
fp = paste0(other_path,"/errors2.csv")
write.csv(df.error , fp,row.names = FALSE)

