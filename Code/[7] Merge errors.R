


library('groundhog')
pkgs=c('this.path')
date='2024-11-15'
groundhog.library(pkgs,date)
#Paths
  RB=dirname(this.dir())
  data_path=paste0(RB,"/data/")
  other_path=paste0(RB,"/other/")
  
  
  fps=c("errors1.csv" , 
        "errors2.csv" ,
        "errors_students.csv",
        "errors_mpl.csv",
        "errors_bdm.csv",
        "errors_4box.csv")
  
  df1=read.csv(file.path(other_path,fps[1]))
  df2=read.csv(file.path(other_path,fps[2]))
  df3=read.csv(file.path(other_path,fps[3]))
  df4=read.csv(file.path(other_path,fps[4]))
  df5=read.csv(file.path(other_path,fps[5]))
  df6=read.csv(file.path(other_path,fps[6]))
  
  df1$treatment =fps[1]
  df2$treatment =fps[2]
  df3$treatment =fps[3]
  df4$treatment =fps[4]
  df5$treatment =fps[5]
  df6$treatment =fps[6]
  
  
  histx = function(x,...) {
    barplot(table(factor(x,levels=0:24)),col='red4',...)
  }
  
#Visualize
  par(mfrow=c(3,2))
  histx(df1$errors,main=fps[1])
  histx(df2$errors,main=fps[2])
  histx(df3$errors,main=fps[3])
  histx(df4$errors,main=fps[4])
  histx(df5$errors,main=fps[5])
  histx(df6$errors,main=fps[6])
  
  
#Merge
  df=rbind(df1,df2,df3,df4,df5,df6)
  
#Rename treatment dropping csv
  df$treatment <- gsub('.csv', '', df$treatment)
  
  
#Save
  fp=paste0(data_path,'Data with errors by participants - compiled by Uri.csv')
  write.csv(df, fp)