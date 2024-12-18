#0 load the pkgs and set paths
#1 load  the data
#2 Function for plots
  #2.1  hist2: histogram for discrete data
  #2.2  hist3: Function to create histogram side-by-side lottery and complex elicitation

#3 Histograms for gains
#4 Histograms for losses
#5 Histograms for mixed gambles 
#6 Implausible valuations
#7 mixed gambles 

#-------------------------------

#0 load the pkgs & data
    
  library('groundhog')
    pkgs=c('this.path')
    date='2024-11-15'
    groundhog.library(pkgs,date)
    
    #GROUNDHOG NOTE:
  # Groundhog loads the version of each package as current on the specified date.
  # When re-running this code, you can change the date to a more recent one.
  # Most likely the script will run fine with the new date, and it will load 
  # pkgs much faster if the date is recent. But, if the code does not run with 
  # a recent date, it means a pkg update broke this code. You can 
  # switch the date back to the one above. The older pkg versions will install
  # faster if you run this code on the version of *R* available on that day.
  # See details and instructions: https://groundhogr.com/many
  
  
  #Paths
    RB=dirname(this.dir())
    data_path=paste0(RB,"/data/")
    other_path=paste0(RB,"/other/")
    
  # Paths are set dynamically based on location of this scripot
  # assuming a ResearchBox folder structure:  RB/Code  RB/Data   RB/Other
    
#----------------------------------------------------------------------------------------
    
#1 load  the data

  #From the posted code I run the script to produce the final dataset for repro
    fp=file.path(data_path,'Oprea Data - manuscript.csv')
    df = read.csv(fp)
    
  #Load the errors csv
    fp2=file.path(data_path,'Data with errors by participants - compiled by Uri.csv')
    errors = read.csv(fp2)
    
  #Merge them
    #Rename treatmetn to avoid conflict
      names(errors)[4]='treatment_errors'
      
    #Actual merge
      df2=merge(df, errors,by='ID',all=TRUE)
    
    #Check coungs
      nrow(df)
      nrow(df2)  #gained 4 observations somehow, drop them
      df2=df2[!is.na(df2$treatment),]
      nrow(df2)  #gained 4 observations somehow, drop them
      
    #Check treatments
      table(df2$treatment,df2$treatment_errors)
    
      
    
  #It has N=673 participants, matching the paper 
    length(unique(df$ID))
  
  
    table(df$session)
    table(df$treatment ,  df$session)
    
    
    #Switch back to df
    df=df2   #assign to df
    rm(df2)  #remove df2

  #Short variable names
    lotteryG10 = df$lottery[df$taskName=='G10']
    lotteryG25 = df$lottery[df$taskName=='G25']
    lotteryG75 = df$lottery[df$taskName=='G75']
    lotteryG90 = df$lottery[df$taskName=='G90']
    
    lotteryL10 = df$lottery[df$taskName=='L10']
    lotteryL25 = df$lottery[df$taskName=='L25']
    lotteryL75 = df$lottery[df$taskName=='L75']
    lotteryL90 = df$lottery[df$taskName=='L90']
    
    mirrorG10 = df$mirror[df$taskName=='G10']
    mirrorG25 = df$mirror[df$taskName=='G25']
    mirrorG75 = df$mirror[df$taskName=='G75']
    mirrorG90 = df$mirror[df$taskName=='G90']
    
    mirrorL10 = df$mirror[df$taskName=='L10']
    mirrorL25 = df$mirror[df$taskName=='L25']
    mirrorL75 = df$mirror[df$taskName=='L75']
    mirrorL90 = df$mirror[df$taskName=='L90']
    
    
#------------------------------------------------------
# Function for plots 
    
    
#2 hist2: Neater histogram
 
  hist2=function(x,col='dodgerblue',...)
  {
    xs <- seq(min(c(x,0)),max(c(x,25.5)),.5)
    f <- table(factor(x, levels = xs))
    f = f/sum(f)
    ylim = range(f)
    ylim[2]=ylim[2]*1.15
    plot(xs,as.numeric(f),ylim=ylim,pch=16,cex=.1,...)
    segments(x0=xs,x1=xs,y0=0,y1=f,lwd=4,col=col)
  }
  
  

  
  
  
#2 hist3: Function to create histogram side-by-side lottery and complex elicitation
       hist3=function(data=df,l,n,col1='red4',col2='dodgerblue',...)
      {
        
        df=data
        #Get WTP
          lottery = df$lottery[df$type==l & df$prob ==n]
          mirror =  df$mirror[df$type==l & df$prob ==n]
          wtp=c(lottery,mirror)

        #Freq tables
        xs <- seq(min(wtp),max(wtp),.5)
        fl <- table(factor(lottery, levels = xs))
        fm <- table(factor(mirror, levels = xs))
        fl=fl/sum(fl)
        fm=fm/sum(fm)
        
        
        #ylim  
        fs=c(fl,fm)
        ylim = range(fs)
        ylim[2]=ylim[2] + .45*diff(ylim)
        
        #Empty plot
        plot(xs,as.numeric(fl),ylim=ylim,pch=16,cex=.1,ylab='% of participants',xlab='Certainty/Simplicity Equivalent',main=paste0(l,n),las=1,col='white')
        
        #Vertical vars
        
          segments(x0=xs-.1,x1=xs-.1,y0=0,y1=fl,lwd=4,col=col1)
          segments(x0=xs+.1,x1=xs+.1,y0=0,y1=fm,lwd=4,col=col2)
          
        #Line at EV
          #abline(v=n/100*25,col='gray',lwd=3,lty=2)
          outcome=ifelse(l=="G",25,-25)
          points(n/100*outcome,ylim[2]*.75,pch=16,col='green4',cex=2)
          text(x=n/100*outcome, ylim[2]*.75,paste0('EV=',n/100*25),pos=3,col='green4')
          
        #Legend
          ml=round(median(lottery),1)
          mm=round(median(mirror),1)
          legend('top',bty='n',col=c(col1,col2),lty=c(1,1),lwd=4,
                 legend=c(paste0("Lottery (Med = ",ml,")"),
                          paste0("Mirror (Med = ",mm,")")))
          
          
      }

    

#3 histograms for gains
    n.all = length(unique(df$ID))
    n.0   = length(unique(df.e0$ID))
    
  #All participants
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Adjust 'oma' for outer margins
      hist3(data=df, l='G',n=10)
      hist3(data=df, l='G',n=25)
      hist3(data=df, l='G',n=75)
      hist3(data=df, l='G',n=90)
      mtext(paste0("All Participants (N=",n.all,")"), outer = TRUE, cex = 1.5, font = 2)
      
      
  #No errors    
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Adjust 'oma' for outer margins
      hist3(data=df.e0, l='G',n=10)
      hist3(data=df.e0, l='G',n=25)
      hist3(data=df.e0, l='G',n=75)
      hist3(data=df.e0, l='G',n=90)
      mtext(paste0("Subset without errors (N=",n.0,")"), outer = TRUE, cex = 1.5, font = 2)
      


#4 histograms for Losses

  #All participants
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Adjust 'oma' for outer margins
      hist3(data=df, l='L',n=10)
      hist3(data=df, l='L',n=25)
      hist3(data=df, l='L',n=75)
      hist3(data=df, l='L',n=90)
      mtext(paste0("All Participants (N=",n.all,")"), outer = TRUE, cex = 1.5, font = 2)
      
      
  #No errors    
    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Adjust 'oma' for outer margins
      hist3(data=df.e0, l='L',n=10)
      hist3(data=df.e0, l='L',n=25)
      hist3(data=df.e0, l='L',n=75)
      hist3(data=df.e0, l='L',n=90)
      mtext(paste0("Subset without errors (N=",n.0,")"), outer = TRUE, cex = 1.5, font = 2)
      

#6 Implausible valuations
      
      
  #How many people may more than $10 for a 10% chance of 25
      mean(df$lottery[df$taskName=="G10"]>15)
      mean(df.e0$lottery[df.e0$taskName=="G10"]>15)
      
      

#7 mixed gambles 
      
      lottery.A10=df$lottery[df$taskName=='A10']
      lottery.A15=df$lottery[df$taskName=='A15']
      mirror.A10=df$mirror[df$taskName=='A10']
      mirror.A15=df$mirror[df$taskName=='A15']
      
      
   par(mfrow=c(2,2))
      
      hist2(lottery.A10,main="Lottery 50:50, lose $10, and gain $x, vs $0 for sure",xlab='$x chosen ')
      hist2(lottery.A15,main="Lottery 50:50, lose $15, and gain $x, vs $0 for sure",xlab='$x chosen ')
      hist2(mirror.A10,main="For sure get mean(-$10 , $x) vs $0 for sure",xlab='$x chosen ')
      hist2(mirror.A15,main="For sure get mean(-$15 , $x) vs $0 for sure",xlab='$x chosen ')
     
      
  #Shares with impossible values 
        mean(lottery.A10<=0)
        mean(lottery.A15<=0)
        
        mean(mirror.A10<=0)
        mean(mirror.A10<10)
        mean(mirror.A10<9)
        mean(mirror.A15<15)
        mean(mirror.A15<=0)



    
    
    

