#Basic Trouble Relaxing Models and Meta Analysis


library(RSiena)
library(gmodels)
library(parallel)
#Set Directory
tempfile(tmpdir="")
setwd("")

rm(list=ls())  # clear memory
gc()


#School 58 Wave 0-1
    mydata <- readRDS('BDAM58F.RDS')
  #Set up effects
    #Base effects
      myeff <- getEffects(mydata)
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
#Fix Rate paremeters
      #df<-as.data.frame(myeff$initialValue)  
      #myeff <- setEffect(myeff, Rate, type='rate', initialValue=df[1,1], fix=TRUE, test=TRUE, period=1)
      #myeff <- setEffect(myeff, Rate, type='rate', initialValue=df[2,1], fix=TRUE, test=TRUE, period=2)
    #FR Flag effects
   #   myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG')
      myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG')
    #Structural effects
      myeff <- includeEffects(myeff, recip, density, transTrip, inPopSqrt)  # and  are too correlated with basic rate
    #Selection effects
      myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
    #Influence effects
      myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention')
    #Final Model
      myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM58F', useStdInits = FALSE, nsub=4, n3=2000, diagonalize = .7) #Final Algorithm
    #Run and Save
      ans58F <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
      siena.table(ans58F, type="html", sig=TRUE)  #Save Results
      ans58F  #Show Results
      saveRDS(ans58F, 'SM BDAM58F.RDS')    
      
      rm(list=ls())  # clear memory
      gc()
      
      #School 58 Wave 1-2
      mydata <- readRDS('BDAM58L.RDS')
      #Set up effects
      #Base effects
      myeff <- getEffects( mydata )
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
      #Fix Rate paremeters
      #df<-as.data.frame(myeff$initialValue)  
      #myeff <- setEffect(myeff, Rate, type='rate', initialValue=df[1,1], fix=TRUE, test=TRUE, period=1)
      #myeff <- setEffect(myeff, Rate, type='rate', initialValue=df[2,1], fix=TRUE, test=TRUE, period=2)
      #FR Flag effects
      #myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG')
      myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG')
      #Structural effects
      myeff <- includeEffects(myeff, recip, density, transTrip, inPopSqrt)  #inpop and  are too correlated with basic rate
      #Selection effects
      myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
      #Influence effects
      myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention')
      #Final Model
      myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM58L', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
      #Run and Save
      ans58L <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
      siena.table(ans58L, type="html", sig=TRUE)  #Save Results
      ans58L  #Show Results
      saveRDS(ans58L, 'SM BDAM58L.RDS')    
      
      rm(list=ls())  # clear memory
      gc()
      
      
    
#School 77 Waves 0-1
    mydata <- readRDS('BDAM77F.RDS')
#Set up effects
      #Base effects
        myeff <- getEffects( mydata )
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
    #Fix Rate paremetera
        #myeff <- includeEffects(myeff, Rate, type='rate', fix=TRUE, test=TRUE)
      #FR Flag effects
        #myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG')
        myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG')
      #Structural effects
        myeff <- includeEffects(myeff, density, recip, transTrip, inPopSqrt) 
      #Selection effects
        myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
      #Influence effects
        myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention')
    #Final Model
      myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM77F', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
    #Run and Save
      ans77F <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
      siena.table(ans77F, type="html", sig=TRUE)  #Save Results
      ans77F  #Show Results
      saveRDS(ans77F, 'SM BDAM77F.RDS') 
      
      rm(list=ls())  # clear memory
      gc()
      
      
#School 77 waves 1-2
      mydata <- readRDS('BDAM77L.RDS')
      #Set up effects
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
      #Base effects
      myeff <- getEffects( mydata )
      #Fix Rate paremetera
      #myeff <- includeEffects(myeff, Rate, type='rate', fix=TRUE, test=TRUE)
      #FR Flag effects
#      myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG')
      myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG')
      #Structural effects
      myeff <- includeEffects(myeff, density, recip, transTrip, inPopSqrt) 
      #Selection effects
      myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
      #Influence effects
      myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention')
      #Final Model
      myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM77L', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
      #Run and Save
      ans77L <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
      siena.table(ans77L, type="html", sig=TRUE)  #Save Results
      ans77L  #Show Results
      saveRDS(ans77L, 'SM BDAM77L.RDS')    
      
      rm(list=ls())  # clear memory
      gc()
      


#Meta Analysis
ans58F<-readRDS('SM BDAM58F.RDS')
ans58L<-readRDS('SM BDAM58L.RDS')
ans77F<-readRDS('SM BDAM77F.RDS')
ans77L<-readRDS('SM BDAM77L.RDS')
meta<-siena08(ans58F,ans58L,ans77F,ans77L)
summary(meta)
plo<-plot(meta,layout=c(3,1))
plo
print(meta, file=TRUE,type="html")
siena.table(meta)