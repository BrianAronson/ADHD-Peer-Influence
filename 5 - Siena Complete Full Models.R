library(RSiena)
library(gmodels)
library(parallel)
#Set Directory
  tempfile(tmpdir="bda13/")
  
  rm(list=ls())  # clear memory
  gc()


#Difficulty paying attention
  mydata <- readRDS('Siena Model.RDS')
#Set up effects
  #print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
#Base effects
  myeff <- getEffects( mydata )
#FR Flag effects
  myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG') #Rate effect (i.e. how it effects everyone)
  myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG') #Ego effect (i.e. how it effects the individual)
#Structural effects
  myeff <- includeEffects(myeff, density, recip, transTrip, inPopSqrt) 
#Selection effects
  myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
  myeff <- includeEffects(myeff, sameX, interaction1="Race")
  myeff <- includeEffects(myeff, sameX, interaction1="Grade")
  myeff <- includeEffects(myeff, sameX, interaction1="Female")
  myeff <- includeEffects(myeff, simX, interaction1="GPA")
  myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="Counseling")
  myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="TeacherTrouble")
  myeff <- includeEffects(myeff, X, interaction1='overlap')
  myeff <- includeEffects(myeff, sameX, interaction1="HighDifficultyAttention")
  myeff <- includeInteraction(myeff, X, sameX, interaction1 = c('overlap','HighDifficultyAttention') )

#Social Influence
  myeff <- includeEffects(myeff, avSim,interaction1='friendship', name='DifficultyAttention')
  myeff <- includeEffects(myeff, AltsAvAlt, interaction1='Counseling', interaction2='friendship', name='DifficultyAttention')
  myeff <- includeEffects(myeff, AltsAvAlt, interaction1='TeacherTrouble', interaction2='friendship', name='DifficultyAttention')

#Behavior Controls
  myeff <- includeEffects(myeff, effFrom, interaction1='Female', name='DifficultyAttention')
  myeff <- includeEffects(myeff, effFrom, interaction1='White', name='DifficultyAttention')
  myeff <- includeEffects(myeff, effFrom, interaction1='GPA', name='DifficultyAttention')
  myeff <- includeEffects(myeff, effFrom, interaction1='Counseling', name='DifficultyAttention')
  myeff <- includeEffects(myeff, effFrom, interaction1='TeacherTrouble', name='DifficultyAttention')
  #myeff <- includeEffects(myeff, effFrom, interaction1='Age', name='DifficultyAttention') #not remotely significant
  #myeff <- includeEffects(myeff, effFrom, interaction1='Black', name='DifficultyAttention') #not remotely significant
  #myeff <- includeEffects(myeff, effFrom, interaction1='Latino', name='DifficultyAttention')#not remotely significant
  #myeff <- includeEffects(myeff, effFrom, interaction1='Asian', name='DifficultyAttention')#not remotely significant

#School dummy effects
  myeff <- includeEffects(myeff, egoX, interaction1 = 'SchoolDummy') #school dummy on ego friend rate
  myeff <- includeInteraction(myeff, sameX, egoX,interaction1 = c('Race', 'SchoolDummy')) #school on race selection
  myeff <- includeInteraction(myeff, X, egoX, interaction1 = c('overlap','SchoolDummy'))
  #myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('', 'SchoolDummy'), name='DifficultyAttention') #school on behavior rate - Not significant
  #myeff <- includeInteraction(myeff, recip, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, transTrip, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, inPopSqrt, egoX, interaction1 = c('', 'SchoolDummy'))

#Remove Unimportant effects
  myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="Counseling", include=FALSE)
  myeff <- includeEffects(myeff, altX, egoX, interaction1="TeacherTrouble", include=FALSE)
  myeff <- includeEffects(myeff, AltsAvAlt, interaction1='Counseling', interaction2='friendship', name='DifficultyAttention', include=FALSE)
  myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG',include=FALSE) #Rate effect (i.e. how it effects everyone)

#Final Model
  myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
#Run and Save
  ansCF <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
  siena.table(ansCF, type="html", sig=TRUE)  #Save Results
  ansCF  #Show Results
  saveRDS(ansCF, 'SM CDAM FULL.RDS')    

#Remove or Add effects
  myeff <- includeInteraction(myeff, simX, sameX, interaction1 = c('TeacherTrouble','HighDifficultyAttention'),include=FALSE )
  myeff <- includeEffects(myeff, effFrom, interaction1='GPA', name='DifficultyAttention',include=FALSE)
  myeff <- includeEffects(myeff, simX, interaction1="GPA",include=FALSE)

#Convergence less than optimal, continue
  ansCF <- readRDS('SM CDAM FULL.RDS')
  ansCF1 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T, prevAns=ansCF)  #Compute
  siena.table(ansCF1, type="html", sig=TRUE)  #Save Results
  ansCF1  #Show Results
  saveRDS(ansCF1, 'SM CDAM FULL1.RDS')
  
