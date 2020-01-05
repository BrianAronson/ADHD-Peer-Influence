library(RSiena)
library(gmodels)
library(parallel)

#Set Directory
  tempfile(tmpdir="//secure-nas-fe01.oit.duke.edu/SSRI-ADHEALTH/moody/work/bda13/")
  setwd("//secure-nas-fe01.oit.duke.edu/SSRI-ADHEALTH/moody/work/bda13/")


#load data
  mydata <- readRDS('Siena Model.RDS')
#Base effects
  myeff <- getEffects( mydata )
#FR Flag effects
  myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG') #Rate effect (i.e. how it effects everyone)
  myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG') #Ego effect (i.e. how it effects the individual)
#Structural effects
  myeff <- includeEffects(myeff, density, recip, transTrip, inPopSqrt) 
#Selection effects
  myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
#Influence effects
  myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention')
#School dummy effects 
  myeff <- includeEffects(myeff, egoX, interaction1 = 'SchoolDummy')
  #myeff <- includeInteraction(myeff, recip, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, density, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, transTrip, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, inPopSqrt, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, inPopSqrt, egoX, interaction1 = c('', 'SchoolDummy'))
  #myeff <- includeInteraction(myeff, linear, effFrom, interaction1 = c('', 'SchoolDummy'), name='DifficultyAttention')

#Model
  myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
#Run and Save
  ansF <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
  siena.table(ansF, type="html", sig=TRUE)  #Save Results
  ansF  #Show Results
  saveRDS(ansF, 'SM BDAM FULL.RDS')    

#Convergence less than optimal, continue
  ansF1 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T, prevAns=ansF)  #Compute
  siena.table(ansF1, type="html", sig=TRUE)  #Save Results
  ansF1  #Show Results
  saveRDS(ansF1, 'SM BDAM FULL1.RDS')