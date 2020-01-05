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
mydata <- readRDS('BDAM58.RDS')
#Set up effects
#Base effects
myeff <- getEffects(mydata)
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
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
ans58 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
siena.table(ans58, type="html", sig=TRUE)  #Save Results
ans58  #Show Results
saveRDS(ans58, 'SM BDAM58.RDS')    

rm(list=ls())  # clear memory
gc()


#School 77 Waves 0-1
mydata <- readRDS('BDAM77.RDS')
#Set up effects
#Base effects
myeff <- getEffects( mydata )
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG')
#Structural effects
myeff <- includeEffects(myeff, density, recip, transTrip, inPopSqrt) 
#Selection effects
myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
#Influence effects
myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention')
#Final Model
myalgorithm <- sienaAlgorithmCreate( projname = 'BTRM77', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
#Run and Save
ans77 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
siena.table(ans77, type="html", sig=TRUE)  #Save Results
ans77  #Show Results
saveRDS(ans77, 'SM BDAM77.RDS') 

rm(list=ls())  # clear memory
gc()


#Both Schools
mydata <- readRDS('Siena Model.RDS')
#Set up effects
#      print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
#Base effects
myeff <- getEffects( mydata )
#Fix Rate paremetera
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

#Final Model
myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
#Run and Save
ansF <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T)  #Compute
siena.table(ansF, type="html", sig=TRUE)  #Save Results
ansF  #Show Results
saveRDS(ansF, 'SM BDAM FULL.RDS')    

ansF1 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T, prevAns=ansF)  #Compute
siena.table(ansF1, type="html", sig=TRUE)  #Save Results
ansF1  #Show Results
saveRDS(ansF, 'SM BDAM FULL1.RDS')

rm(list=ls())  # clear memory
gc()


