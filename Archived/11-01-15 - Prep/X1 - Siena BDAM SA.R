#Basic Trouble Relaxing via Structural Analysis
mydata <- readRDS('Siena Model.RDS')
#Set up effects
#Base effects
myeff <- getEffects( mydata )
#Fix Rate paremetera
myeff <- includeEffects(myeff, Rate, type='rate', fix=TRUE, test=TRUE)
#FR Flag effects
myeff <- includeEffects(myeff, RateX, type='rate', interaction1='FRFLAG')
myeff <- includeEffects(myeff, egoX, interaction1='FRFLAG')
#Structural effects
myeff <- includeEffects(myeff, density, recip, transTrip, inPopSqrt) 
#Selection effects
myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="DifficultyAttention")
#    myeff <- includeEffects(myeff, simX, altX, egoX, interaction1="TroubleRelaxing")
#Influence effects
myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='DifficultyAttention') #Separate effects of similarity increase
#    myeff <- includeEffects(myeff, avSim, interaction1='friendship', name='TroubleRelaxing') #Separate effects of similarity increase
#School Dummy Effects
myeff <- includeEffects(myeff, egoX, interaction1 = 'SchoolDummy') #Ego Alters


#Final Model
myalgorithm <- sienaAlgorithmCreate( projname = 'SADHD1', useStdInits = FALSE, nsub=4, n3=2000) #Final Algorithm
#Run and Save
ans <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=3, useCluster=T, initC=T)  #Compute
siena.table(ans, type="html", sig=TRUE)  #Save Results
ans  #Show Results
