  comp<-as.data.frame(ADHDFriends58M0$Missing)
  comp$begin<-ifelse(ADHDFriends58M0$Missing==0,1.0,ifelse(ADHDFriends58M1$Missing==0,2.0,ifelse(ADHDFriends58M2$Missing==0,3.0,0)))
  comp$end<-ifelse(ADHDFriends58M1$Missing==1.0,2.0,ifelse(ADHDFriends58M2$Missing==1.0,2.5,3.0))
  comp$`ADHDFriends58M0$Missing`<-NULL
  comp.list <- as.list(as.data.frame(t(comp)))
  changes <- sienaCompositionChange(comp.list)


#ADHD Homophily Sienna Model 
  ADHD1<-as.matrix(data.frame(V1=ADHDFriends58M0$ADHD, V2=ADHDFriends58M1$ADHD, V3=ADHDFriends58M2$ADHD))
  FriendsArrayT1 <- array( c( S58W0, S58W1,S58W2),
                         dim = c( 972, 972, 3 ) )
  friendship1 <- sienaDependent(FriendsArrayT1)   #Network
  SADHD1 <- sienaDependent( ADHD1, type="behavior" ) #behavior
  mydata10 <- sienaDataCreate(friendship1, SADHD1, changes ) #Data
  
  saveRDS(mydata10,'Siena Model.RDS')
  rm(list=ls())  # clear memory
  gc()
  mydata10 <- readRDS('Siena Model.RDS')
  
  
  myeff10 <- getEffects( mydata10 ) #Set up effects
  myeff10 <- includeEffects(myeff10, density, recip, transTrip, inPopSqrt) #Structural effects
  myeff10 <- includeEffects(myeff10, simX, altX, egoX, interaction1="SADHD1") #Trait on friendship formation
  myalgorithm10 <- sienaAlgorithmCreate( projname = 'SADHD1',diagonalize = 0.5, doubleAveraging = 0 ) #Final Algorithm
  ans10 <- siena07( myalgorithm10, data = mydata10, effects = myeff10)  #Compute
  siena.table(ans10, type="html", sig=TRUE)  #Save Results
  ans10  #Show Results
  
  
#ADHD Influence Sienna Model
  

mydataMG <- sienaGroupCreate(list(mydata58,mydata77))
  
  
  #DifficultyAttention and Trouble Relaxing

#Create longitudinal datasets for key covariate 
  ADHDScale1<-as.matrix(data.frame(V1=ADHDFriendsM1$ADHDScale1, V2=ADHDFriendsM2$ADHDScale1))
  DifficultyAttention1<-as.matrix(data.frame(V1=ADHDFriendsM1$DifficultyAttention, V2=ADHDFriendsM2$DifficultyAttention))
  EasilyBored1<-as.matrix(data.frame(V1=ADHDFriendsM1$EasilyBored, V2=ADHDFriendsM2$EasilyBored))
  TroubleRelaxing1<-as.matrix(data.frame(V1=ADHDFriendsM1$TroubleRelaxing, V2=ADHDFriendsM2$TroubleRelaxing))
  EasilyDistracted1<-as.matrix(data.frame(V1=ADHDFriendsM1$EasilyDistracted, V2=ADHDFriendsM2$EasilyDistracted))

#Sienna models
#Network
friendship1 <- sienaDependent(FriendsArrayT1)
friendship2 <- sienaDependent(FriendsArrayT2)

#Behavior

SADHD1 <- sienaDependent( ADHD1, type="behavior" )
SADHD2 <- sienaDependent( ADHD2, type="behavior" )
SADHDScale1 <- sienaDependent( ADHDScale1, type="behavior" )
SADHDScale2 <- sienaDependent( ADHDScale2, type="behavior" )
STroubleRelaxing1 <- sienaDependent( TroubleRelaxing1, type="behavior" )
STroubleRelaxing2 <- sienaDependent( TroubleRelaxing2, type="behavior" )
SDifficultyAttention1 <- sienaDependent( DifficultyAttention1, type="behavior" )
SDifficultyAttention2 <- sienaDependent( DifficultyAttention2, type="behavior" )
SEasilyBored1 <- sienaDependent( EasilyBored1, type="behavior" )
SEasilyBored2 <- sienaDependent( EasilyBored2, type="behavior" )
SGutFeeling1 <- sienaDependent( GutFeeling1, type="behavior" )
SGutFeeling2 <- sienaDependent( GutFeeling2, type="behavior" )
SEasilyDistracted1 <- sienaDependent( EasilyDistracted1, type="behavior" )
SEasilyDistracted2 <- sienaDependent( EasilyDistracted2, type="behavior" )


#Data
mydata10 <- sienaDataCreate(friendship1, SADHD1 )
mydata20 <- sienaDataCreate(friendship2, SADHD2 )
mydata11 <- sienaDataCreate(friendship1, SADHDScale1 )
mydata21 <- sienaDataCreate(friendship2, SADHDScale2 )
mydata12 <- sienaDataCreate(friendship1, STroubleRelaxing1 )
mydata22 <- sienaDataCreate(friendship2, STroubleRelaxing2 )
mydata13 <- sienaDataCreate(friendship1, SDifficultyAttention1 )
mydata23 <- sienaDataCreate(friendship2, SDifficultyAttention2 )
mydata14 <- sienaDataCreate(friendship1, SEasilyBored1 )
mydata24 <- sienaDataCreate(friendship2, SEasilyBored2 )
mydata15 <- sienaDataCreate(friendship1, SGutFeeling1 )
mydata25 <- sienaDataCreate(friendship2, SGutFeeling2 )
mydata16 <- sienaDataCreate(friendship1, SEasilyDistracted1 )
mydata26 <- sienaDataCreate(friendship2, SEasilyDistracted2 )


#Model
myeff10 <- getEffects( mydata10 )
myeff20 <- getEffects( mydata20 )
myeff11 <- getEffects( mydata11 )
myeff21 <- getEffects( mydata21 )
myeff12 <- getEffects( mydata12 )
myeff22 <- getEffects( mydata22 )
myeff13 <- getEffects( mydata13 )
myeff23 <- getEffects( mydata23 )
myeff14 <- getEffects( mydata14 )
myeff24 <- getEffects( mydata24 )
myeff15 <- getEffects( mydata15 )
myeff25 <- getEffects( mydata25 )
myeff16 <- getEffects( mydata16 )
myeff26 <- getEffects( mydata26 )

#print01Report( mydata, modelname = 'Basic Stats' )  #Basic Descriptive Stats
#myeff <- includeEffects(myeff, linear, simX, altX, egoX, outdeg, recipDeg, transTrip, inPop,
#                        name="STroubleRelaxing", interaction1="friendship")
#Structural effects
myeff10 <- includeEffects(myeff10, density, recip, transTrip, inPopSqrt)
myeff20 <- includeEffects(myeff20, density, recip, transTrip, inPopSqrt)
myeff11 <- includeEffects(myeff11, density, recip, transTrip, inPopSqrt)
myeff21 <- includeEffects(myeff21, density, recip, transTrip, inPopSqrt)
myeff12 <- includeEffects(myeff12, density, recip, transTrip, inPopSqrt)
myeff22 <- includeEffects(myeff22, density, recip, transTrip, inPopSqrt)
myeff13 <- includeEffects(myeff13, density, recip, transTrip, inPopSqrt)
myeff23 <- includeEffects(myeff23, density, recip, transTrip, inPopSqrt)
myeff14 <- includeEffects(myeff14, density, recip, transTrip, inPopSqrt)
myeff24 <- includeEffects(myeff24, density, recip, transTrip, inPopSqrt)
myeff15 <- includeEffects(myeff15, density, recip, transTrip, inPopSqrt)
myeff25 <- includeEffects(myeff25, density, recip, transTrip, inPopSqrt)
myeff16 <- includeEffects(myeff16, density, recip, transTrip, inPopSqrt)
myeff26 <- includeEffects(myeff26, density, recip, transTrip, inPopSqrt)

#ADHD trait effect on Friendship formation
myeff10 <- includeEffects(myeff10, simX, altX, egoX, interaction1="SADHD1")
myeff20 <- includeEffects(myeff20, simX, altX, egoX, interaction1="SADHD2")
myeff11 <- includeEffects(myeff11, simX, altX, egoX, interaction1="SADHDScale1")
myeff21 <- includeEffects(myeff21, simX, altX, egoX, interaction1="SADHDScale2")
myeff12 <- includeEffects(myeff12, simX, altX, egoX, interaction1="STroubleRelaxing1")
myeff22 <- includeEffects(myeff22, simX, altX, egoX, interaction1="STroubleRelaxing2")
myeff13 <- includeEffects(myeff13, simX, altX, egoX, interaction1="SDifficultyAttention1")
myeff23 <- includeEffects(myeff23, simX, altX, egoX, interaction1="SDifficultyAttention2")
myeff14 <- includeEffects(myeff14, simX, altX, egoX, interaction1="SEasilyBored1")
myeff24 <- includeEffects(myeff24, simX, altX, egoX, interaction1="SEasilyBored1")
myeff15 <- includeEffects(myeff15, simX, altX, egoX, interaction1="SGutFeeling1")
myeff25 <- includeEffects(myeff25, simX, altX, egoX, interaction1="SGutFeeling2")
myeff16 <- includeEffects(myeff16, simX, altX, egoX, interaction1="SEasilyDistracted1")
myeff26 <- includeEffects(myeff26, simX, altX, egoX, interaction1="SEasilyDistracted2")

#Network influence on ADHD trait
myeff11 <- includeEffects(myeff11, name = "SADHDScale1",  avSim, interaction1 = "friendship1" )
myeff21 <- includeEffects(myeff21, name = "SADHDScale2",  avSim, interaction1 = "friendship2" )
myeff12 <- includeEffects(myeff12, name = "STroubleRelaxing1",  avSim, interaction1 = "friendship1" )
myeff22 <- includeEffects(myeff22, name = "STroubleRelaxing2",  avSim, interaction1 = "friendship2" )
myeff13 <- includeEffects(myeff13, name = "SDifficultyAttention1",  avSim, interaction1 = "friendship1" )
myeff23 <- includeEffects(myeff23, name = "SDifficultyAttention2",  avSim, interaction1 = "friendship2" )
myeff14 <- includeEffects(myeff14, name = "SEasilyBored1",  avSim, interaction1 = "friendship1" )
myeff24 <- includeEffects(myeff24, name = "SEasilyBored2",  avSim, interaction1 = "friendship2" )
myeff15 <- includeEffects(myeff15, name = "SGutFeeling1",  avSim, interaction1 = "friendship1" )
myeff25 <- includeEffects(myeff25, name = "SGutFeeling2",  avSim, interaction1 = "friendship2" )
myeff16 <- includeEffects(myeff16, name = "SEasilyDistracted1",  avSim, interaction1 = "friendship1" )
myeff26 <- includeEffects(myeff26, name = "SEasilyDistracted2",  avSim, interaction1 = "friendship2" )

#Remove Unimportant effects
#myeff <- includeEffects(myeff, name = "STroubleRelaxing",  quad,  include = FALSE )
myeff1 #What effects to include
myeff2 #What effects to include

#best diagonalize =.5, followed by .2
myalgorithm10 <- sienaAlgorithmCreate( projname = 'SADHD1',diagonalize = 0.5, doubleAveraging = 0 )
ans10 <- siena07( myalgorithm10, data = mydata10, effects = myeff10)
siena.table(ans10, type="html", sig=TRUE)

myalgorithm20 <- sienaAlgorithmCreate( projname = 'SADHD1',diagonalize = 0.5, doubleAveraging = 0 )
ans20 <- siena07( myalgorithm20, data = mydata20, effects = myeff20)
siena.table(ans20, type="html", sig=TRUE)

myalgorithm11 <- sienaAlgorithmCreate( projname = 'SADHDScale1',diagonalize = 0.5, doubleAveraging = 0 )
ans11 <- siena07( myalgorithm11, data = mydata11, effects = myeff11)
siena.table(ans11, type="html", sig=TRUE)

myalgorithm21 <- sienaAlgorithmCreate( projname = 'SADHDScale1',diagonalize = 0.5, doubleAveraging = 0 )
ans21 <- siena07( myalgorithm21, data = mydata21, effects = myeff21)
siena.table(ans21, type="html", sig=TRUE)

myalgorithm12 <- sienaAlgorithmCreate( projname = 'TroubleRelaxing1',diagonalize = 0.5, doubleAveraging = 0 )
ans12 <- siena07( myalgorithm12, data = mydata12, effects = myeff12)
siena.table(ans12, type="html", sig=TRUE)

myalgorithm22 <- sienaAlgorithmCreate( projname = 'TroubleRelaxing2',diagonalize = 0.5, doubleAveraging = 0 )
ans22 <- siena07( myalgorithm22, data = mydata22, effects = myeff22)
siena.table(ans22, type="html", sig=TRUE)

myalgorithm13 <- sienaAlgorithmCreate( projname = 'SDifficultyAttention1',diagonalize = 0.5, doubleAveraging = 0 )
ans13 <- siena07( myalgorithm13, data = mydata13, effects = myeff13)
siena.table(ans13, type="html", sig=TRUE)

myalgorithm23 <- sienaAlgorithmCreate( projname = 'SDifficultyAttention2',diagonalize = 0.5, doubleAveraging = 0 )
ans23 <- siena07( myalgorithm23, data = mydata23, effects = myeff23)
siena.table(ans23, type="html", sig=TRUE)

myalgorithm14 <- sienaAlgorithmCreate( projname = 'SEasilyBored1',diagonalize = 0.5, doubleAveraging = 0 )
ans14 <- siena07( myalgorithm14, data = mydata14, effects = myeff14)
siena.table(ans14, type="html", sig=TRUE)

myalgorithm24 <- sienaAlgorithmCreate( projname = 'SEasilyBored2',diagonalize = 0.5, doubleAveraging = 0 )
ans24 <- siena07( myalgorithm24, data = mydata24, effects = myeff24)
siena.table(ans24, type="html", sig=TRUE)

myalgorithm15 <- sienaAlgorithmCreate( projname = 'SGutFeeling1',diagonalize = 0.5, doubleAveraging = 0 )
ans15 <- siena07( myalgorithm15, data = mydata15, effects = myeff15)
siena.table(ans15, type="html", sig=TRUE)

myalgorithm25 <- sienaAlgorithmCreate( projname = 'SGutFeeling2',diagonalize = 0.5, doubleAveraging = 0 )
ans25 <- siena07( myalgorithm25, data = mydata25, effects = myeff25)
siena.table(ans25, type="html", sig=TRUE)

myalgorithm16 <- sienaAlgorithmCreate( projname = 'SEasilyDistracted1',diagonalize = 0.5, doubleAveraging = 0 )
ans16 <- siena07( myalgorithm16, data = mydata16, effects = myeff16)
siena.table(ans16, type="html", sig=TRUE)

myalgorithm26 <- sienaAlgorithmCreate( projname = 'SEasilyDistracted2',diagonalize = 0.5, doubleAveraging = 0 )
ans26 <- siena07( myalgorithm26, data = mydata26, effects = myeff26)
siena.table(ans26, type="html", sig=TRUE)

ans10
ans20
ans11
ans21
ans12
ans22
ans13
ans23
ans14
ans24
ans15
ans25
ans16
ans26
