#Create simulation for wave 1 and 2 to measure behavior rate difference when peer influence is removed. 



library(RSiena)
library(gmodels)
library(parallel)
library(ggplot2)
#Set Directory
tempfile(tmpdir="")
setwd("")
mydata <- readRDS('Siena Model.RDS')

#SETUP DATA - This is the same as step 5 but does not include wave 3
    #load data
    overlap77.95<-readRDS('overlap77.95.RDS')
    overlap77.94<-readRDS('overlap77.94.RDS')
    overlap58.95<-readRDS('overlap58.95.RDS')
    overlap58.94<-readRDS('overlap58.94.RDS')
    
    #Peer Influence Models    
    #Set Data for School 58
    #Set composition change
    comp58<-as.data.frame(ADHDFriends58M0$Missing)
    comp58$begin<-ifelse(ADHDFriends58M0$Missing==0,1.0,ifelse(ADHDFriends58M1$Missing==0,2.0,ifelse(ADHDFriends58M2$Missing==0,3.0,0)))
    comp58$end<-ifelse(ADHDFriends58M1$Missing==1.0,1.0,ifelse(ADHDFriends58M2$Missing==1.0,2.0,2.0)) #CHANGED
    comp58$'ADHDFriends58M0$Missing'<-NULL
    comp58.list <- as.list(as.data.frame(t(comp58)))
    changes <- sienaCompositionChange(comp58.list)
    #Create Networks
    FriendsArray58 <- array( c( S58W0, S58W1),
                             dim = c( 736, 736, 2 ) ) #CHANGED
    friendship <- sienaDependent(FriendsArray58)   #Siena Object
    #Create Overlap covariate
    overlap <- coDyadCovar(overlap58.94) #CHANGED
    #Behaviors - CHANGED
    DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58M0$DifficultyAttention,V2=ADHDFriends58M1$DifficultyAttention))
    TroubleRelaxing58<-as.matrix(data.frame(V1=ADHDFriends58M0$TroubleRelaxing,V2=ADHDFriends58M1$TroubleRelaxing))
    TroubleRelaxing <- sienaDependent( TroubleRelaxing58, type="behavior" )#Siena Object
    DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
    #TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
    HighDifficultyAttention58<-ifelse(DifficultyAttention58>2,1,0)  
    HighDifficultyAttention58<-HighDifficultyAttention58[,1]#CHANGED
    HighDifficultyAttention <- coCovar(HighDifficultyAttention58)#Siena Object
    
    #Covariates
    #Prep
    FRFLAG58<-ADHDFriends58M1$FR_FLAG
    ADHDFriends58M1$sch<-as.numeric(0)
    SchoolDummy58<-ADHDFriends58M1$sch
    Age<-data.frame(V1=(94-ADHDFriends58M0$Age), V2=ADHDFriends58M1$Age, V3=ADHDFriends58M2$Age) 
    Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))) 
    Female<-ifelse(!is.na(ADHDFriends58M0$Sex),ADHDFriends58M0$Sex-1,ifelse(!is.na(ADHDFriends58M1$Sex),ADHDFriends58M1$Sex-1,ADHDFriends58M2$Sex-1))
    Race<-ifelse(ADHDFriends58M0$White==1,1,ifelse(ADHDFriends58M0$Black==1,2,ifelse(ADHDFriends58M0$Latino==1,3,ifelse(ADHDFriends58M0$Latino==1,4,ifelse(ADHDFriends58M0$Asian==1,5,6)))))
    Black<-ADHDFriends58M0$Black
    White<-ADHDFriends58M0$White
    Latino<-ADHDFriends58M0$Latino
    Latino<-ifelse(Latino!=1,0,1)
    Asian<-ADHDFriends58M0$Asian
    Grade<-ADHDFriends58M0$Grade
    GPA<-ADHDFriends58M0$GPA-1 #CHANGED
    Counseling<-ADHDFriends58M0$Counseling #CHANGED
    TeacherTrouble<-ADHDFriends58M0$TeacherTrouble #CHANGED
    
    #Siena Objects
    FRFLAG<- coCovar(FRFLAG58)
    SchoolDummy<- coCovar(SchoolDummy58)
    Age<-coCovar(Age$Age)
    Female<-coCovar(Female)
    White<-coCovar(White)
    Black<-coCovar(Black)
    Latino<-coCovar(Latino)
    Asian<-coCovar(Asian)
    Race<-coCovar(Race)
    Grade<-coCovar(Grade)
    GPA<-coCovar(GPA)
    Counseling<-coCovar(Counseling)
    TeacherTrouble<-coCovar(TeacherTrouble)
    
    #Data
    mydata58 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention)
    #mydata58.1 <- sienaDataCreate(friendship, TroubleRelaxing, FRFLAG, Age, Female, Black, Latino, Asian, Race, GPA, SchoolDummy, changes)

    #Set Data for School 77
    #Set composition change
    comp77<-as.data.frame(ADHDFriends77M0$Missing)
    comp77$begin<-ifelse(ADHDFriends77M0$Missing==0,1.0,ifelse(ADHDFriends77M1$Missing==0,2.0,ifelse(ADHDFriends77M2$Missing==0,3.0,0)))
    comp77$end<-ifelse(ADHDFriends77M1$Missing==1.0,1.0,ifelse(ADHDFriends77M2$Missing==1.0,2.0,2.0)) #CHANGED
    comp77$'ADHDFriends77M0$Missing'<-NULL
    comp77.list <- as.list(as.data.frame(t(comp77)))
    changes <- sienaCompositionChange(comp77.list)
    #Create Networks
    FriendsArray77 <- array( c( S77W0, S77W1),
                             dim = c( 1467, 1467, 2 ) ) #CHANGED
    friendship <- sienaDependent(FriendsArray77)   #Siena Object
    #Create Overlap covariate
    overlap <- coDyadCovar(overlap77.94) #CHANGED
    #Behaviors - CHANGED
    DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77M0$DifficultyAttention,V2=ADHDFriends77M1$DifficultyAttention))
    TroubleRelaxing77<-as.matrix(data.frame(V1=ADHDFriends77M0$TroubleRelaxing,V2=ADHDFriends77M1$TroubleRelaxing))
    TroubleRelaxing <- sienaDependent( TroubleRelaxing77, type="behavior" )#Siena Object
    DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
    #TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
    HighDifficultyAttention77<-ifelse(DifficultyAttention77>2,1,0)  
    HighDifficultyAttention77<-HighDifficultyAttention77[,1]#CHANGED
    HighDifficultyAttention <- coCovar(HighDifficultyAttention77)#Siena Object
    
    #Covariates
    #Prep
    FRFLAG77<-ADHDFriends77M1$FR_FLAG
    ADHDFriends77M1$sch<-as.numeric(0)
    SchoolDummy77<-ADHDFriends77M1$sch
    Age<-data.frame(V1=(94-ADHDFriends77M0$Age), V2=ADHDFriends77M1$Age, V3=ADHDFriends77M2$Age) 
    Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))) 
    Female<-ifelse(!is.na(ADHDFriends77M0$Sex),ADHDFriends77M0$Sex-1,ifelse(!is.na(ADHDFriends77M1$Sex),ADHDFriends77M1$Sex-1,ADHDFriends77M2$Sex-1))
    Race<-ifelse(ADHDFriends77M0$White==1,1,ifelse(ADHDFriends77M0$Black==1,2,ifelse(ADHDFriends77M0$Latino==1,3,ifelse(ADHDFriends77M0$Latino==1,4,ifelse(ADHDFriends77M0$Asian==1,5,6)))))
    Black<-ADHDFriends77M0$Black
    White<-ADHDFriends77M0$White
    Latino<-ADHDFriends77M0$Latino
    Latino<-ifelse(Latino!=1,0,1)
    Asian<-ADHDFriends77M0$Asian
    Grade<-ADHDFriends77M0$Grade
    GPA<-ADHDFriends77M0$GPA-1 #CHANGED
    Counseling<-ADHDFriends77M0$Counseling #CHANGED
    TeacherTrouble<-ADHDFriends77M0$TeacherTrouble #CHANGED
    
    #Siena Objects
    FRFLAG<- coCovar(FRFLAG77)
    SchoolDummy<- coCovar(SchoolDummy77)
    Age<-coCovar(Age$Age)
    Female<-coCovar(Female)
    White<-coCovar(White)
    Black<-coCovar(Black)
    Latino<-coCovar(Latino)
    Asian<-coCovar(Asian)
    Race<-coCovar(Race)
    Grade<-coCovar(Grade)
    GPA<-coCovar(GPA)
    Counseling<-coCovar(Counseling)
    TeacherTrouble<-coCovar(TeacherTrouble)
    
    #Data
    mydata77 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention)

    #Combine and Save Data  
    mydata <- sienaGroupCreate(list(mydata58,mydata77))
    #mydata.1 <- sienaGroupCreate(list(mydata58.1,mydata77.1))
    saveRDS(mydata, 'Simulation Model.RDS')    
    #Clear Memory  
    rm(list=ls())  # clear memory
    gc()

    
#RUN SIMULATION FOR FIRST two Waves
    #Difficulty paying attention
    mydata <- readRDS('Simulation Model.RDS')
    #Load siena estimates to use for new model
    ansCF1 <- readRDS('SM CDAM FULL1.RDS')
    ansCF1$theta
    ansCF1$effects$effectName
    #Create data frame with Effect names and thetas
    initvalue<-data.frame(Effect=ansCF1$effects$effectName, Value=ansCF1$theta)
    #Set  effects sequentially
    #print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
    #Base effects
    myeff <- getEffects( mydata )
    
    #Rate effects
    myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, initialValue=initvalue[1,2])
    myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, group=2, initialValue=initvalue[3,2])
    #outdegree
    myeff <- setEffect(myeff, density, fix=TRUE, test=FALSE, initialValue=initvalue[5,2])
    #reciprocity
    myeff <- setEffect(myeff, recip, fix=TRUE, test=FALSE, initialValue=initvalue[6,2])
    #transitive triplets
    myeff <- setEffect(myeff, transTrip, fix=TRUE, test=FALSE, initialValue=initvalue[7,2])
    #indegree (sqrt)
    myeff <- setEffect(myeff, inPopSqrt, fix=TRUE, test=FALSE, initialValue=initvalue[8,2])
    #overlap
    myeff <- setEffect(myeff, X, interaction1='overlap', fix=TRUE, test=FALSE, initialValue=initvalue[9,2])
    #Inattention alter, ego, similarity
    myeff <- setEffect(myeff, altX, interaction1="DifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[10,2])
    myeff <- setEffect(myeff, egoX, interaction1="DifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[11,2])
    myeff <- setEffect(myeff, simX, interaction1="DifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[12,2])
    #GPA similarity, Teacher Trouble similarity, same highdifficulty attention
    myeff <- setEffect(myeff, simX, interaction1="GPA", fix=TRUE, test=FALSE, initialValue=initvalue[13,2])
    myeff <- setEffect(myeff, simX, interaction1="TeacherTrouble", fix=TRUE, test=FALSE, initialValue=initvalue[14,2])
    myeff <- setEffect(myeff, sameX, interaction1="HighDifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[15,2])
    #FR EGO, same female, same grade, same race, school ego
    myeff <- setEffect(myeff, egoX, interaction1="FRFLAG", fix=TRUE, test=FALSE, initialValue=initvalue[16,2])
    myeff <- setEffect(myeff, sameX, interaction1="Female", fix=TRUE, test=FALSE, initialValue=initvalue[17,2])
    myeff <- setEffect(myeff, sameX, interaction1="Grade", fix=TRUE, test=FALSE, initialValue=initvalue[18,2])
    myeff <- setEffect(myeff, sameX, interaction1="Race", fix=TRUE, test=FALSE, initialValue=initvalue[19,2])
    myeff <- setEffect(myeff, egoX, interaction1="SchoolDummy", fix=TRUE, test=FALSE, initialValue=initvalue[20,2])
    #interactions - this is ugly, but the only way I know how to do it. setEffect doesn't seem to work with interactions. Luckily, interactions are entered into the dataset in the order you put them 
    myeff <- includeInteraction(myeff, X, sameX, interaction1 = c('overlap','HighDifficultyAttention'))
    myeff <- includeInteraction(myeff, sameX, egoX,interaction1 = c('Race', 'SchoolDummy')) #school on race selection
    myeff <- includeInteraction(myeff, X, egoX, interaction1 = c('overlap','SchoolDummy'))
    myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,]$fix<-TRUE
    myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,]$test<-FALSE
    myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,][1,]$initialValue<-initvalue[21,2]
    myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,][2,]$initialValue<-initvalue[22,2]
    myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,][3,]$initialValue<-initvalue[23,2]
  #Behavior effects
    #rate effects
    myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, initialValue=initvalue[24,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, initialValue=initvalue[26,2],name='DifficultyAttention', group=2)
    #shape effects
    myeff <- setEffect(myeff, linear, fix=TRUE, test=FALSE, initialValue=initvalue[28,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, quad, fix=TRUE, test=FALSE, initialValue=initvalue[29,2],name='DifficultyAttention')
    #attention sim, GPA, Counseling, Teacher,female, white
    myeff <- setEffect(myeff, avSim,interaction1='friendship', fix=TRUE, test=FALSE, initialValue=initvalue[30,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, effFrom,interaction1='GPA', fix=TRUE, test=FALSE, initialValue=initvalue[31,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, effFrom,interaction1='Counseling', fix=TRUE, test=FALSE, initialValue=initvalue[32,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, effFrom,interaction1='TeacherTrouble', fix=TRUE, test=FALSE, initialValue=initvalue[33,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, effFrom,interaction1='Female', fix=TRUE, test=FALSE, initialValue=initvalue[35,2],name='DifficultyAttention')
    myeff <- setEffect(myeff, effFrom,interaction1='White', fix=TRUE, test=FALSE, initialValue=initvalue[36,2],name='DifficultyAttention')
    #alter teacher
    myeff <- setEffect(myeff, AltsAvAlt, interaction1='TeacherTrouble', interaction2='friendship', name='DifficultyAttention',fix=TRUE, test=FALSE, initialValue=initvalue[34,2])
      #replace with AltsAvAlt on server

    #Final Model
    myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=0, n3=1000, simOnly=TRUE, cond=FALSE) #Final Algorithm
    #Run and Save
    ansSim1 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T,returnDeps=TRUE)  #Compute
    siena.table(ansSim1, type="html", sig=TRUE)  #Save Results
    ansSim1  #Show Results
    saveRDS(ansSim1, 'ansSim1.RDS')    
    
#Simulate model without peer influence
  #remove peer influence from previous model
    myeff <- setEffect(myeff, avSim,interaction1='friendship', fix=TRUE, test=FALSE, initialValue=initvalue[30,2],name='DifficultyAttention', include=FALSE)
    #Final Model
    myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=0, n3=1000, simOnly=TRUE, cond=TRUE) #Final Algorithm
    #Run and Save
    ansSim2 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T,returnDeps=TRUE)  #Compute
    siena.table(ansSim2, type="html", sig=TRUE)  #Save Results
    ansSim2  #Show Results
    saveRDS(ansSim2, 'ansSim2.RDS')    

#Stats    
    #Look at simulated results
    ansSim1<-readRDS('ansSim1.RDS') #read data
    ansSim2<-readRDS('ansSim2.RDS') #read data
    
    #sims refers to simulated data, the first brackets chooses simulation, second bracket chooses group (i.e. school), third bracket chooses dependent variable (i.e. behavior rather than network) 
    ansSim1$sims[[1]][[1]][[2]]
    #create data frame for all simulated results of school 58
    Sim58Results<-data.frame(matrix(NA,nrow = 736, ncol = 1002)) #empty dataframe
    Sim77Results<-data.frame(matrix(NA,nrow = 1467, ncol = 1002)) #empty dataframe
    for (i in 1:1002){
      Sim58Results[,i]<-ansSim1$sims[[i]][[1]][[2]]
      Sim77Results[,i]<-ansSim1$sims[[i]][[2]][[2]]
    }
    #create data frame for all simulated results of school 58
    Sim58Results2<-data.frame(matrix(NA,nrow = 736, ncol = 1002)) #empty dataframe
    Sim77Results2<-data.frame(matrix(NA,nrow = 1467, ncol = 1002)) #empty dataframe
    for (i in 1:1002){
      Sim58Results2[,i]<-ansSim2$sims[[i]][[1]][[2]]
      Sim77Results2[,i]<-ansSim2$sims[[i]][[2]][[2]]
    }
    
    #Convert sim results to vectors
    Sim58v2<-as.vector(as.matrix(Sim58Results2))
    Sim77v2<-as.vector(as.matrix(Sim77Results2))
    Sim58v1<-as.vector(as.matrix(Sim58Results))
    Sim77v1<-as.vector(as.matrix(Sim77Results))
    Sim1v<-append(Sim58v1,Sim77v1)
    Sim2v<-append(Sim58v2,Sim77v2)
    #Estimate mean inattention
    mean(Sim58v1)
    mean(Sim58v2)
    mean(Sim77v1)
    mean(Sim77v2)
    mean(Sim1v)
    mean(Sim2v)
    #Estimate rate of high inattention
    Sim58Count2<-sum(Sim58Results2>2, na.rm = TRUE)/1002
    Sim77Count2<-sum(Sim77Results2>2, na.rm = TRUE)/1002
    Sim58Count1<-sum(Sim58Results>2, na.rm = TRUE)/1002
    Sim77Count1<-sum(Sim77Results>2, na.rm = TRUE)/1002
    SimCount2<-sum(Sim58Results2>2,Sim77Results2>2)/1002
    SimCount1<-sum(Sim58Results>2,Sim77Results>2)/1002
    table(Sim77Results2$X1002)
    
    table(round(rowMeans(Sim58Results2)))
    table(round(rowMeans(Sim77Results2)))
    
    table(round(rowMeans(Sim58Results)))
    table(round(rowMeans(Sim77Results)))
#Results demonstrate my point, but also show how imperfect my model fit is. Let's try to simulate wave 3.
#Simulate full model for wave 3.
  #Now repeat most of everything for simulation 1
      #SETUP DATA - This is the same as step 5 but does not include wave 3
      #load data
      overlap77.95<-readRDS('overlap77.95.RDS')
      overlap77.94<-readRDS('overlap77.94.RDS')
      overlap58.95<-readRDS('overlap58.95.RDS')
      overlap58.94<-readRDS('overlap58.94.RDS')
    
#NEW STEPs
  #Create updated behavior variables for wave 2 based on final simulation. Can't use averages since it skews everything towards the center.
  #VERY IMPORTANT - CHANGE DATAFRAME TO GRAB FROM IN ORDER TO CHANGE WHICH SIMULATION I USE. SIMULATION 3 SHOULD BE BASED ON SimXXResults, NOT SimXXResults2!
    ADHDFriends58M1$DifficultyAttention<-Sim58Results$X1002
  #DELETE LEAVERS AT WAVE 1
    Leavers<-ifelse(ADHDFriends58M1$Missing==1,1,0)
    ADHDFriends58M0<-ADHDFriends58M0[Leavers==0,]
    ADHDFriends58M1<-ADHDFriends58M1[Leavers==0,]
    ADHDFriends58M2<-ADHDFriends58M2[Leavers==0,]
    S58W1<-S58W1[Leavers==0,Leavers==0]
    S58W2<-S58W2[Leavers==0,Leavers==0]
    overlap58.95<-overlap58.95[Leavers==0,Leavers==0]
  #Minor changes throughout the rest    
      comp58<-as.data.frame(ADHDFriends58M0$Missing)
      comp58$begin<-1.0 #CHANGED from 1st sim
      comp58$end<-ifelse(ADHDFriends58M1$Missing==1.0,0,ifelse(ADHDFriends58M2$Missing==1.0,1.0,2.0)) #CHANGED from 1st sim
      comp58$'ADHDFriends58M0$Missing'<-NULL
      comp58.list <- as.list(as.data.frame(t(comp58)))
      changes <- sienaCompositionChange(comp58.list)
      #Create Networks
      FriendsArray58 <- array( c( S58W1, S58W2),
                               dim = c( 615, 615, 2 ) ) #CHANGED
      friendship <- sienaDependent(FriendsArray58)   #Siena Object
      #Create Overlap covariate
      overlap <- coDyadCovar(overlap58.95) #CHANGED
      #Behaviors - CHANGED
      DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58M1$DifficultyAttention,V2=ADHDFriends58M2$DifficultyAttention))
      TroubleRelaxing58<-as.matrix(data.frame(V1=ADHDFriends58M1$TroubleRelaxing,V2=ADHDFriends58M2$TroubleRelaxing))
      TroubleRelaxing <- sienaDependent( TroubleRelaxing58, type="behavior" )#Siena Object
      DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
      #TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
      HighDifficultyAttention58<-ifelse(DifficultyAttention58>2,1,0)  
      HighDifficultyAttention58<-HighDifficultyAttention58[,2]#CHANGED
      HighDifficultyAttention <- coCovar(HighDifficultyAttention58)#Siena Object
      
      #Covariates
      #Prep
      FRFLAG58<-ADHDFriends58M1$FR_FLAG
      ADHDFriends58M1$sch<-as.numeric(0)
      SchoolDummy58<-ADHDFriends58M1$sch
      Age<-data.frame(V1=(94-ADHDFriends58M0$Age), V2=ADHDFriends58M1$Age, V3=ADHDFriends58M2$Age) 
      Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))) 
      Female<-ifelse(!is.na(ADHDFriends58M0$Sex),ADHDFriends58M0$Sex-1,ifelse(!is.na(ADHDFriends58M1$Sex),ADHDFriends58M1$Sex-1,ADHDFriends58M2$Sex-1))
      Race<-ifelse(ADHDFriends58M0$White==1,1,ifelse(ADHDFriends58M0$Black==1,2,ifelse(ADHDFriends58M0$Latino==1,3,ifelse(ADHDFriends58M0$Latino==1,4,ifelse(ADHDFriends58M0$Asian==1,5,6)))))
      Black<-ADHDFriends58M0$Black
      White<-ADHDFriends58M0$White
      Latino<-ADHDFriends58M0$Latino
      Latino<-ifelse(Latino!=1,0,1)
      Asian<-ADHDFriends58M0$Asian
      Grade<-ADHDFriends58M0$Grade
      GPA<-ADHDFriends58M1$GPA-1 #CHANGED
      Counseling<-ADHDFriends58M1$Counseling #CHANGED
      TeacherTrouble<-ADHDFriends58M1$TeacherTrouble #CHANGED
      
      #Siena Objects
      FRFLAG<- coCovar(FRFLAG58)
      SchoolDummy<- coCovar(SchoolDummy58)
      Age<-coCovar(Age$Age)
      Female<-coCovar(Female)
      White<-coCovar(White)
      Black<-coCovar(Black)
      Latino<-coCovar(Latino)
      Asian<-coCovar(Asian)
      Race<-coCovar(Race)
      Grade<-coCovar(Grade)
      GPA<-coCovar(GPA)
      Counseling<-coCovar(Counseling)
      TeacherTrouble<-coCovar(TeacherTrouble)
      
      #Data
      mydata58 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention)
      #mydata58.1 <- sienaDataCreate(friendship, TroubleRelaxing, FRFLAG, Age, Female, Black, Latino, Asian, Race, GPA, SchoolDummy, changes)
      
      
      #Create updated behavior variables for wave 2 based on final simulation. Can't use averages since it skews everything towards the center.
#VERY IMPORTANT - CHANGE DATAFRAME TO GRAB FROM IN ORDER TO CHANGE WHICH SIMULATION I USE. SIMULATION 3 SHOULD BE BASED ON SimXXResults, NOT SimXXResults2!
      ADHDFriends77M1$DifficultyAttention<-Sim77Results$X1002
      #DELETE LEAVERS AT WAVE 1
      Leavers<-ifelse(ADHDFriends77M1$Missing==1,1,0)
      ADHDFriends77M0<-ADHDFriends77M0[Leavers==0,]
      ADHDFriends77M1<-ADHDFriends77M1[Leavers==0,]
      ADHDFriends77M2<-ADHDFriends77M2[Leavers==0,]
      S77W1<-S77W1[Leavers==0,Leavers==0]
      S77W2<-S77W2[Leavers==0,Leavers==0]
      overlap77.95<-overlap77.95[Leavers==0,Leavers==0]
      #Minor changes throughout the rest    
      comp77<-as.data.frame(ADHDFriends77M0$Missing)
      comp77$begin<-1.0 #CHANGED from 1st sim
      comp77$end<-ifelse(ADHDFriends77M1$Missing==1.0,0,ifelse(ADHDFriends77M2$Missing==1.0,1.0,2.0)) #CHANGED from 1st sim
      comp77$'ADHDFriends77M0$Missing'<-NULL
      comp77.list <- as.list(as.data.frame(t(comp77)))
      changes <- sienaCompositionChange(comp77.list)
      #Create Networks
      FriendsArray77 <- array( c( S77W1, S77W2),
                               dim = c( 1212, 1212, 2 ) ) #CHANGED
      friendship <- sienaDependent(FriendsArray77)   #Siena Object
      #Create Overlap covariate
      overlap <- coDyadCovar(overlap77.95) #CHANGED
      #Behaviors - CHANGED
      DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77M1$DifficultyAttention,V2=ADHDFriends77M2$DifficultyAttention))
      TroubleRelaxing77<-as.matrix(data.frame(V1=ADHDFriends77M1$TroubleRelaxing,V2=ADHDFriends77M2$TroubleRelaxing))
      TroubleRelaxing <- sienaDependent( TroubleRelaxing77, type="behavior" )#Siena Object
      DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
      #TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
      HighDifficultyAttention77<-ifelse(DifficultyAttention77>2,1,0)  
      HighDifficultyAttention77<-HighDifficultyAttention77[,2]#CHANGED
      HighDifficultyAttention <- coCovar(HighDifficultyAttention77)#Siena Object
      
      #Covariates
      #Prep
      FRFLAG77<-ADHDFriends77M1$FR_FLAG
      ADHDFriends77M1$sch<-as.numeric(0)
      SchoolDummy77<-ADHDFriends77M1$sch
      Age<-data.frame(V1=(94-ADHDFriends77M0$Age), V2=ADHDFriends77M1$Age, V3=ADHDFriends77M2$Age) 
      Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))) 
      Female<-ifelse(!is.na(ADHDFriends77M0$Sex),ADHDFriends77M0$Sex-1,ifelse(!is.na(ADHDFriends77M1$Sex),ADHDFriends77M1$Sex-1,ADHDFriends77M2$Sex-1))
      Race<-ifelse(ADHDFriends77M0$White==1,1,ifelse(ADHDFriends77M0$Black==1,2,ifelse(ADHDFriends77M0$Latino==1,3,ifelse(ADHDFriends77M0$Latino==1,4,ifelse(ADHDFriends77M0$Asian==1,5,6)))))
      Black<-ADHDFriends77M0$Black
      White<-ADHDFriends77M0$White
      Latino<-ADHDFriends77M0$Latino
      Latino<-ifelse(Latino!=1,0,1)
      Asian<-ADHDFriends77M0$Asian
      Grade<-ADHDFriends77M0$Grade
      GPA<-ADHDFriends77M1$GPA-1 #CHANGED
      Counseling<-ADHDFriends77M1$Counseling #CHANGED
      TeacherTrouble<-ADHDFriends77M1$TeacherTrouble #CHANGED
      
      #Siena Objects
      FRFLAG<- coCovar(FRFLAG77)
      SchoolDummy<- coCovar(SchoolDummy77)
      Age<-coCovar(Age$Age)
      Female<-coCovar(Female)
      White<-coCovar(White)
      Black<-coCovar(Black)
      Latino<-coCovar(Latino)
      Asian<-coCovar(Asian)
      Race<-coCovar(Race)
      Grade<-coCovar(Grade)
      GPA<-coCovar(GPA)
      Counseling<-coCovar(Counseling)
      TeacherTrouble<-coCovar(TeacherTrouble)
      
      #Data
      mydata77 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention)
      
      
      #Combine and Save Data  
      mydata <- sienaGroupCreate(list(mydata58,mydata77))
      #mydata.1 <- sienaGroupCreate(list(mydata58.1,mydata77.1))
      saveRDS(mydata, 'Simulation Model 2.RDS')    
      #Clear Memory  
      rm(list=ls())  # clear memory
      gc()
      
      
      #RUN SIMULATION FOR SECOND two Waves
      #Difficulty paying attention
      mydata <- readRDS('Simulation Model 2.RDS')
      #Load siena estimates to use for new model
      ansCF1 <- readRDS('SM CDAM FULL1.RDS')
      ansCF1$theta
      ansCF1$effects$effectName
      #Create data frame with Effect names and thetas
      initvalue<-data.frame(Effect=ansCF1$effects$effectName, Value=ansCF1$theta)
      #Set  effects sequentially
      #print01Report(mydata, myeff, modelname = 'BDAM58F Basic Stats')
      #Base effects
      myeff <- getEffects( mydata )
      
      #Rate effects
      myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, initialValue=initvalue[2,2])
      myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, group=2, initialValue=initvalue[4,2])
      #outdegree
      myeff <- setEffect(myeff, density, fix=TRUE, test=FALSE, initialValue=initvalue[5,2])
      #reciprocity
      myeff <- setEffect(myeff, recip, fix=TRUE, test=FALSE, initialValue=initvalue[6,2])
      #transitive triplets
      myeff <- setEffect(myeff, transTrip, fix=TRUE, test=FALSE, initialValue=initvalue[7,2])
      #indegree (sqrt)
      myeff <- setEffect(myeff, inPopSqrt, fix=TRUE, test=FALSE, initialValue=initvalue[8,2])
      #overlap
      myeff <- setEffect(myeff, X, interaction1='overlap', fix=TRUE, test=FALSE, initialValue=initvalue[9,2])
      #Inattention alter, ego, similarity
      myeff <- setEffect(myeff, altX, interaction1="DifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[10,2])
      myeff <- setEffect(myeff, egoX, interaction1="DifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[11,2])
      myeff <- setEffect(myeff, simX, interaction1="DifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[12,2])
      #GPA similarity, Teacher Trouble similarity, same highdifficulty attention
      myeff <- setEffect(myeff, simX, interaction1="GPA", fix=TRUE, test=FALSE, initialValue=initvalue[13,2])
      myeff <- setEffect(myeff, simX, interaction1="TeacherTrouble", fix=TRUE, test=FALSE, initialValue=initvalue[14,2])
      myeff <- setEffect(myeff, sameX, interaction1="HighDifficultyAttention", fix=TRUE, test=FALSE, initialValue=initvalue[15,2])
      #FR EGO, same female, same grade, same race, school ego
      myeff <- setEffect(myeff, egoX, interaction1="FRFLAG", fix=TRUE, test=FALSE, initialValue=initvalue[16,2])
      myeff <- setEffect(myeff, sameX, interaction1="Female", fix=TRUE, test=FALSE, initialValue=initvalue[17,2])
      myeff <- setEffect(myeff, sameX, interaction1="Grade", fix=TRUE, test=FALSE, initialValue=initvalue[18,2])
      myeff <- setEffect(myeff, sameX, interaction1="Race", fix=TRUE, test=FALSE, initialValue=initvalue[19,2])
      myeff <- setEffect(myeff, egoX, interaction1="SchoolDummy", fix=TRUE, test=FALSE, initialValue=initvalue[20,2])
      #interactions - this is ugly, but the only way I know how to do it. setEffect doesn't seem to work with interactions. Luckily, interactions are entered into the dataset in the order you put them 
      myeff <- includeInteraction(myeff, X, sameX, interaction1 = c('overlap','HighDifficultyAttention'))
      myeff <- includeInteraction(myeff, sameX, egoX,interaction1 = c('Race', 'SchoolDummy')) #school on race selection
      myeff <- includeInteraction(myeff, X, egoX, interaction1 = c('overlap','SchoolDummy'))
      myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,]$fix<-TRUE
      myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,]$test<-FALSE
      myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,][1,]$initialValue<-initvalue[21,2]
      myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,][2,]$initialValue<-initvalue[22,2]
      myeff[myeff$shortName=='unspInt'&myeff$include==TRUE,][3,]$initialValue<-initvalue[23,2]
      #Behavior effects
      #rate effects
      myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, initialValue=initvalue[25,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, Rate, type='rate', fix=TRUE, test=FALSE, initialValue=initvalue[27,2],name='DifficultyAttention', group=2)
      #shape effects
      myeff <- setEffect(myeff, linear, fix=TRUE, test=FALSE, initialValue=initvalue[28,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, quad, fix=TRUE, test=FALSE, initialValue=initvalue[29,2],name='DifficultyAttention')
      #attention sim, GPA, Counseling, Teacher,female, white
      myeff <- setEffect(myeff, avSim,interaction1='friendship', fix=TRUE, test=FALSE, initialValue=initvalue[30,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, effFrom,interaction1='GPA', fix=TRUE, test=FALSE, initialValue=initvalue[31,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, effFrom,interaction1='Counseling', fix=TRUE, test=FALSE, initialValue=initvalue[32,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, effFrom,interaction1='TeacherTrouble', fix=TRUE, test=FALSE, initialValue=initvalue[33,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, effFrom,interaction1='Female', fix=TRUE, test=FALSE, initialValue=initvalue[35,2],name='DifficultyAttention')
      myeff <- setEffect(myeff, effFrom,interaction1='White', fix=TRUE, test=FALSE, initialValue=initvalue[36,2],name='DifficultyAttention')
      #alter teacher
      myeff <- setEffect(myeff, AltsAvAlt, interaction1='TeacherTrouble', interaction2='friendship', name='DifficultyAttention',fix=TRUE, test=FALSE, initialValue=initvalue[34,2])
      #replace with AltsAvAlt on server

#ALSO EXTREMELY IMPORTANT - INCLUDE EFFECT AND SWITCH SIMULATION NAME ACCORDING TO WHICH SIMULATION YOU ARE RUNNING
      #remove peer influence from previous model
      #myeff <- setEffect(myeff, avSim,interaction1='friendship', fix=TRUE, test=FALSE, initialValue=initvalue[30,2],name='DifficultyAttention', include=FALSE)
      
      #Final Model
      myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=0, n3=1000, simOnly=TRUE, cond=FALSE) #Final Algorithm
      #Run and Save
      ansSim3 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T,returnDeps=TRUE)  #Compute
      siena.table(ansSim3, type="html", sig=TRUE)  #Save Results
      ansSim3  #Show Results
      saveRDS(ansSim3, 'ansSim3.RDS')    
    

      
      
      
      #Stats    
      #Load at simulated results
      ansSim3<-readRDS('ansSim3.RDS') #read data
      ansSim4<-readRDS('ansSim4.RDS') #read data
      #create data frame for all simulated results of school 58
      Sim58Results3<-data.frame(matrix(NA,nrow = 615, ncol = 1002)) #empty dataframe
      Sim77Results3<-data.frame(matrix(NA,nrow = 1212, ncol = 1002)) #empty dataframe
      for (i in 1:1002){
        Sim58Results3[,i]<-ansSim3$sims[[i]][[1]][[2]]
        Sim77Results3[,i]<-ansSim3$sims[[i]][[2]][[2]]
      }
      Sim58Results4<-data.frame(matrix(NA,nrow = 615, ncol = 1002)) #empty dataframe
      Sim77Results4<-data.frame(matrix(NA,nrow = 1212, ncol = 1002)) #empty dataframe
      for (i in 1:1002){
        Sim58Results4[,i]<-ansSim4$sims[[i]][[1]][[2]]
        Sim77Results4[,i]<-ansSim4$sims[[i]][[2]][[2]]
      }
      #Convert sim results to vectors
      Sim58v4<-as.vector(as.matrix(Sim58Results4))
      Sim77v4<-as.vector(as.matrix(Sim77Results4))
      Sim58v3<-as.vector(as.matrix(Sim58Results3))
      Sim77v3<-as.vector(as.matrix(Sim77Results3))
      Sim3v<-append(Sim58v3,Sim77v3)
      Sim4v<-append(Sim58v4,Sim77v4)
      #Estimate mean inattention
      mean(Sim58v3)
      mean(Sim58v4)
      mean(Sim77v3)
      mean(Sim77v4)
      mean(Sim3v)
      mean(Sim4v)

      #Estimate rate of high inattention
      Sim58Count4<-sum(Sim58Results4>2, na.rm = TRUE)/1002
      Sim77Count4<-sum(Sim77Results4>2, na.rm = TRUE)/1002
      Sim58Count3<-sum(Sim58Results3>2, na.rm = TRUE)/1002
      Sim77Count3<-sum(Sim77Results3>2, na.rm = TRUE)/1002
      SimCount4<-sum(Sim58Results4>2,Sim77Results2>2)/1002
      SimCount3<-sum(Sim58Results3>2,Sim77Results>2)/1002
      

#Visualizing
  #first round of simulations
    #Create dataframe with two columns of the two simulations
      VizSim1<-data.frame(Inattention=Sim1v,Simulation=1)
      VizSim1<-rbind(VizSim1,data.frame(Inattention=Sim2v,Simulation=2))
      
      p <- ggplot(VizSim1, aes(factor(Simulation), Inattention))
      p + geom_boxplot() #I don't like it. It shows off that there is a lot of variation. I need something simpler
    #Simple line graph with dots at points
      VizSim1<-data.frame(matrix(NA, nrow = 9, ncol = 4))
      colnames(VizSim1)<-c("Inattention","HInattention","Simulation","Wave")
      VizSim1$Wave<-c(1,2,3)
      Innattention<-rbind(DifficultyAttention58,DifficultyAttention77)
      HInnattention<-rbind(HighDifficultyAttention58,HighDifficultyAttention77)
      w1I<-mean(Innattention[,1],na.rm = TRUE)
      w2I<-mean(Innattention[,2],na.rm = TRUE)
      w3I<-mean(Innattention[,3],na.rm = TRUE)
      w1HI<-sum(HInnattention[,1],na.rm = TRUE)
      w2HI<-sum(HInnattention[,2],na.rm = TRUE)
      w3HI<-sum(HInnattention[,3],na.rm = TRUE)
      VizSim1$Inattention<-c(w1I,w2I,w3I,w1I,mean(Sim1v),mean(Sim3v),w1I,mean(Sim2v),mean(Sim4v))
      VizSim1$HInattention<-c(w1HI,w2HI,w3HI,w1HI,SimCount1,SimCount3,w1HI,SimCount2,SimCount4)
      VizSim1$Simulation<-c("Observed","Observed","Observed","Simulation 1","Simulation 1","Simulation 1","Simulation 2","Simulation 2","Simulation 2")
      
      #I don't like showing 3 waves. Shows too big a drop off in inattention, and doesn't add much information.
      VizSim1<-VizSim1[VizSim1$Wave!=1,]
      
      p <- ggplot(VizSim1, aes(x=Wave, group=Simulation, y=Inattention))
      p + geom_line()+geom_point()
      
      p <- ggplot(VizSim1, aes(x=Wave, group=Simulation, y=HInattention))
      p + geom_line()+geom_point()
      