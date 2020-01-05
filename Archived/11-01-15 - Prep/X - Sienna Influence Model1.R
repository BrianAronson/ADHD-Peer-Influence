  
  #Peer Influence Models    
  #Set Data for School 58
    #Set composition change
      comp58<-as.data.frame(ADHDFriends58M0$Missing)
      comp58$begin<-ifelse(ADHDFriends58M0$Missing==0,1.0,ifelse(ADHDFriends58M1$Missing==0,2.0,ifelse(ADHDFriends58M2$Missing==0,3.0,0)))
      comp58$end<-ifelse(ADHDFriends58M1$Missing==1.0,2.0,ifelse(ADHDFriends58M2$Missing==1.0,2.5,3.0))
      comp58$'ADHDFriends58M0$Missing'<-NULL
      comp58.list <- as.list(as.data.frame(t(comp58)))
      changes <- sienaCompositionChange(comp58.list)
    #Create Networks
      FriendsArray58 <- array( c( S58W0, S58W1,S58W2),
                               dim = c( 939, 939, 3 ) )
      friendship <- sienaDependent(FriendsArray58)   #Siena Object
    #Behaviors
      DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58M0$DifficultyAttention,V2=ADHDFriends58M1$DifficultyAttention, V3=ADHDFriends58M2$DifficultyAttention))
      TroubleRelaxing58<-as.matrix(data.frame(V1=ADHDFriends58M0$TroubleRelaxing,V2=ADHDFriends58M1$TroubleRelaxing, V3=ADHDFriends58M2$TroubleRelaxing))
      DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
      TroubleRelaxing <- sienaDependent( TroubleRelaxing58, type="behavior" )#Siena Object
    #Covariates
      #Prep
        FRFLAG58<-ADHDFriends58M1$FR_FLAG
        ADHDFriends58M1$sch<-as.numeric(0)
        SchoolDummy58<-ADHDFriends58M1$sch
        Age<-data.frame(V1=(94-ADHDFriends58M0$Age), V2=ADHDFriends58M1$Age, V3=ADHDFriends58M2$Age)
        Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA)))
      #Siena Objects
        FRFLAG<- coCovar(FRFLAG58)
        SchoolDummy<- coCovar(SchoolDummy58)
        Age<-coCovar(Age$Age)
    #Data
      mydata58 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, SchoolDummy, changes) #Data - TroubleRelaxing removed

      
  #Set Data for School 77
    #Set composition change
      comp77<-as.data.frame(ADHDFriends77M0$Missing)
      comp77$begin<-ifelse(ADHDFriends77M0$Missing==0,1.0,ifelse(ADHDFriends77M1$Missing==0,2.0,ifelse(ADHDFriends77M2$Missing==0,3.0,0)))
      comp77$end<-ifelse(ADHDFriends77M1$Missing==1.0,2.0,ifelse(ADHDFriends77M2$Missing==1.0,2.5,3.0))
      comp77$'ADHDFriends77M0$Missing'<-NULL
      comp77.list <- as.list(as.data.frame(t(comp77)))
      changes <- sienaCompositionChange(comp77.list)
    #Create Networks
      FriendsArray77 <- array( c( S77W0, S77W1,S77W2),
                               dim = c( 1907, 1907, 3 ) )
      friendship <- sienaDependent(FriendsArray77)   #Siena Object
    #Behaviors
      DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77M0$DifficultyAttention,V2=ADHDFriends77M1$DifficultyAttention, V3=ADHDFriends77M2$DifficultyAttention))
      TroubleRelaxing77<-as.matrix(data.frame(V1=ADHDFriends77M0$TroubleRelaxing,V2=ADHDFriends77M1$TroubleRelaxing, V3=ADHDFriends77M2$TroubleRelaxing))
      DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
      TroubleRelaxing <- sienaDependent( TroubleRelaxing77, type="behavior" )#Siena Object
    #Covariates
      #Prep
        FRFLAG77<-ADHDFriends77M1$FR_FLAG
        ADHDFriends77M1$sch<-as.numeric(1)
        SchoolDummy77<-ADHDFriends77M1$sch
        Age<-data.frame(V1=(94-ADHDFriends77M0$Age), V2=ADHDFriends77M1$Age, V3=ADHDFriends77M2$Age)
        Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA)))
      #Siena Objects
        FRFLAG<- coCovar(FRFLAG77)
        SchoolDummy<- coCovar(SchoolDummy77)
        Age<-coCovar(Age$Age)
      #Data
        mydata77 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, SchoolDummy, changes) #Data - TroubleRelaxing removed
  #Combine and Save Data  
    mydata <- sienaGroupCreate(list(mydata58,mydata77))
    saveRDS(mydata, 'Siena Model.RDS')    
  #Clear Memory  
    rm(list=ls())  # clear memory
    gc()
      
      
      
      
      
      
#Meta Analysis Strategy    
  #School 58
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
          

      
      
      
      
      
      
      
      
      
      
      
      
      
      
  #Combined with structural 0s
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
      
      