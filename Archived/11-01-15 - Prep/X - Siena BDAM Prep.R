#Basic Trouble Relaxing Model Prep


  #Peer Influence Models    
  #Set Data for School 58
    #Waves 0-1
    #Set composition change
      comp58<-as.data.frame(ADHDFriends58M0$Missing)
      comp58$begin<-ifelse(ADHDFriends58M0$Missing==0,1.0,ifelse(ADHDFriends58M1$Missing==0,2.0,0))
      comp58$end<-ifelse(ADHDFriends58M1$Missing==1.0,1.0,2.0)
      comp58$'ADHDFriends58M0$Missing'<-NULL
      comp58.list <- as.list(as.data.frame(t(comp58)))
      changes <- sienaCompositionChange(comp58.list)
    #Create Networks
      FriendsArray58F <- array( c( S58FW0, S58FW1),
                               dim = c( 938, 938, 2 ) )
      friendship <- sienaDependent(FriendsArray58F)   #Siena Object
    #Behaviors
      DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58M0$DifficultyAttention,V2=ADHDFriends58M1$DifficultyAttention))
      TroubleRelaxing58<-as.matrix(data.frame(V1=ADHDFriends58M0$TroubleRelaxing,V2=ADHDFriends58M1$TroubleRelaxing))
      DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
      TroubleRelaxing <- sienaDependent( TroubleRelaxing58, type="behavior" )#Siena Object
    #Covariates
      #Prep
        FRFLAG58<-ADHDFriends58M1$FR_FLAG
        ADHDFriends58M1$sch<-as.numeric(0)
        SchoolDummy58<-ADHDFriends58M1$sch
        Age<-data.frame(V1=(94-ADHDFriends58M0$Age), V2=ADHDFriends58M1$Age)
        Age$Age<-ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))
      #Siena Objects
        FRFLAG<- coCovar(FRFLAG58)
        SchoolDummy<- coCovar(SchoolDummy58)
        Age<-coCovar(Age$Age)
    #Data
      mydata58F <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, SchoolDummy, changes) #Data - TroubleRelaxing removed

  #Waves 1-2
      #Set composition change
      comp58<-as.data.frame(ADHDFriends58ML1$Missing)
      comp58$begin<-ifelse(ADHDFriends58ML1$Missing==0,1.0,ifelse(ADHDFriends58M2$Missing==0,2.0,0))
      comp58$end<-ifelse(ADHDFriends58M2$Missing==1.0,1.0,2.0)
      comp58$'ADHDFriends58ML1$Missing'<-NULL
      comp58.list <- as.list(as.data.frame(t(comp58)))
      changes <- sienaCompositionChange(comp58.list)
      #Create Networks
      FriendsArray58 <- array( c( S58LW1,S58LW2),
                               dim = c( 808, 808, 2 ) )
      friendship <- sienaDependent(FriendsArray58)   #Siena Object
      #Behaviors
      DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58ML1$DifficultyAttention,V2=ADHDFriends58M2$DifficultyAttention))
      TroubleRelaxing58<-as.matrix(data.frame(V1=ADHDFriends58ML1$TroubleRelaxing,V2=ADHDFriends58M2$TroubleRelaxing))
      DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
      TroubleRelaxing <- sienaDependent( TroubleRelaxing58, type="behavior" )#Siena Object
      #Covariates
      #Prep
      FRFLAG58<-ADHDFriends58ML1$FR_FLAG
      ADHDFriends58ML1$sch<-as.numeric(0)
      SchoolDummy58<-ADHDFriends58ML1$sch
      Age<-data.frame(V1=ADHDFriends58ML1$Age, V2=ADHDFriends58M2$Age)
      Age$Age<-ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))
      #Siena Objects
      FRFLAG<- coCovar(FRFLAG58)
      SchoolDummy<- coCovar(SchoolDummy58)
      Age<-coCovar(Age$Age)
      #Data
      mydata58L <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, SchoolDummy, changes) #Data - TroubleRelaxing removed
      
            
  ##Basic Trouble Relaxing Model Prep
      
      
#Peer Influence Models    
  #Set Data for School 77
    #Waves 0-1
      #Set composition change
      comp77<-as.data.frame(ADHDFriends77M0$Missing)
      comp77$begin<-ifelse(ADHDFriends77M0$Missing==0,1.0,ifelse(ADHDFriends77M1$Missing==0,2.0,0))
      comp77$end<-ifelse(ADHDFriends77M1$Missing==1.0,1.0,2.0)
      comp77$'ADHDFriends77M0$Missing'<-NULL
      comp77.list <- as.list(as.data.frame(t(comp77)))
      changes <- sienaCompositionChange(comp77.list)
    #Create Networks
      FriendsArray77F <- array( c( S77FW0, S77FW1),
                                dim = c( 1894, 1894, 2 ) )
      friendship <- sienaDependent(FriendsArray77F)   #Siena Object
    #Behaviors
      DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77M0$DifficultyAttention,V2=ADHDFriends77M1$DifficultyAttention))
      TroubleRelaxing77<-as.matrix(data.frame(V1=ADHDFriends77M0$TroubleRelaxing,V2=ADHDFriends77M1$TroubleRelaxing))
      DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
      TroubleRelaxing <- sienaDependent( TroubleRelaxing77, type="behavior" )#Siena Object
  #Covariates
    #Prep
      FRFLAG77<-ADHDFriends77M1$FR_FLAG
      ADHDFriends77M1$sch<-as.numeric(0)
      SchoolDummy77<-ADHDFriends77M1$sch
      Age<-data.frame(V1=(94-ADHDFriends77M0$Age), V2=ADHDFriends77M1$Age)
      Age$Age<-ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))
    #Siena Objects
      FRFLAG<- coCovar(FRFLAG77)
      SchoolDummy<- coCovar(SchoolDummy77)
      Age<-coCovar(Age$Age)
    #Data
      mydata77F <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, SchoolDummy, changes) #Data - TroubleRelaxing removed
      
  #Waves 1-2
    #Set composition change
      comp77<-as.data.frame(ADHDFriends77ML1$Missing)
      comp77$begin<-ifelse(ADHDFriends77ML1$Missing==0,1.0,ifelse(ADHDFriends77M2$Missing==0,2.0,0))
      comp77$end<-ifelse(ADHDFriends77M2$Missing==1.0,1.0,2.0)
      comp77$'ADHDFriends77ML1$Missing'<-NULL
      comp77.list <- as.list(as.data.frame(t(comp77)))
      changes <- sienaCompositionChange(comp77.list)
    #Create Networks
      FriendsArray77 <- array( c( S77LW1,S77LW2),
                               dim = c(1574, 1574, 2 ) )
      friendship <- sienaDependent(FriendsArray77)   #Siena Object
    #Behaviors
      DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77ML1$DifficultyAttention,V2=ADHDFriends77M2$DifficultyAttention))
      TroubleRelaxing77<-as.matrix(data.frame(V1=ADHDFriends77ML1$TroubleRelaxing,V2=ADHDFriends77M2$TroubleRelaxing))
      DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
      TroubleRelaxing <- sienaDependent( TroubleRelaxing77, type="behavior" )#Siena Object
  #Covariates
    #Prep
      FRFLAG77<-ADHDFriends77ML1$FR_FLAG
      ADHDFriends77ML1$sch<-as.numeric(0)
      SchoolDummy77<-ADHDFriends77ML1$sch
      Age<-data.frame(V1=ADHDFriends77ML1$Age, V2=ADHDFriends77M2$Age)
      Age$Age<-ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))
    #Siena Objects
      FRFLAG<- coCovar(FRFLAG77)
      SchoolDummy<- coCovar(SchoolDummy77)
      Age<-coCovar(Age$Age)
    #Data
      mydata77L <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, SchoolDummy, changes) #Data - TroubleRelaxing removed
  
  #Combine and Save Data  
    saveRDS(mydata58F, 'BDAM58F.RDS')    
    saveRDS(mydata58L, 'BDAM58L.RDS')    
    saveRDS(mydata77F, 'BDAM77F.RDS')    
    saveRDS(mydata77L, 'BDAM77L.RDS')    
  #Clear Memory  
      rm(list=ls())  # clear memory
      gc()
      
    