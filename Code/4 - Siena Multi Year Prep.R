overlap77.95<-readRDS('overlap77.95.RDS')
overlap77.94<-readRDS('overlap77.94.RDS')
overlap58.95<-readRDS('overlap58.95.RDS')
overlap58.94<-readRDS('overlap58.94.RDS')

  #Peer Influence Models    
  #Set Data for School 58
    #Set composition change
      comp58<-as.data.frame(ADHDFriends58M0$Missing)
      comp58$begin<-ifelse(ADHDFriends58M0$Missing==0,1.0,ifelse(ADHDFriends58M1$Missing==0,2.0,ifelse(ADHDFriends58M2$Missing==0,3.0,0)))
      comp58$end<-ifelse(ADHDFriends58M1$Missing==1.0,1.0,ifelse(ADHDFriends58M2$Missing==1.0,2.0,3.0))
      comp58$'ADHDFriends58M0$Missing'<-NULL
      comp58.list <- as.list(as.data.frame(t(comp58)))
      changes <- sienaCompositionChange(comp58.list)
    #Create Networks
      FriendsArray58 <- array( c( S58W0, S58W1,S58W2),
                               dim = c( 736, 736, 3 ) )
      friendship <- sienaDependent(FriendsArray58)   #Siena Object
    #Create Overlap covariate
      overlap <- varDyadCovar(array(c(overlap58.94,overlap58.95), dim=c(736, 736, 2)))
    #Behaviors
      DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58M0$DifficultyAttention,V2=ADHDFriends58M1$DifficultyAttention, V3=ADHDFriends58M2$DifficultyAttention))
      TroubleRelaxing58<-as.matrix(data.frame(V1=ADHDFriends58M0$TroubleRelaxing,V2=ADHDFriends58M1$TroubleRelaxing, V3=ADHDFriends58M2$TroubleRelaxing))
      TroubleRelaxing <- sienaDependent( TroubleRelaxing58, type="behavior" )#Siena Object
      DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
    #TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
      HighDifficultyAttention58<-ifelse(DifficultyAttention58>2,1,0)
      HighDifficultyAttention <- varCovar(HighDifficultyAttention58)#Siena Object
      
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
        GPA<-cbind(ADHDFriends58M0$GPA-1,ADHDFriends58M1$GPA-1,ADHDFriends58M2$GPA-1)
        Counseling<-cbind(ADHDFriends58M0$Counseling,ADHDFriends58M1$Counseling,ADHDFriends58M2$Counseling)
        TeacherTrouble<-cbind(ADHDFriends58M0$TeacherTrouble,ADHDFriends58M1$TeacherTrouble,ADHDFriends58M2$TeacherTrouble)
 
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
        GPA<-varCovar(GPA)
        Counseling<-varCovar(Counseling)
        TeacherTrouble<-varCovar(TeacherTrouble)
      
    #Data
      mydata58 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention)
      #mydata58.1 <- sienaDataCreate(friendship, TroubleRelaxing, FRFLAG, Age, Female, Black, Latino, Asian, Race, GPA, SchoolDummy, changes)
    #Descriptives
      mean(Counseling[,1], na.rm=TRUE)  
      mean(Counseling[,2], na.rm=TRUE)
      mean(Counseling[,3], na.rm=TRUE)
      mean(TeacherTrouble[,1], na.rm=TRUE)  
      mean(TeacherTrouble[,2], na.rm=TRUE)
      mean(TeacherTrouble[,3], na.rm=TRUE)
      mean(GPA[,1], na.rm=TRUE)  
      mean(GPA[,2], na.rm=TRUE)
      mean(GPA[,3], na.rm=TRUE)
      mean(c(DifficultyAttention58[,2],DifficultyAttention77[,2]), na.rm=TRUE)  
      sum(HighDifficultyAttention58[,2], na.rm=TRUE)
      sum(HighDifficultyAttention77[,2], na.rm=TRUE)
      
  #Set Data for School 77
    #Set composition change
      comp77<-as.data.frame(ADHDFriends77M0$Missing)
      comp77$begin<-ifelse(ADHDFriends77M0$Missing==0,1.0,ifelse(ADHDFriends77M1$Missing==0,2.0,ifelse(ADHDFriends77M2$Missing==0,3.0,0)))
      comp77$end<-ifelse(ADHDFriends77M1$Missing==1.0,1.0,ifelse(ADHDFriends77M2$Missing==1.0,2.0,3.0))
      comp77$'ADHDFriends77M0$Missing'<-NULL
      comp77.list <- as.list(as.data.frame(t(comp77)))
      changes <- sienaCompositionChange(comp77.list)
    #Create Networks
      FriendsArray77 <- array( c( S77W0, S77W1,S77W2),
                               dim = c( 1467, 1467, 3 ) )
      friendship <- sienaDependent(FriendsArray77)   #Siena Object
    #Create Overlap covariate
      overlap <- varDyadCovar(array(c(overlap77.94,overlap77.95), dim=c(1467, 1467, 2)))
    #Behaviors
      DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77M0$DifficultyAttention,V2=ADHDFriends77M1$DifficultyAttention, V3=ADHDFriends77M2$DifficultyAttention))
      TroubleRelaxing77<-as.matrix(data.frame(V1=ADHDFriends77M0$TroubleRelaxing,V2=ADHDFriends77M1$TroubleRelaxing, V3=ADHDFriends77M2$TroubleRelaxing))
      TroubleRelaxing <- sienaDependent( TroubleRelaxing77, type="behavior" )#Siena Object
      DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
    #TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
      HighDifficultyAttention77<-ifelse(DifficultyAttention77>2,1,0)
      HighDifficultyAttention <- varCovar(HighDifficultyAttention77)#Siena Object
    #Covariates
      #Prep
        FRFLAG77<-ADHDFriends77M1$FR_FLAG
        ADHDFriends77M1$sch<-as.numeric(1)
        SchoolDummy77<-ADHDFriends77M1$sch
        Age<-data.frame(V1=(94-ADHDFriends77M0$Age), V2=ADHDFriends77M1$Age, V3=ADHDFriends77M2$Age)
        Age$Age<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA)))
        Female<-ifelse(!is.na(ADHDFriends77M0$Sex),ADHDFriends77M0$Sex-1,ifelse(!is.na(ADHDFriends77M1$Sex),ADHDFriends77M1$Sex-1,ADHDFriends77M2$Sex-1))
        Race<-ifelse(ADHDFriends77M0$White==1,1,ifelse(ADHDFriends77M0$Black==1,2,ifelse(ADHDFriends77M0$Latino==1,3,ifelse(ADHDFriends77M0$Latino==1,4,ifelse(ADHDFriends77M0$Asian==1,5,6)))))
        White<-ADHDFriends77M0$White
        Black<-ADHDFriends77M0$Black
        Latino<-ADHDFriends77M0$Latino
        Latino<-ifelse(Latino!=1,0,1)
        Asian<-ADHDFriends77M0$Asian
        Grade<-ADHDFriends77M0$Grade
        GPA<-cbind(ADHDFriends77M0$GPA-1,ADHDFriends77M1$GPA-1,ADHDFriends77M2$GPA-1)
        Counseling<-cbind(ADHDFriends77M0$Counseling,ADHDFriends77M1$Counseling,ADHDFriends77M2$Counseling)
        TeacherTrouble<-cbind(ADHDFriends77M0$TeacherTrouble,ADHDFriends77M1$TeacherTrouble,ADHDFriends77M2$TeacherTrouble)

      #Siena Objects
        FRFLAG<- coCovar(FRFLAG77)
        SchoolDummy<- coCovar(SchoolDummy77)
        Age<-coCovar(Age$Age)
        Female<-coCovar(Female)
        Black<-coCovar(Black)
        White<-coCovar(White)
        Latino<-coCovar(Latino)
        Asian<-coCovar(Asian)
        Race<-coCovar(Race)
        Grade<-coCovar(Grade)
        GPA<-varCovar(GPA)
        Counseling<-varCovar(Counseling)
        TeacherTrouble<-varCovar(TeacherTrouble)
      
      #Descriptives
        #mean(Counseling[,1], na.rm=TRUE)  
        #mean(Counseling[,2], na.rm=TRUE)
        #mean(Counseling[,3], na.rm=TRUE)
        #mean(TeacherTrouble[,1], na.rm=TRUE)  
        #mean(TeacherTrouble[,2], na.rm=TRUE)
        #mean(TeacherTrouble[,3], na.rm=TRUE)
        #mean(GPA[,1], na.rm=TRUE)  
        #mean(GPA[,2], na.rm=TRUE)
        #mean(GPA[,3], na.rm=TRUE)
        
      #Data
        mydata77 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention)
        #mydata77.1 <- sienaDataCreate(friendship, TroubleRelaxing, FRFLAG, Age, Female, Black, Latino, Asian, Race, GPA, SchoolDummy, changes) #Data - TroubleRelaxing removed
      #Combine and Save Data  
        mydata <- sienaGroupCreate(list(mydata58,mydata77))
        saveRDS(mydata, 'Siena Model.RDS')    
    