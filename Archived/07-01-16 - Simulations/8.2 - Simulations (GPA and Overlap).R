
library(RSiena)
library(gmodels)
library(parallel)
library(ggplot2)
#Set Directory
tempfile(tmpdir="")
setwd("")

#Create new overlap networks to simulate.
  #overlap simulation
  #None of the rnorm, runif, etc, functions work for generating a random matrix with the same distribution as overlap. Therefore I'm taking a blunter approach.
#load data
  overlap77.95<-readRDS('overlap77.95.RDS')
  overlap77.94<-readRDS('overlap77.94.RDS')
  overlap58.95<-readRDS('overlap58.95.RDS')
  overlap58.94<-readRDS('overlap58.94.RDS')

#SCHOOL 58
    #Create grade and GPA variables
    Grade<-ADHDFriends58M0$Grade
    GPA<-cbind(ADHDFriends58M0$GPA-1,ADHDFriends58M1$GPA-1,ADHDFriends58M2$GPA-1)
    #Create matrix for sharing a grade to estimate correlation
    AdjList<- data.frame(sender=rep(Grade, length(Grade))) #create duplicates of each business in a city
    Temp<-data.frame(receiver=Grade)
    Temp[,2]<-1:nrow(Temp)   #add an index
    Temp1<- Temp[rep(row.names(Temp), length(Temp[,1])), 1:2] #duplicate rows and index
    Temp1<- Temp1[order(Temp1[,2]),] #reorder 
    AdjList<-cbind(AdjList,Temp1[,1]) #bind two different 
    AdjList$Gradeequal<-ifelse(AdjList$sender==AdjList$`Temp1[, 1]`,1,0)
    Grademat58<-matrix(AdjList[,3], nrow=length(Grade), ncol=length(Grade))
    #Create GPA correlation matrix
    AdjList<- data.frame(sender=rep(GPA[,1], length(GPA[,1]))) #create duplicates of each business in a city
    Temp<-data.frame(receiver=GPA[,1])
    Temp[,2]<-1:nrow(Temp)   #add an index
    Temp1<- Temp[rep(row.names(Temp), length(Temp[,1])), 1:2] #duplicate rows and index
    Temp1<- Temp1[order(Temp1[,2]),] #reorder 
    AdjList<-cbind(AdjList,Temp1[,1]) #bind two different 
    AdjList$GPAdiff<-abs(AdjList[,1]-AdjList[,2])
    GPAmat58<-matrix(AdjList[,3], nrow=length(GPA[,1]), ncol=length(GPA[,1]))
  #SCHOOL 77
    #Create grade and GPA variables
    Grade<-ADHDFriends77M0$Grade
    GPA<-cbind(ADHDFriends77M0$GPA-1,ADHDFriends77M1$GPA-1,ADHDFriends77M2$GPA-1)
    #Create matrix for sharing a grade to estimate correlation
    AdjList<- data.frame(sender=rep(Grade, length(Grade))) #create duplicates of each business in a city
    Temp<-data.frame(receiver=Grade)
    Temp[,2]<-1:nrow(Temp)   #add an index
    Temp1<- Temp[rep(row.names(Temp), length(Temp[,1])), 1:2] #duplicate rows and index
    Temp1<- Temp1[order(Temp1[,2]),] #reorder 
    AdjList<-cbind(AdjList,Temp1[,1]) #bind two different 
    AdjList$Gradeequal<-ifelse(AdjList$sender==AdjList$`Temp1[, 1]`,1,0)
    Grademat77<-matrix(AdjList[,3], nrow=length(Grade), ncol=length(Grade))
    #Create GPA correlation matrix
    AdjList<- data.frame(sender=rep(GPA[,1], length(GPA[,1]))) #create duplicates of each business in a city
    Temp<-data.frame(receiver=GPA[,1])
    Temp[,2]<-1:nrow(Temp)   #add an index
    Temp1<- Temp[rep(row.names(Temp), length(Temp[,1])), 1:2] #duplicate rows and index
    Temp1<- Temp1[order(Temp1[,2]),] #reorder 
    AdjList<-cbind(AdjList,Temp1[,1]) #bind two different 
    AdjList$GPAdiff<-abs(AdjList[,1]-AdjList[,2])
    GPAmat77<-matrix(AdjList[,3], nrow=length(GPA[,1]), ncol=length(GPA[,1]))

    
    
    
        
#Siena setup

#Set Data for School 58
#Set composition change
comp58<-as.data.frame(ADHDFriends58M0$Missing)
comp58$begin<-ifelse(ADHDFriends58M0$Missing==0,1.0,ifelse(ADHDFriends58M1$Missing==0,2.0,ifelse(ADHDFriends58M2$Missing==0,3.0,0)))
comp58$end<-ifelse(ADHDFriends58M1$Missing==1.0,1.0,ifelse(ADHDFriends58M2$Missing==1.0,2.0,2.0)) #CHANGED
comp58$'ADHDFriends58M0$Missing'<-NULL
comp58.list <- as.list(as.data.frame(t(comp58)))
#Create Networks
FriendsArray58 <- array( c( S58W0, S58W1),
                         dim = c( 736, 736, 2 ) ) #CHANGED

#Behaviors - CHANGED
DifficultyAttention58<-as.matrix(data.frame(V1=ADHDFriends58M0$DifficultyAttention,V2=ADHDFriends58M1$DifficultyAttention))
#TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
HighDifficultyAttention58<-ifelse(DifficultyAttention58>2,1,0)  
HighDifficultyAttention58<-HighDifficultyAttention58[,1]#CHANGED

#Covariates
#Prep
FRFLAG58<-ADHDFriends58M1$FR_FLAG
ADHDFriends58M1$sch<-as.numeric(0)
SchoolDummy58<-ADHDFriends58M1$sch
Age<-data.frame(V1=(94-ADHDFriends58M0$Age), V2=ADHDFriends58M1$Age, V3=ADHDFriends58M2$Age) 
Age58<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))) 
Female58<-ifelse(!is.na(ADHDFriends58M0$Sex),ADHDFriends58M0$Sex-1,ifelse(!is.na(ADHDFriends58M1$Sex),ADHDFriends58M1$Sex-1,ADHDFriends58M2$Sex-1))
Race58<-ifelse(ADHDFriends58M0$White==1,1,ifelse(ADHDFriends58M0$Black==1,2,ifelse(ADHDFriends58M0$Latino==1,3,ifelse(ADHDFriends58M0$Latino==1,4,ifelse(ADHDFriends58M0$Asian==1,5,6)))))
Black58<-ADHDFriends58M0$Black
White58<-ADHDFriends58M0$White
Latino<-ADHDFriends58M0$Latino
Latino58<-ifelse(Latino!=1,0,1)
Asian58<-ADHDFriends58M0$Asian
Grade58<-ADHDFriends58M0$Grade
GPA58<-ADHDFriends58M0$GPA-1 #CHANGED
Counseling58<-ADHDFriends58M0$Counseling #CHANGED
TeacherTrouble58<-ADHDFriends58M0$TeacherTrouble #CHANGED

#Set Data for School 77
#Set composition change
comp77<-as.data.frame(ADHDFriends77M0$Missing)
comp77$begin<-ifelse(ADHDFriends77M0$Missing==0,1.0,ifelse(ADHDFriends77M1$Missing==0,2.0,ifelse(ADHDFriends77M2$Missing==0,3.0,0)))
comp77$end<-ifelse(ADHDFriends77M1$Missing==1.0,1.0,ifelse(ADHDFriends77M2$Missing==1.0,2.0,2.0)) #CHANGED
comp77$'ADHDFriends77M0$Missing'<-NULL
comp77.list <- as.list(as.data.frame(t(comp77)))
#Create Networks
FriendsArray77 <- array( c( S77W0, S77W1),
                         dim = c( 1467, 1467, 2 ) ) #CHANGED

#Behaviors - CHANGED
DifficultyAttention77<-as.matrix(data.frame(V1=ADHDFriends77M0$DifficultyAttention,V2=ADHDFriends77M1$DifficultyAttention))
#TEMPORARY - Make binary difficulty attention variable - Cut off at inattenion=3
HighDifficultyAttention77<-ifelse(DifficultyAttention77>2,1,0)  
HighDifficultyAttention77<-HighDifficultyAttention77[,1]#CHANGED

#Covariates
#Prep
FRFLAG77<-ADHDFriends77M1$FR_FLAG
ADHDFriends77M1$sch<-as.numeric(0)
SchoolDummy77<-ADHDFriends77M1$sch
Age<-data.frame(V1=(94-ADHDFriends77M0$Age), V2=ADHDFriends77M1$Age, V3=ADHDFriends77M2$Age) 
Age77<-ifelse(!is.na(Age$V3),Age$V3,ifelse(!is.na(Age$V2),Age$V2,ifelse(!is.na(Age$V1),Age$V1,NA))) 
Female77<-ifelse(!is.na(ADHDFriends77M0$Sex),ADHDFriends77M0$Sex-1,ifelse(!is.na(ADHDFriends77M1$Sex),ADHDFriends77M1$Sex-1,ADHDFriends77M2$Sex-1))
Race77<-ifelse(ADHDFriends77M0$White==1,1,ifelse(ADHDFriends77M0$Black==1,2,ifelse(ADHDFriends77M0$Latino==1,3,ifelse(ADHDFriends77M0$Latino==1,4,ifelse(ADHDFriends77M0$Asian==1,5,6)))))
Black77<-ADHDFriends77M0$Black
White77<-ADHDFriends77M0$White
Latino<-ADHDFriends77M0$Latino
Latino77<-ifelse(Latino!=1,0,1)
Asian77<-ADHDFriends77M0$Asian
Grade77<-ADHDFriends77M0$Grade
GPA77<-ADHDFriends77M0$GPA-1 #CHANGED
Counseling77<-ADHDFriends77M0$Counseling #CHANGED
TeacherTrouble77<-ADHDFriends77M0$TeacherTrouble #CHANGED

#Randomize overlap
#  cor(c(overlap58.94),c(Grademat58),use="complete") #correlated
x<-data.frame(v1=runif(736^2,-1,1),v2=runif(736^2,-1,1),v3=runif(736^2,-1,1),v4=runif(736^2,-1,1),v5=runif(736^2,-1,1),v6=runif(736^2,-1,1),v7=runif(736^2,-1,1),v8=runif(736^2,-1,1),v9=runif(736^2,-1,1),v10=runif(736^2,-1,1), Overlap=c(overlap58.94))
x$Overlap1<-(x$v1+x$v2+x$v3+x$v4+x$v5+x$v6+x$v7+x$v8+x$v9+x$v10+x$Overlap)
x$Overlap1<-ifelse(x$Overlap1>5.4,5.4,ifelse(x$Overlap1<2.7,2.7,x$Overlap1))
x$Overlap1<-x$Overlap1-(mean(x$Overlap1)-mean(overlap58.94))
R1Overlap<-x[,12]
#  cor(R1Overlap,c(Grademat58),use="complete") #correlation gone
#  mean(R1Overlap)
#  min(R1Overlap)
#Randomize but Correlate overlap with sharing a grade
R2Overlap<-ifelse(c(Grademat58==1),(R1Overlap+.1),R1Overlap*.5)
#  max(R2Overlap,na.rm=TRUE)
#  mean(R2Overlap,na.rm=TRUE)
#  cor(R2Overlap,c(Grademat58),use="complete") #correlation good 
R2Overlap<-ifelse(is.na(R2Overlap),mean(R2Overlap,na.rm=TRUE),R2Overlap)
#Correlated with similar GPA at .25
GPAmat582<-c(GPAmat58)
#  R3Overlap<-(R2Overlap-GPAmat582/12.5)
#  R3Overlap<-R3Overlap-(mean(R3Overlap,na.rm=TRUE)-mean(overlap58.94))
#  cor(R3Overlap,GPAmat582,use="complete") #Good
#  cor(R3Overlap,c(Grademat58),use="complete") #No correlation originally
#  max(R3Overlap,na.rm=TRUE)
#  mean(R3Overlap,na.rm=TRUE)
#  min(R3Overlap,na.rm=TRUE)
#  R3Overlap<-ifelse(is.na(R3Overlap),mean(R3Overlap,na.rm=TRUE),R3Overlap)
#Correlated with similar GPA at .5
R4Overlap<-abs(R2Overlap-GPAmat582/3.6)
R4Overlap<-R4Overlap-.26
R4Overlap<-ifelse(R4Overlap<0,0,R4Overlap)
#  cor(R4Overlap,GPAmat582,use="complete") #No correlation originally
#  max(R4Overlap,na.rm=TRUE)
#  mean(R4Overlap,na.rm=TRUE)
#  min(R4Overlap,na.rm=TRUE)
R4Overlap<-ifelse(is.na(R4Overlap),mean(R4Overlap,na.rm=TRUE),R4Overlap)
#Turn the key overlap parameters into matrices with more accurate names
#overlap58.94corGrade<-matrix(R2Overlap, nrow = 736,ncol=736)
#overlap58.94corGPA1<-matrix(R3Overlap, nrow = 736,ncol=736)
overlap58.94corGPA2<-matrix(R4Overlap, nrow = 736,ncol=736)

#School 77
#Randomize overlap
#  cor(c(overlap77.94),c(Grademat77),use="complete") #correlated
x<-data.frame(v1=runif(1467^2,-1,1),v2=runif(1467^2,-1,1),v3=runif(1467^2,-1,1),v4=runif(1467^2,-1,1),v5=runif(1467^2,-1,1),v6=runif(1467^2,-1,1),v7=runif(1467^2,-1,1),v8=runif(1467^2,-1,1),v9=runif(1467^2,-1,1),v10=runif(1467^2,-1,1), Overlap=c(overlap77.94))
x$Overlap1<-(x$v1+x$v2+x$v3+x$v4+x$v5+x$v6+x$v7+x$v8+x$v9+x$v10+x$Overlap)
x$Overlap1<-ifelse(x$Overlap1>5.4,5.4,ifelse(x$Overlap1<2.7,2.7,x$Overlap1))
x$Overlap1<-x$Overlap1-(mean(x$Overlap1)-mean(overlap77.94))
R1Overlap<-x[,12]
#  cor(R1Overlap,c(Grademat77),use="complete") #correlation gone
#  mean(R1Overlap)
#  max(R1Overlap)
#  mean(c(overlap77.94))
#Randomize but Correlate overlap with sharing a grade
R2Overlap<-ifelse(c(Grademat77==1),(R1Overlap+.04),R1Overlap*.55)
#  max(R2Overlap,na.rm=TRUE)
#  mean(R2Overlap,na.rm=TRUE)
#  cor(R2Overlap,c(Grademat77),use="complete") #correlation good 
R2Overlap<-ifelse(is.na(R2Overlap),mean(R2Overlap,na.rm=TRUE),R2Overlap)
#Correlated with similar GPA at .25
GPAmat772<-c(GPAmat77)
#  R3Overlap<-(R2Overlap-GPAmat772/12.5)
#  R3Overlap<-R3Overlap-(mean(R3Overlap,na.rm=TRUE)-mean(overlap77.94))
#  cor(R3Overlap,GPAmat772,use="complete") #No correlation originally
#  cor(R2Overlap,c(GPAmat77),use="complete") #No correlation originally
#  max(R3Overlap,na.rm=TRUE)
#  mean(R3Overlap,na.rm=TRUE)
#  R3Overlap<-ifelse(is.na(R3Overlap),mean(R3Overlap,na.rm=TRUE),R3Overlap)
#Correlated with similar GPA at .5
R4Overlap<-abs(R2Overlap-GPAmat772/2.7)
R4Overlap<-R4Overlap-.5
R4Overlap<-ifelse(R4Overlap<0,0,R4Overlap)
#  cor(R4Overlap,GPAmat772,use="complete") #No correlation originally
#  max(R4Overlap,na.rm=TRUE)
#  mean(R4Overlap,na.rm=TRUE)
#  min(R4Overlap,na.rm=TRUE)
R4Overlap<-ifelse(is.na(R4Overlap),mean(R4Overlap,na.rm=TRUE),R4Overlap)
#Turn the key overlap parameters into matrices with more accurate names
#  overlap77.94corGrade<-matrix(R2Overlap, nrow = 1467,ncol=1467)
#  overlap77.94corGPA1<-matrix(R3Overlap, nrow = 1467,ncol=1467)
overlap77.94corGPA2<-matrix(R4Overlap, nrow = 1467,ncol=1467)

sd(overlap58.94)

changes <- sienaCompositionChange(comp58.list)
friendship <- sienaDependent(FriendsArray58)   #Siena Object
#KEY NEW STEP
#Create Overlap covariates
#overlapcorGrade <- coDyadCovar(overlap58.94corGrade) 
#overlapcorGPA1 <- coDyadCovar(overlap58.94corGPA1) 
overlapcorGPA2 <- coDyadCovar(overlap58.94corGPA2) 
DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
HighDifficultyAttention <- coCovar(HighDifficultyAttention58)#Siena Object
#Siena Objects
FRFLAG<- coCovar(FRFLAG58)
SchoolDummy<- coCovar(SchoolDummy58)
Age<-coCovar(Age58)
Female<-coCovar(Female58)
White<-coCovar(White58)
Black<-coCovar(Black58)
Latino<-coCovar(Latino58)
Asian<-coCovar(Asian58)
Race<-coCovar(Race58)
Grade<-coCovar(Grade58)
GPA<-coCovar(GPA58)
Counseling<-coCovar(Counseling58)
TeacherTrouble<-coCovar(TeacherTrouble58)
overlap <- coDyadCovar(overlap58.94) 

#Data
mydata58 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention, overlapcorGPA2)
changes <- sienaCompositionChange(comp77.list)
friendship <- sienaDependent(FriendsArray77)   #Siena Object
#KEY NEW STEP
#Create Overlap covariate
overlap <- coDyadCovar(overlap77.94) #CHANGED
#overlapcorGrade <- coDyadCovar(overlap77.94corGrade) 
#overlapcorGPA1 <- coDyadCovar(overlap77.94corGPA1) 
overlapcorGPA2 <- coDyadCovar(overlap77.94corGPA2) 
DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
HighDifficultyAttention <- coCovar(HighDifficultyAttention77)#Siena Object
#Siena Objects
FRFLAG<- coCovar(FRFLAG77)
SchoolDummy<- coCovar(SchoolDummy77)
Age<-coCovar(Age77)
Female<-coCovar(Female77)
White<-coCovar(White77)
Black<-coCovar(Black77)
Latino<-coCovar(Latino77)
Asian<-coCovar(Asian77)
Race<-coCovar(Race77)
Grade<-coCovar(Grade77)
GPA<-coCovar(GPA77)
Counseling<-coCovar(Counseling77)
TeacherTrouble<-coCovar(TeacherTrouble77)
#Data
mydata77 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention, overlapcorGPA2)
#Combine and Save Data  
mydata <- sienaGroupCreate(list(mydata58,mydata77))

#Load siena estimates to use for new model
ansCF1 <- readRDS('SM CDAM FULL1.RDS')
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


#CHANGE PER SIMULATION!

      #overlap
      myeff <- setEffect(myeff, X, interaction1='overlapcorGPA2', fix=TRUE, test=FALSE, initialValue=initvalue[9,2])
      #overlapcorGrade, overlapcorGPA1, overlapcorGPA2


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



#Start iterating here. Requires a single run already with code belo


for (j in 1:50){

#Randomize overlap
#  cor(c(overlap58.94),c(Grademat58),use="complete") #correlated
x<-data.frame(v1=runif(736^2,-1,1),v2=runif(736^2,-1,1),v3=runif(736^2,-1,1),v4=runif(736^2,-1,1),v5=runif(736^2,-1,1),v6=runif(736^2,-1,1),v7=runif(736^2,-1,1),v8=runif(736^2,-1,1),v9=runif(736^2,-1,1),v10=runif(736^2,-1,1), Overlap=c(overlap58.94))
x$Overlap1<-(x$v1+x$v2+x$v3+x$v4+x$v5+x$v6+x$v7+x$v8+x$v9+x$v10+x$Overlap)
x$Overlap1<-ifelse(x$Overlap1>5.4,5.4,ifelse(x$Overlap1<2.7,2.7,x$Overlap1))
x$Overlap1<-x$Overlap1-(mean(x$Overlap1)-mean(overlap58.94))
R1Overlap<-x[,12]
#  cor(R1Overlap,c(Grademat58),use="complete") #correlation gone
#  mean(R1Overlap)
#  min(R1Overlap)
#Randomize but Correlate overlap with sharing a grade
R2Overlap<-ifelse(c(Grademat58==1),(R1Overlap+.1),R1Overlap*.5)
#  max(R2Overlap,na.rm=TRUE)
#  mean(R2Overlap,na.rm=TRUE)
#  cor(R2Overlap,c(Grademat58),use="complete") #correlation good 
R2Overlap<-ifelse(is.na(R2Overlap),mean(R2Overlap,na.rm=TRUE),R2Overlap)
#Correlated with similar GPA at .25
GPAmat582<-c(GPAmat58)
#  R3Overlap<-(R2Overlap-GPAmat582/12.5)
#  R3Overlap<-R3Overlap-(mean(R3Overlap,na.rm=TRUE)-mean(overlap58.94))
#  cor(R3Overlap,GPAmat582,use="complete") #Good
#  cor(R3Overlap,c(Grademat58),use="complete") #No correlation originally
#  max(R3Overlap,na.rm=TRUE)
#  mean(R3Overlap,na.rm=TRUE)
#  min(R3Overlap,na.rm=TRUE)
#  R3Overlap<-ifelse(is.na(R3Overlap),mean(R3Overlap,na.rm=TRUE),R3Overlap)
#Correlated with similar GPA at .5
R4Overlap<-abs(R2Overlap-GPAmat582/3.6)
R4Overlap<-R4Overlap-.26
R4Overlap<-ifelse(R4Overlap<0,0,R4Overlap)
#  cor(R4Overlap,GPAmat582,use="complete") #No correlation originally
#  max(R4Overlap,na.rm=TRUE)
#  mean(R4Overlap,na.rm=TRUE)
#  min(R4Overlap,na.rm=TRUE)
R4Overlap<-ifelse(is.na(R4Overlap),mean(R4Overlap,na.rm=TRUE),R4Overlap)
#Turn the key overlap parameters into matrices with more accurate names
#overlap58.94corGrade<-matrix(R2Overlap, nrow = 736,ncol=736)
#overlap58.94corGPA1<-matrix(R3Overlap, nrow = 736,ncol=736)
overlap58.94corGPA2<-matrix(R4Overlap, nrow = 736,ncol=736)

#School 77
#Randomize overlap
#  cor(c(overlap77.94),c(Grademat77),use="complete") #correlated
x<-data.frame(v1=runif(1467^2,-1,1),v2=runif(1467^2,-1,1),v3=runif(1467^2,-1,1),v4=runif(1467^2,-1,1),v5=runif(1467^2,-1,1),v6=runif(1467^2,-1,1),v7=runif(1467^2,-1,1),v8=runif(1467^2,-1,1),v9=runif(1467^2,-1,1),v10=runif(1467^2,-1,1), Overlap=c(overlap77.94))
x$Overlap1<-(x$v1+x$v2+x$v3+x$v4+x$v5+x$v6+x$v7+x$v8+x$v9+x$v10+x$Overlap)
x$Overlap1<-ifelse(x$Overlap1>5.4,5.4,ifelse(x$Overlap1<2.7,2.7,x$Overlap1))
x$Overlap1<-x$Overlap1-(mean(x$Overlap1)-mean(overlap77.94))
R1Overlap<-x[,12]
#  cor(R1Overlap,c(Grademat77),use="complete") #correlation gone
#  mean(R1Overlap)
#  max(R1Overlap)
#  mean(c(overlap77.94))
#Randomize but Correlate overlap with sharing a grade
R2Overlap<-ifelse(c(Grademat77==1),(R1Overlap+.04),R1Overlap*.55)
#  max(R2Overlap,na.rm=TRUE)
#  mean(R2Overlap,na.rm=TRUE)
#  cor(R2Overlap,c(Grademat77),use="complete") #correlation good 
R2Overlap<-ifelse(is.na(R2Overlap),mean(R2Overlap,na.rm=TRUE),R2Overlap)
#Correlated with similar GPA at .25
GPAmat772<-c(GPAmat77)
#  R3Overlap<-(R2Overlap-GPAmat772/12.5)
#  R3Overlap<-R3Overlap-(mean(R3Overlap,na.rm=TRUE)-mean(overlap77.94))
#  cor(R3Overlap,GPAmat772,use="complete") #No correlation originally
#  cor(R2Overlap,c(GPAmat77),use="complete") #No correlation originally
#  max(R3Overlap,na.rm=TRUE)
#  mean(R3Overlap,na.rm=TRUE)
#  R3Overlap<-ifelse(is.na(R3Overlap),mean(R3Overlap,na.rm=TRUE),R3Overlap)
#Correlated with similar GPA at .5
R4Overlap<-abs(R2Overlap-GPAmat772/2.7)
R4Overlap<-R4Overlap-.5
R4Overlap<-ifelse(R4Overlap<0,0,R4Overlap)
#  cor(R4Overlap,GPAmat772,use="complete") #No correlation originally
#  max(R4Overlap,na.rm=TRUE)
#  mean(R4Overlap,na.rm=TRUE)
#  min(R4Overlap,na.rm=TRUE)
R4Overlap<-ifelse(is.na(R4Overlap),mean(R4Overlap,na.rm=TRUE),R4Overlap)
#Turn the key overlap parameters into matrices with more accurate names
#  overlap77.94corGrade<-matrix(R2Overlap, nrow = 1467,ncol=1467)
#  overlap77.94corGPA1<-matrix(R3Overlap, nrow = 1467,ncol=1467)
overlap77.94corGPA2<-matrix(R4Overlap, nrow = 1467,ncol=1467)

changes <- sienaCompositionChange(comp58.list)
friendship <- sienaDependent(FriendsArray58)   #Siena Object
#KEY NEW STEP
#Create Overlap covariates
#overlapcorGrade <- coDyadCovar(overlap58.94corGrade) 
#overlapcorGPA1 <- coDyadCovar(overlap58.94corGPA1) 
overlapcorGPA2 <- coDyadCovar(overlap58.94corGPA2) 
DifficultyAttention <- sienaDependent( DifficultyAttention58, type="behavior" )#Siena Object
HighDifficultyAttention <- coCovar(HighDifficultyAttention58)#Siena Object
#Siena Objects
FRFLAG<- coCovar(FRFLAG58)
SchoolDummy<- coCovar(SchoolDummy58)
Age<-coCovar(Age58)
Female<-coCovar(Female58)
White<-coCovar(White58)
Black<-coCovar(Black58)
Latino<-coCovar(Latino58)
Asian<-coCovar(Asian58)
Race<-coCovar(Race58)
Grade<-coCovar(Grade58)
GPA<-coCovar(GPA58)
Counseling<-coCovar(Counseling58)
TeacherTrouble<-coCovar(TeacherTrouble58)
overlap <- coDyadCovar(overlap58.94) 

#Data
mydata58 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention, overlapcorGPA2)
changes <- sienaCompositionChange(comp77.list)
friendship <- sienaDependent(FriendsArray77)   #Siena Object
#KEY NEW STEP
#Create Overlap covariate
overlap <- coDyadCovar(overlap77.94) #CHANGED
#overlapcorGrade <- coDyadCovar(overlap77.94corGrade) 
#overlapcorGPA1 <- coDyadCovar(overlap77.94corGPA1) 
overlapcorGPA2 <- coDyadCovar(overlap77.94corGPA2) 
DifficultyAttention <- sienaDependent( DifficultyAttention77, type="behavior" )#Siena Object
HighDifficultyAttention <- coCovar(HighDifficultyAttention77)#Siena Object
#Siena Objects
FRFLAG<- coCovar(FRFLAG77)
SchoolDummy<- coCovar(SchoolDummy77)
Age<-coCovar(Age77)
Female<-coCovar(Female77)
White<-coCovar(White77)
Black<-coCovar(Black77)
Latino<-coCovar(Latino77)
Asian<-coCovar(Asian77)
Race<-coCovar(Race77)
Grade<-coCovar(Grade77)
GPA<-coCovar(GPA77)
Counseling<-coCovar(Counseling77)
TeacherTrouble<-coCovar(TeacherTrouble77)
#Data
mydata77 <- sienaDataCreate(friendship, DifficultyAttention, FRFLAG, Age, Female, Black, Latino, Asian, White, Grade, Race, GPA, Counseling, TeacherTrouble, SchoolDummy, changes, overlap, HighDifficultyAttention, overlapcorGPA2)
#Combine and Save Data  
mydata <- sienaGroupCreate(list(mydata58,mydata77))
#Final ModelQ
myalgorithm <- sienaAlgorithmCreate( projname = 'BDAM Full', useStdInits = FALSE, nsub=0, n3=50, simOnly=TRUE, cond=FALSE) #Final Algorithm
#Run
ansSim7 <- siena07( myalgorithm, data = mydata, effects = myeff, nbrNodes=6, useCluster=T, initC=T,returnDeps=TRUE)  #Compute

#create data frame for all simulated results
Sim58Results7<-data.frame(matrix(NA,nrow = 736, ncol = 50)) #empty dataframe
Sim77Results7<-data.frame(matrix(NA,nrow = 1467, ncol = 50)) #empty dataframe
for (i in 1:50){
  Sim58Results7[,i]<-ansSim7$sims[[i]][[1]][[2]]
  Sim77Results7[,i]<-ansSim7$sims[[i]][[2]][[2]]
}

Sim7v<-rbind(Sim58Results7,Sim77Results7)
#Combine results with dataframe of all results
#First time:
if(j==1){
  Sim7v2<-Sim7v
  }else {Sim7v2<-cbind(Sim7v,Sim7v2)}
saveRDS(Sim7v2,"Sim7v2.rds")
}


Sim7v2<-readRDS("Sim7v2.rds")

#STATS
Sim58v7<-Sim7v2[1:736,]
Sim77v7<-Sim7v2[737:2203,]

SimMeans2<-colMeans(Sim7v2)
mean(SimMeans2)
sd(SimMeans2)

SimMeans<-colMeans(Sim58v7)
mean(SimMeans)
SimMeans<-colMeans(Sim77v7)
mean(SimMeans)

#Estimate mean inattention
mean(Sim58v7)
mean(Sim77v7)
mean(Sim7v2)
mean(Sim7v)
#Estimate rate of high inattention
Sim58Count7<-sum(Sim58v7>2, na.rm = TRUE)/2500
Sim77Count7<-sum(Sim77v7>2, na.rm = TRUE)/2500
SimCount7<-sum(Sim58v7>2,Sim77v7>2)/2500
