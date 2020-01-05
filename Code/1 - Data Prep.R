#Libraries
  library(aod)
  library(ggplot2)
  library(lme4)
  library(igraph)
  library(RSiena)
  library(gmodels)
  library(parallel)

#Set directory (hidden)
  
#Load datasets 
  load("adhd0.RData")
  load("adhd.RData")
  load("adhd2.RData")
  load("friends0.RData")
  load("friends1.RData")
  load("friends2.RData")
  load("schinfo.RData")
  
#Convert friends values to numeric
  friends1 <- transform(friends1, as.numeric(aid, MF_AID1,MF_AID1,MF_AID2,MF_AID3,MF_AID4,MF_AID5,FF_AID1,FF_AID2,FF_AID3,FF_AID4,FF_AID5))
  
#Drop missing data  
  friends1[friends1==55555555|friends1==77777777|friends1==88888888|friends1==99999999|friends1==99959995|friends1=="NA"] <- NA
  
#Modify adhd
  adhd$ADHD<-as.numeric(adhd$H4ID5L) #Make ADHD variable
  adhd$ADHD[adhd$ADHD==6]<-NA   #replace ADHD with value 6
  adhd$scid<-as.numeric(adhd$scid)  #make SCID numeric
  adhd$aid<-as.numeric(adhd$aid)  #make AID numeric

#Make and Modify ADHD Friends merge adhd and friends1
  ADHDFriends<-merge(friends1,adhd, by=("aid")) #merge network data and person data
  ADHDFriends<-lapply(ADHDFriends,as.numeric) #Convert whole dataset to numeric
  ADHDFriends<-transform(ADHDFriends,   #Create variables of interest
                       ReadingSkills=ifelse((H1ED11)>4,NA,(H1ED11)),
                       MathSkills=ifelse((H1ED12)>4,NA,(H1ED12)),
                       RepeatGrades=ifelse((H1ED5)>1,NA,(H1ED5)),
                       LowSocialSkills=ifelse((H1PF35)>5,NA,(H1PF35)),
                       LowSelfEsteem1=ifelse((H1PF32)>5,NA,(H1PF32)),
                       LowSelfEsteem2=ifelse((H1PF33)>5,NA,(H1PF33)),
                       LowSelfEsteem3=ifelse((H1PF30)>5,NA,(H1PF30)),
                       LowSelfEsteem=(H1PF30+H1PF33+H1PF32)/3,
                       Rejected=ifelse((H1ED18)>5,NA,(H1ED18)),
                       TeacherTrouble=ifelse((H1ED15)>5,NA,(H1ED15)),
                       MoreCig=ifelse((H1TO3)>1,NA,(H1TO3)),
                       MoreAlc=ifelse((H1TO12)>1,NA,(H1TO12)),
                       MorePot=ifelse((H1TO32)>1,NA,(H1TO32)),
                       Suspended=ifelse((H1ED7)>1,NA,(H1ED7)),
                       Sex=BIO_SEX,
                       White=H1GI6A,
                       Black=H1GI6B,
                       HomeLanguage=ifelse((H1GI10)==3,2,ifelse((H1GI10>3),NA,H1GI10)),
                       BirthCountry=ifelse((H1GI11)>2,NA,(H1GI11)),
                       MotherEducation=ifelse((H1RM1)>9,NA,(H1RM1)),
                       TroubleRemembering=ifelse((H1BC3)>5,NA,(H1BC3)),
                       AvoidHomework=ifelse((H1ED17)>5,NA,(H1ED17)),
                       Procrastinates=ifelse((H1FS18)>5,NA,(H1FS18)),
                       GutFeeling=ifelse((H1PF16)>5,NA,(H1PF16)),
                       DifficultyAttention=ifelse((H1ED16)>5,NA,(H1ED16)),
                       EasilyDistracted=ifelse((H1FS5)>4,NA,(H1FS5)),
                       EasilyBored=ifelse((H1IR19)>2,NA,(H1IR19)),
                       TroubleRelaxing=ifelse((H1GH19)>5,NA,(H1GH19)),
                       Counseling=ifelse((H1HS3)>4,NA,(H1HS3)),  
                       H1PF8=ifelse((H1PF8)>5,NA,(H1PF8)),
                       H1PF15=ifelse((H1PF15)>5,NA,(H1PF15)),
                       H1PF18=ifelse(H1PF18>5,NA,(H1PF18)),
                       H1PF19=ifelse((H1PF19)>5,NA,(H1PF19)),
                       H1PF20=ifelse(H1PF20>5,NA,(H1PF20)),
                       Age=H1GI1Y
  )
#Do the same steps for third wave
  friends2 <- transform(friends2, as.numeric(aid, MF_AID1,MF_AID1,MF_AID2,MF_AID3,MF_AID4,MF_AID5,FF_AID1,FF_AID2,FF_AID3,FF_AID4,FF_AID5))
  friends2[friends2==55555555|friends2==77777777|friends2==88888888|friends2==99999999|friends2==99959995|friends2=="NA"] <- NA
  adhd2$ADHD<-as.numeric(adhd2$H4ID5L)
  adhd2$ADHD[adhd2$ADHD==6]<-NA
  ADHDFriends2<-merge(friends2,adhd2, by=("aid"))
  ADHDFriends2<-sapply(ADHDFriends2,as.numeric)
  ADHDFriends2<-transform(ADHDFriends2,
                        ReadingSkills=ifelse((H2ED7)>4,NA,(H2ED7)),
                        MathSkills=ifelse((H2ED8)>4,NA,(H2ED8)),
                        LowSocialSkills=ifelse((H2PF26)>5,NA,(H2PF26)),
                        LowSelfEsteem1=ifelse((H2PF21)>5,NA,(H2PF21)),
                        LowSelfEsteem2=ifelse((H2PF23)>5,NA,(H2PF23)),
                        LowSelfEsteem3=ifelse((H2PF24)>5,NA,(H2PF24)),
                        LowSelfEsteem=(H2PF21+H2PF23+H2PF24)/3,
                        TeacherTrouble=ifelse((H2ED11)>5,NA,(H2ED11)),
                        Suspended=ifelse((H2ED3)>1,NA,(H2ED3)),
                        TroubleRemembering=ifelse((H2BC3)>5,NA,(H2BC3)),
                        AvoidHomework=ifelse((H2ED13)>5,NA,(H2ED13)),
                        Procrastinates=ifelse((H2FS18)>5,NA,(H2FS18)),
                        GutFeeling=ifelse((H2PF15)>5,NA,(H2PF15)),
                        DifficultyAttention=ifelse((H2ED12)>5,NA,(H2ED12)),
                        EasilyDistracted=ifelse((H2FS5)>4,NA,(H2FS5)),
                        EasilyBored=ifelse((H2IR19)>2,NA,(H2IR19)),
                        TroubleRelaxing=ifelse((H2GH24)>5,NA,(H2GH24)),
                        HistorySkills=ifelse((H2ED9)>4,NA,(H2ED9)),
                        ScienceSkills=ifelse((H2ED10)>4,NA,(H2ED10)),
                        Counseling=ifelse((H2HS5)>4,NA,(H2HS5)),
                        Grade=H2GI9,
                        Age=H2GI1Y
  )           
  
#Grab time-invariant variables from ADHDFriends
  ADHDFriends2<-merge(ADHDFriends2,ADHDFriends[,c("aid","Sex","White","Black","HomeLanguage","BirthCountry","MotherEducation","scid")], by=("aid"))

#Do the same steps for the first wave (school file)
    MasterID<-data.frame(aid=adhd0$aid, sqid=adhd0$sqid) #create masterID list
    friends0<-merge(friends0, MasterID, by ="sqid") #merge IDs into friends0
    friends0$aid[friends0$aid=="NA"]<-NA  #convert character NA into NA
    friends0<-friends0[!(is.na(friends0$aid)),] #Remove NA aids from dataset  
    friends0 <- transform(friends0, as.numeric(aid, MF1AID,MF2AID,MF3AID,MF4AID,MF5AID,FF1AID,FF2AID,FF3AID,FF4AID,FF5AID))
    friends0[friends0==55555555|friends0==77777777|friends0==88888888|friends0==99999999|friends0==99959995|friends0=="NA"] <- NA
    adhd0$ADHD<-as.numeric(adhd0$H4ID5L)
    adhd0$ADHD[adhd0$ADHD==6]<-NA
    ADHDFriends0<-as.data.frame(merge(friends0,adhd0, by=("aid")))
    ADHDFriends0$aid<-as.character(ADHDFriends0$aid)
    ADHDFriends0<-as.data.frame(lapply(ADHDFriends0,as.numeric))
    ADHDFriends0<-transform(ADHDFriends0,
                            ReadingSkills=ifelse((S10A)>4,NA,(S10A)),
                            MathSkills=ifelse((S10B)>4,NA,(S10B)),
                            LowSocialSkills=ifelse((S62O)>5,NA,(S62O)),
                            LowSelfEsteem1=ifelse((S62M)>5,NA,(S62M)),
                            LowSelfEsteem2=ifelse((S62N)>5,NA,(S62N)),
                            LowSelfEsteem3=ifelse((S62P)>5,NA,(S62P)),
                            LowSelfEsteem=(S62M+S62N+S62P)/3,
                            AvoidHomework=ifelse((S46C)>5,NA,(S46C)),
                            DifficultyAttention=ifelse((S46B)>5,NA,(S46B)),
                            TroubleRelaxing=ifelse((S60L)>5,NA,(S60L)),
                            Age=S1,
                            Sex=S2,
                            Grade=S3,
                            Latino=S4,
                            White=S6A,
                            Black=S6B,
                            Asian=S6C,
                            NativeAmerican=S6D,
                            OtherRace=S6E,
                            HistorySkills=ifelse((S10C)>4,NA,(S10C)),
                            ScienceSkills=ifelse((S10D)>4,NA,(S10D)),
                            scid=as.numeric(sschlcde),
                            TeacherTrouble=ifelse(S46A>5,NA,S46A),
                            Counseling=ifelse(S53==1,1,0)
                            
    )           
    
#Create GPA Variables
  ADHDFriends0$GPA<-rowMeans(cbind(ADHDFriends0$MathSkills,ADHDFriends0$ReadingSkills,ADHDFriends0$HistorySkills,ADHDFriends0$ScienceSkills),na.rm=TRUE)
  ADHDFriends$GPA<-rowMeans(cbind(ADHDFriends$MathSkills,ADHDFriends$ReadingSkills,ADHDFriends$HistorySkills,ADHDFriends$ScienceSkills),na.rm=TRUE)
  ADHDFriends2$GPA<-rowMeans(cbind(ADHDFriends2$MathSkills,ADHDFriends2$ReadingSkills,ADHDFriends2$HistorySkills,ADHDFriends2$ScienceSkills),na.rm=TRUE)
