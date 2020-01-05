#To install packages
  #myrepo = getOption("repos")
  #myrepo["CRAN"]= "http://archive.linux.duke.edu/cran/"
  #options(repos=myrepo)
  #rm(myrepo)
  #install.packages("aod")
  #install.packages("ggplot2")
  #install.packages("lme4")
  #install.packages("igraph")
  #install.packages("RSiena")
  #install.packages("gmodels")
  
library(aod)
library(ggplot2)
library(lme4)
library(igraph)
library(RSiena)
library(gmodels)
#Set Directory
tempfile(tmpdir="")
setwd("")

#Load datasets
load("R Files/adhd.RData")
load("R Files/adhd2.RData")
load("R Files/friends1.RData")
load("R Files/friends2.RData")
load("R Files/schinfo.RData")
#load("R Files/homeinterview1.RData")
#load("R Files/homeinterview2.RData")
#load("R Files/homeinterview4.RData")
#load("R Files/weights.RData")
#load("R Files/weights2.RData")


#Modify friends1

  #Convert values to numeric
  attach(friends1)
  friends1 <- transform(friends1, 
                        as.numeric(aid, MF_AID1,MF_AID1,MF_AID2,MF_AID3,MF_AID4,MF_AID5,FF_AID1,FF_AID2,FF_AID3,FF_AID4,FF_AID5))
  #Drop missing data  
  friends1[friends1==55555555|friends1==77777777|friends1==88888888|friends1==99999999|friends1==99959995|friends1=="NA"] <- NA
  detach(friends1)
#Modify adhd
  attach(adhd)  
  #Make ADHD variable
  adhd$ADHD<-as.numeric(H4ID5L)
  #replace ADHD with value 6
  adhd$ADHD[adhd$ADHD==6]<-NA
  #make AID and SCID numeric
  adhd$scid<-as.numeric(scid)
  adhd$aid<-as.numeric(aid)
  #Remove missing ADHD observations
  adhd<-adhd[!(is.na(adhd$ADHD)), ]
  #merge adhd and friends1
  detach(adhd)
  ADHDFriends<-merge(friends1,adhd, by=("aid"))
  #Make Variables in ADHDFriends
  attach(ADHDFriends)
  ADHDFriends$ReadingSkills<-as.numeric(H1ED11)
  ADHDFriends$ReadingSkills[ADHDFriends$ReadingSkills>4]<-NA
  #Convert whole dataset to numeric
  ADHDFriends<-lapply(ADHDFriends,as.numeric)
  ADHDFriends<-transform(ADHDFriends,
                         ReadingSkills=ifelse((H1ED11)>4,NA,(H1ED11)),
                         MathSkills=ifelse((H1ED12)>4,NA,(H1ED12)),
                         RepeatGrades=ifelse((H1ED5)>1,NA,(H1ED5)),
                         LowSocialSkills=ifelse((H1PF35)>5,NA,(H1PF35)),
                         LowSelfEsteem1=ifelse((H1PF32)>5,NA,(H1PF32)),
                         LowSelfEsteem2=ifelse((H1PF33)>5,NA,(H1PF33)),
                         LowSelfEsteem3=ifelse((H1PF30)>5,NA,(H1PF30)),
                         LowSelfEsteem=sum(c(H1PF30,H1PF33,H1PF32))/3,
                         Rejected=ifelse((H1ED18)>5,NA,(H1ED18)),
                         ConflictsWithTeachers=ifelse((H1ED15)>5,NA,(H1ED15)),
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
                         H1PF8=ifelse((H1PF8)>5,NA,(H1PF8)),
                         H1PF15=ifelse((H1PF15)>5,NA,(H1PF15)),
                         H1PF18=ifelse(H1PF18>5,NA,(H1PF18)),
                         H1PF19=ifelse((H1PF19)>5,NA,(H1PF19)),
                         H1PF20=ifelse(H1PF20>5,NA,(H1PF20))
                         )
  tail(colnames(ADHDFriends))
  #Create Propensity scores for having ADHD
  #install.packages("lme4")

  attach(ADHDFriends)
  #Create propensity scores, first step runs the model
  PropScore<-glm(ADHD~DifficultyAttention+RepeatGrades+Suspended+EasilyBored+EasilyDistracted+LowSelfEsteem+MathSkills+LowSocialSkills+GutFeeling+TroubleRelaxing+ReadingSkills+White+Black+Sex+HomeLanguage+MotherEducation+factor(scid),
                 data = ADHDFriends, family = "binomial"(link="logit"), na.action=na.omit )
  #Since propensity scores omit missing data, cannot put in propensity scores into the dataset directly.  First need to create a dataset the same size
  ADHDFriendsT1<-ADHDFriends[!(is.na(ADHDFriends$DifficultyAttention)|is.na(ADHDFriends$RepeatGrades)|is.na(ADHDFriends$Suspended)|is.na(ADHDFriends$EasilyBored)|is.na(ADHDFriends$EasilyDistracted)|is.na(ADHDFriends$LowSelfEsteem)|is.na(ADHDFriends$MathSkills)|is.na(ADHDFriends$LowSocialSkills)|is.na(ADHDFriends$GutFeeling)|is.na(ADHDFriends$TroubleRelaxing)|is.na(ADHDFriends$ReadingSkills)|is.na(ADHDFriends$White)|is.na(ADHDFriends$Black)|is.na(ADHDFriends$Sex)|is.na(ADHDFriends$HomeLanguage)|is.na(ADHDFriends$MotherEducation)|is.na(ADHDFriends$scid)), ]
  #Now make prop scores
  ADHDFriendsT1$Prob_ADHD<-fitted(PropScore, type="response")
  #Now merge back into main dataframe
  ADHDFriends1<-merge(ADHDFriends,ADHDFriendsT1[,c("aid","Prob_ADHD")], by=("aid"), all=TRUE)
  
#Do the same for second wave
  
  attach(friends2)
  friends2 <- transform(friends2, 
                        as.numeric(aid, MF_AID1,MF_AID1,MF_AID2,MF_AID3,MF_AID4,MF_AID5,FF_AID1,FF_AID2,FF_AID3,FF_AID4,FF_AID5))
  #Drop missing data  
  friends2[friends2==55555555|friends2==77777777|friends2==88888888|friends2==99999999|friends2==99959995|friends2=="NA"] <- NA
  detach(friends2)
  #Modify adhd2
  attach(adhd2)  
  #Make ADHD variable
  adhd2$ADHD<-as.numeric(H4ID5L)
  #replace ADHD with value 6
  adhd2$ADHD[adhd2$ADHD==6]<-NA
  #Remove missing ADHD observations
  adhd2<-adhd2[!(is.na(adhd2$ADHD)), ]
  detach(adhd2)
  #merge adhd and friends1
  ADHDFriends2<-merge(friends2,adhd2, by=("aid"))
  #Grab time-invariant variables from ADHDFriends
  ADHDFriends2<-merge(ADHDFriends2,ADHDFriends[,c("aid","Sex","White","Black","HomeLanguage","RepeatGrades","BirthCountry","MotherEducation","scid")], by=("aid"))
  #Make Variables in ADHDFriends
  attach(ADHDFriends2)
  #Convert whole dataset to numeric
  ADHDFriends2<-sapply(ADHDFriends2,as.numeric)
  ADHDFriends2<-transform(ADHDFriends2,
                         ReadingSkills=ifelse((H2ED7)>4,NA,(H2ED7)),
                         MathSkills=ifelse((H2ED8)>4,NA,(H2ED8)),
                         LowSocialSkills=ifelse((H2PF26)>5,NA,(H2PF26)),
                         LowSelfEsteem1=ifelse((H2PF21)>5,NA,(H2PF21)),
                         LowSelfEsteem2=ifelse((H2PF23)>5,NA,(H2PF23)),
                         LowSelfEsteem3=ifelse((H2PF24)>5,NA,(H2PF24)),
                         LowSelfEsteem=sum(c(H2PF21,H2PF23,H2PF24))/3,
#                         Rejected=ifelse((H1ED18)>5,NA,(H1ED18)),
#                         ConflictsWithTeachers=ifelse((H1ED15)>5,NA,(H1ED15)),
#                         MoreCig=ifelse((H1TO3)>1,NA,(H1TO3)),
#                         MoreAlc=ifelse((H1TO12)>1,NA,(H1TO12)),
#                         MorePot=ifelse((H1TO32)>1,NA,(H1TO32)),
                         Suspended=ifelse((H2ED3)>1,NA,(H2ED3)),
                         TroubleRemembering=ifelse((H2BC3)>5,NA,(H2BC3)),
                         AvoidHomework=ifelse((H2ED13)>5,NA,(H2ED13)),
                         Procrastinates=ifelse((H2FS18)>5,NA,(H2FS18)),
                         GutFeeling=ifelse((H2PF15)>5,NA,(H2PF15)),
                         DifficultyAttention=ifelse((H2ED12)>5,NA,(H2ED12)),
                         EasilyDistracted=ifelse((H2FS5)>4,NA,(H2FS5)),
                         EasilyBored=ifelse((H2IR19)>2,NA,(H2IR19)),
                         TroubleRelaxing=ifelse((H2GH24)>5,NA,(H2GH24))
  )           
  #tail(colnames(ADHDFriends1))
  #Create Propensity scores for having ADHD
  #install.packages("lme4")
  #Create propensity scores, first step runs the model
  PropScore<-glm(ADHD~DifficultyAttention+RepeatGrades+Suspended+EasilyBored+EasilyDistracted+LowSelfEsteem+MathSkills+LowSocialSkills+GutFeeling+TroubleRelaxing+ReadingSkills+White+Black+Sex+HomeLanguage+MotherEducation+factor(scid),
                 data = ADHDFriends2, family = "binomial"(link="logit"), na.action=na.omit )
  detach(ADHDFriends2)
  #Since propensity scores omit missing data, cannot put in propensity scores into the dataset directly.  First need to create a dataset the same size
  ADHDFriendsT2<-ADHDFriends2[!(is.na(ADHDFriends2$DifficultyAttention)|is.na(ADHDFriends2$RepeatGrades)|is.na(ADHDFriends2$Suspended)|is.na(ADHDFriends2$EasilyBored)|is.na(ADHDFriends2$EasilyDistracted)|is.na(ADHDFriends2$LowSelfEsteem)|is.na(ADHDFriends2$MathSkills)|is.na(ADHDFriends2$LowSocialSkills)|is.na(ADHDFriends2$GutFeeling)|is.na(ADHDFriends2$TroubleRelaxing)|is.na(ADHDFriends2$ReadingSkills)|is.na(ADHDFriends2$White)|is.na(ADHDFriends2$Black)|is.na(ADHDFriends2$Sex)|is.na(ADHDFriends2$HomeLanguage)|is.na(ADHDFriends2$MotherEducation)|is.na(ADHDFriends2$scid)), ]
  #Now make prop scores
  ADHDFriendsT2$Prob_ADHD<-fitted(PropScore, type="response")
  #Now merge back into main dataframe
  ADHDFriends2<-merge(ADHDFriends2,ADHDFriendsT2[,c("aid","Prob_ADHD")], by=("aid"), all=TRUE)
  
  
  
  
  #For Sienna
  #Remove people missing any of key ADHD Criteria
  ADHDFriendsT1<-ADHDFriends1[!(is.na(ADHDFriends1$DifficultyAttention)|is.na(ADHDFriends1$EasilyBored)|is.na(ADHDFriends1$TroubleRelaxing)|is.na(ADHDFriends1$EasilyDistracted)|is.na(ADHDFriends2$GutFeeling)),]
  ADHDFriendsT2<-ADHDFriends2[!(is.na(ADHDFriends2$DifficultyAttention)|is.na(ADHDFriends2$EasilyBored)|is.na(ADHDFriends2$TroubleRelaxing)|is.na(ADHDFriends2$EasilyDistracted)|is.na(ADHDFriends2$GutFeeling)),] 
  #Create ADHD scale based on core ADHD variables
  #ADHDFriendsT1$ADHDScale1<-(ADHDFriendsT1$DifficultyAttention+ADHDFriendsT1$EasilyBored+ADHDFriendsT1$GutFeeling+ADHDFriendsT1$TroubleRelaxing+ADHDFriendsT1$EasilyDistracted+ADHDFriendsT1$Procrastinates)/6
  #ADHDFriendsT2$ADHDScale1<-(ADHDFriendsT2$DifficultyAttention+ADHDFriendsT2$EasilyBored+ADHDFriendsT2$GutFeeling+ADHDFriendsT2$TroubleRelaxing+ADHDFriendsT2$EasilyDistracted+ADHDFriendsT2$Procrastinates)/6
  ADHDFriendsT1$ADHDScale1<-(ADHDFriendsT1$DifficultyAttention+ADHDFriendsT1$EasilyBored+ADHDFriendsT1$TroubleRelaxing+ADHDFriendsT1$EasilyDistracted)/4
  ADHDFriendsT2$ADHDScale1<-(ADHDFriendsT2$DifficultyAttention+ADHDFriendsT2$EasilyBored+ADHDFriendsT2$TroubleRelaxing+ADHDFriendsT2$EasilyDistracted)/4
  #Create Secondary ADHD scale based off worst 5-20%
    prop.table(table(ADHDFriendsT1$DifficultyAttention))
    prop.table(table(ADHDFriendsT1$EasilyBored))
    prop.table(table(ADHDFriendsT1$GutFeeling))
    prop.table(table(ADHDFriendsT2$TroubleRelaxing))
    prop.table(table(ADHDFriendsT1$EasilyDistracted))
    
    ADHDFriendsT1$DifficultyAttention1<-ifelse((ADHDFriendsT1$DifficultyAttention)>2,1,0)
    ADHDFriendsT1$GutFeeling1<-ifelse((ADHDFriendsT1$GutFeeling)>4,1,0)
    ADHDFriendsT1$TroubleRelaxing1<-ifelse((ADHDFriendsT1$TroubleRelaxing)>1,1,0)
    ADHDFriendsT1$EasilyDistracted1<-ifelse((ADHDFriendsT1$EasilyDistracted)>1,1,0)
    ADHDFriendsT1$Procrastinates1<-ifelse((ADHDFriendsT1$Procrastinates)>1,1,0)
    ADHDFriendsT1$H1PF81<-ifelse((ADHDFriendsT1$H1PF8)>3,1,0)
    ADHDFriendsT1$H1PF151<-ifelse((ADHDFriendsT1$H1PF15)>3,1,0)
    ADHDFriendsT1$H1PF181<-ifelse((ADHDFriendsT1$H1PF18)>3,1,0)
    ADHDFriendsT1$H1PF191<-ifelse((ADHDFriendsT1$H1PF19)>3,1,0)
    ADHDFriendsT1$H1PF201<-ifelse((ADHDFriendsT1$H1PF20)>3,1,0)
    #ADHDFriendsT1$ADHDScale2<-(ADHDFriendsT1$DifficultyAttention1+ADHDFriendsT1$EasilyBored+ADHDFriendsT1$GutFeeling1+ADHDFriendsT1$TroubleRelaxing1+ADHDFriendsT1$EasilyDistracted1+ADHDFriendsT1$Procrastinates1)/6
    ADHDFriendsT1$ADHDScale2<-(ADHDFriendsT1$DifficultyAttention1+ADHDFriendsT1$EasilyBored+ADHDFriendsT1$TroubleRelaxing1+ADHDFriendsT1$EasilyDistracted1+ADHDFriendsT1$Procrastinates1)/6
    ADHDFriendsT2$DifficultyAttention1<-ifelse((ADHDFriendsT2$DifficultyAttention)>2,1,0)
    ADHDFriendsT2$GutFeeling1<-ifelse((ADHDFriendsT2$GutFeeling)>4,1,0)
    ADHDFriendsT2$TroubleRelaxing1<-ifelse((ADHDFriendsT2$TroubleRelaxing)>1,1,0)
    ADHDFriendsT2$EasilyDistracted1<-ifelse((ADHDFriendsT2$EasilyDistracted)>1,1,0)
    ADHDFriendsT2$Procrastinates1<-ifelse((ADHDFriendsT2$Procrastinates)>1,1,0)
    #ADHDFriendsT2$ADHDScale2<-(ADHDFriendsT2$DifficultyAttention1+ADHDFriendsT2$EasilyBored+ADHDFriendsT2$GutFeeling1+ADHDFriendsT2$TroubleRelaxing1+ADHDFriendsT2$EasilyDistracted1+ADHDFriendsT2$Procrastinates1)/6
    ADHDFriendsT2$ADHDScale2<-(ADHDFriendsT2$DifficultyAttention1+ADHDFriendsT2$EasilyBored+ADHDFriendsT2$TroubleRelaxing1+ADHDFriendsT2$EasilyDistracted1+ADHDFriendsT2$Procrastinates1)/6
    #Test whether the scale is predictive of ADHD - YES
  summary(glm(ADHD~ADHDScale1,data=ADHDFriendsT1, family="binomial"(link="logit"), na.action=na.omit))
  summary(glm(ADHD~ADHDScale2,data=ADHDFriendsT1, family="binomial"(link="logit"), na.action=na.omit))  
  #Test which factors are least correlated with ADHD
  summary(glm(ADHD~DifficultyAttention+EasilyBored+GutFeeling+TroubleRelaxing+EasilyDistracted+Procrastinates,data=ADHDFriendsT2, family="binomial"(link="logit"), na.action=na.omit))
  summary(glm(ADHD~DifficultyAttention1+EasilyBored+GutFeeling1+TroubleRelaxing1+EasilyDistracted1+Procrastinates1,data=ADHDFriendsT2, family="binomial"(link="logit"), na.action=na.omit))
  #Procrastination is not related to ADHD, and gutfeeling seems to be inversely related to ADHD (e.g. they think they're more cautious).  
  #Maybe it's measuring confidence?  In wave 2, Trouble relaxing is not related to ADHD in wave 2
  summary(glm(ADHD~DifficultyAttention+EasilyBored+GutFeeling+TroubleRelaxing1+EasilyDistracted,data=ADHDFriendsT1, family="binomial"(link="logit"), na.action=na.omit))
  summary(glm(ADHD~DifficultyAttention1+EasilyBored+GutFeeling1+TroubleRelaxing1+EasilyDistracted1,data=ADHDFriendsT1, family="binomial"(link="logit"), na.action=na.omit))
  #Kids with ADHD seem to think they are very analytic (H1PF201)
  
    #Use only saturated schools
    #convert scids to numbers in schinfo
    schinfo$scid <- as.integer(schinfo$scid)
    #Merge school info into ADHDFriends1
    ADHDFriendsC1<-merge(ADHDFriendsT1,schinfo[,c("scid","SAT_SCHL","grades")], by=("scid"))
    #Remove non-saturated schools
    ADHDFriendsC1<-ADHDFriendsC1[ADHDFriendsC1$SAT_SCHL==1,]
    #Remove middle schools
    ADHDFriendsC1<-ADHDFriendsC1[ADHDFriendsC1$grades<6,]
    #Remove grades 7,8 and 12
    ADHDFriendsC1<-ADHDFriendsC1[ADHDFriendsC1$H1GI20>8 & ADHDFriendsC1$H1GI20<12,]
  #as.data.frame(table(ADHDFriendsC1$H1GI20))
    #Make Friend data matrices that match AIDs and exclude missing cases
  ADHDFriendsL1<-ADHDFriendsC1[ADHDFriendsC1$aid %in% ADHDFriendsT2$aid,]
  ADHDFriendsL2<-ADHDFriendsT2[ADHDFriendsT2$aid %in% ADHDFriendsC1$aid,]
  #Sort by aid
  ADHDFriendsL1<-ADHDFriendsL1[order(ADHDFriendsL1$aid),]
  ADHDFriendsL2<-ADHDFriendsL2[order(ADHDFriendsL2$aid),]
  #Remove sent ties that are not in AID
  ADHDFriendsL1<-transform(ADHDFriendsL1,
                          MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                          MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                          MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                          MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                          MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                          FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                          FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                          FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                          FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                          FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
  )
  ADHDFriendsL2<-transform(ADHDFriendsL2,
                           MF_AID1=ifelse(MF_AID1 %in% aid, MF_AID1, NA),
                           MF_AID2=ifelse(MF_AID2 %in% aid, MF_AID2, NA),
                           MF_AID3=ifelse(MF_AID3 %in% aid, MF_AID3, NA),
                           MF_AID4=ifelse(MF_AID4 %in% aid, MF_AID4, NA),
                           MF_AID5=ifelse(MF_AID5 %in% aid, MF_AID5, NA),
                           FF_AID1=ifelse(FF_AID1 %in% aid, FF_AID1, NA),
                           FF_AID2=ifelse(FF_AID2 %in% aid, FF_AID2, NA),
                           FF_AID3=ifelse(FF_AID3 %in% aid, FF_AID3, NA),
                           FF_AID4=ifelse(FF_AID4 %in% aid, FF_AID4, NA),
                           FF_AID5=ifelse(FF_AID5 %in% aid, FF_AID5, NA)
  )


#School 58
  table(ADHDFriendsL1$scid)
  ADHDFriendsM1<-ADHDFriendsL1[ADHDFriendsL1$scid==58,]
  ADHDFriendsM2<-ADHDFriendsL2[ADHDFriendsL2$scid==58,]

  #Remove everyone without any sent or received ties
  ADHDFriendsH1<-ADHDFriendsM1[(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID5)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID5)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID5)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID5) |!(is.na(ADHDFriendsM1$MF_AID1) & is.na(ADHDFriendsM1$MF_AID2) & is.na(ADHDFriendsM1$FF_AID1) & is.na(ADHDFriendsM1$FF_AID2) & is.na(ADHDFriendsM2$MF_AID1) & is.na(ADHDFriendsM2$MF_AID2) & is.na(ADHDFriendsM2$FF_AID1) & is.na(ADHDFriendsM2$FF_AID2)),]
  ADHDFriendsH2<-ADHDFriendsM2[(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID5)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID5)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID5)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID5) |!(is.na(ADHDFriendsM1$MF_AID1) & is.na(ADHDFriendsM1$MF_AID2) & is.na(ADHDFriendsM1$FF_AID1) & is.na(ADHDFriendsM1$FF_AID2) & is.na(ADHDFriendsM2$MF_AID1) & is.na(ADHDFriendsM2$MF_AID2) & is.na(ADHDFriendsM2$FF_AID1) & is.na(ADHDFriendsM2$FF_AID2)),]
  ADHDFriendsM1<-ADHDFriendsH1
  ADHDFriendsM2<-ADHDFriendsH2
  
      #Reset row names0
  rownames(ADHDFriendsM1)<-NULL
  rownames(ADHDFriendsM2)<-NULL
  #Create Friendship Matrices
    #Extract Friendship data
    FriendsM1<-as.matrix(ADHDFriendsM1[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
    FriendsM2<-as.matrix(ADHDFriendsM2[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
    #Create edge list
    FriendsM11<-cbind(FriendsM1[,1], c(FriendsM1[,-1]))
    FriendsM22<-cbind(FriendsM2[,1], c(FriendsM2[,-1]))
    #Make each person tied to themselves (to make models work)
    uniqueaid<-unique(FriendsM11[,1])
    uniqueaid_df<-data.frame(V1=uniqueaid, V2=uniqueaid)
    FriendsM11<-rbind(FriendsM11,uniqueaid_df)
    uniqueaid2<-unique(FriendsM22[,1])
    uniqueaid_df2<-data.frame(V1=uniqueaid2, V2=uniqueaid2)
    FriendsM22<-rbind(FriendsM22,uniqueaid_df2)
    #Drop Missing ties
    FriendsM11<-FriendsM11[!(is.na(FriendsM11$V2)),]
    FriendsM22<-FriendsM22[!(is.na(FriendsM22$V2)),]
  #Create adjacency matrix from edge list
    #install.packages("igraph")
    #M111
    #Create number, starting with 1, for each unique sender value
    FriendsM11<-FriendsM11[order(FriendsM11$V1),]
    rownames(FriendsM11)<-NULL
    uniqueaid<-unique(FriendsM11[,1])
    uniqueaid_df<-data.frame(V1=uniqueaid, V2=rownames(uniqueaid_df))
    FriendsM11<-merge(FriendsM11, uniqueaid_df, by="V1")
    #Reference those numbers to get receiver value
    uniqueaid_df$V3<-uniqueaid_df$V2
    uniqueaid_df$V2.x<-uniqueaid_df$V1
    uniqueaid_df$V1<-NULL
    FriendsM13<-merge(FriendsM11, uniqueaid_df, by="V2.x")
    #HURRAY
    FriendsM13$V3<-FriendsM13$V2.y
    FriendsM13$V2.x<-NULL
    FriendsM13$V1<-NULL
    FriendsM13$V2.y<-NULL
    rownames(FriendsM13)<-NULL
    g=graph.data.frame(FriendsM13, directed=TRUE)
    g1<-simplify(g)
    FriendsM111<-get.adjacency(g1,sparse=FALSE)
    
    #M222
    #Create number, starting with 1, for each unique sender value
    FriendsM22<-FriendsM22[order(FriendsM22$V1),]
    rownames(FriendsM22)<-NULL
    uniqueaid<-unique(FriendsM22[,1])
    uniqueaid_df<-data.frame(V1=uniqueaid, V2=rownames(uniqueaid_df))
    FriendsM22<-merge(FriendsM22, uniqueaid_df, by="V1")
    #Reference those numbers to get receiver value
    uniqueaid_df$V3<-uniqueaid_df$V2
    uniqueaid_df$V2.x<-uniqueaid_df$V1
    uniqueaid_df$V1<-NULL
    FriendsM23<-merge(FriendsM22, uniqueaid_df, by="V2.x")
    #HURRAY
    FriendsM23$V3<-FriendsM23$V2.y
    FriendsM23$V2.x<-NULL
    FriendsM23$V1<-NULL
    FriendsM23$V2.y<-NULL
    rownames(FriendsM23)<-NULL
    g2=graph.data.frame(FriendsM23, directed=TRUE)
    g2<-simplify(g2)
    FriendsM222<-get.adjacency(g2,sparse=FALSE)
                #No longer needed - Remove duplicates of people who actually nominated themselves.
                #table(FriendsM111)
                #FriendsM111[FriendsM111==2]<-1
                #FriendsM222[FriendsM222==2]<-1    
    Geph1<-get.edgelist(g1)
    Geph2<-get.edgelist(g2)
    #For Gephi
    GephiEdges<-data.frame(Source=Geph1[,1], Target=Geph1[,2])
    write.csv(GephiEdges, file="edges.csv")
    #Create a list of unique nodes
    GephiNodes<-data.frame(ID=rownames(ADHDFriendsM1),ADHD=ADHDFriendsM1$ADHD,DifficultyAttention=ADHDFriendsM1$DifficultyAttention, EasilyBored=ADHDFriendsM1$EasilyBored, GutFeeling=ADHDFriendsM1$GutFeeling, TroubleRelaxing=ADHDFriendsM1$TroubleRelaxing, EasilyDistracted=ADHDFriendsM1$EasilyDistracted, TroubleRemembering=ADHDFriendsM1$TroubleRemembering, ADHDScale1=ADHDFriendsM1$ADHDScale1, ADHDScale2=ADHDFriendsM1$ADHDScale2)
    write.csv(GephiNodes, file="nodes.csv")

    #Create longitudinal datasets for key covariate 
    ADHD1<-as.matrix(data.frame(V1=ADHDFriendsM1$ADHD, V2=ADHDFriendsM2$ADHD))
    ADHDScale1<-as.matrix(data.frame(V1=ADHDFriendsM1$ADHDScale1, V2=ADHDFriendsM2$ADHDScale1))
    DifficultyAttention1<-as.matrix(data.frame(V1=ADHDFriendsM1$DifficultyAttention, V2=ADHDFriendsM2$DifficultyAttention))
    EasilyBored1<-as.matrix(data.frame(V1=ADHDFriendsM1$EasilyBored, V2=ADHDFriendsM2$EasilyBored))
    GutFeeling1<-as.matrix(data.frame(V1=ADHDFriendsM1$GutFeeling, V2=ADHDFriendsM2$GutFeeling))
    TroubleRelaxing1<-as.matrix(data.frame(V1=ADHDFriendsM1$TroubleRelaxing, V2=ADHDFriendsM2$TroubleRelaxing))
    EasilyDistracted1<-as.matrix(data.frame(V1=ADHDFriendsM1$EasilyDistracted, V2=ADHDFriendsM2$EasilyDistracted))
    Procrastinates1<-as.matrix(data.frame(V1=ADHDFriendsM1$Procrastinates, V2=ADHDFriendsM2$Procrastinates))

    #Count sample size
    table(ADHDFriendsM1$scid==58)
    #Array
    FriendsArrayT1 <- array( c( FriendsM111, FriendsM222),
                             dim = c( 439, 439, 2 ) )
    
        
#School 77
    table(ADHDFriendsL1$scid)
    ADHDFriendsM1<-ADHDFriendsL1[ADHDFriendsL1$scid==77,]
    ADHDFriendsM2<-ADHDFriendsL2[ADHDFriendsL2$scid==77,]
    
    #Remove everyone without any sent or received ties
    ADHDFriendsH1<-ADHDFriendsM1[(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$MF_AID5)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM1$FF_AID5)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$MF_AID5)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID1)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID2)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID3)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID4)|(ADHDFriendsM1$aid %in% ADHDFriendsM2$FF_AID5) |!(is.na(ADHDFriendsM1$MF_AID1) & is.na(ADHDFriendsM1$MF_AID2) & is.na(ADHDFriendsM1$FF_AID1) & is.na(ADHDFriendsM1$FF_AID2) & is.na(ADHDFriendsM2$MF_AID1) & is.na(ADHDFriendsM2$MF_AID2) & is.na(ADHDFriendsM2$FF_AID1) & is.na(ADHDFriendsM2$FF_AID2)),]
    ADHDFriendsH2<-ADHDFriendsM2[(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$MF_AID5)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM1$FF_AID5)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$MF_AID5)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID1)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID2)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID3)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID4)|(ADHDFriendsM2$aid %in% ADHDFriendsM2$FF_AID5) |!(is.na(ADHDFriendsM1$MF_AID1) & is.na(ADHDFriendsM1$MF_AID2) & is.na(ADHDFriendsM1$FF_AID1) & is.na(ADHDFriendsM1$FF_AID2) & is.na(ADHDFriendsM2$MF_AID1) & is.na(ADHDFriendsM2$MF_AID2) & is.na(ADHDFriendsM2$FF_AID1) & is.na(ADHDFriendsM2$FF_AID2)),]
    ADHDFriendsM1<-ADHDFriendsH1
    ADHDFriendsM2<-ADHDFriendsH2
    
    #Reset row names0
    rownames(ADHDFriendsM1)<-NULL
    rownames(ADHDFriendsM2)<-NULL
    #Create Friendship Matrices
    #Extract Friendship data
    FriendsM1<-as.matrix(ADHDFriendsM1[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
    FriendsM2<-as.matrix(ADHDFriendsM2[,c("aid", "MF_AID1","MF_AID2","MF_AID3","MF_AID4","MF_AID5","FF_AID1","FF_AID2","FF_AID3","FF_AID4","FF_AID5")])
    #Create edge list
    FriendsM11<-cbind(FriendsM1[,1], c(FriendsM1[,-1]))
    FriendsM22<-cbind(FriendsM2[, 1], c(FriendsM2[, -1]))
    #Make each person tied to themselves (to make models work)
    uniqueaid<-unique(FriendsM11[,1])
    uniqueaid_df<-data.frame(V1=uniqueaid, V2=uniqueaid)
    FriendsM11<-rbind(FriendsM11,uniqueaid_df)
    uniqueaid2<-unique(FriendsM22[,1])
    uniqueaid_df2<-data.frame(V1=uniqueaid2, V2=uniqueaid2)
    FriendsM22<-rbind(FriendsM22,uniqueaid_df2)
    #Drop Missing ties
    FriendsM11<-FriendsM11[!(is.na(FriendsM11$V2)),]
    FriendsM22<-FriendsM22[!(is.na(FriendsM22$V2)),]
    #Create adjacency matrix from edge list
    #install.packages("igraph")
    #M111
    #Create number, starting with 1, for each unique sender value
    FriendsM11<-FriendsM11[order(FriendsM11$V1),]
    rownames(FriendsM11)<-NULL
    uniqueaid<-unique(FriendsM11[,1])
    uniqueaid_df<-data.frame(V1=uniqueaid, V2=rownames(uniqueaid_df))
    FriendsM11<-merge(FriendsM11, uniqueaid_df, by="V1")
    #Reference those numbers to get receiver value
    uniqueaid_df$V3<-uniqueaid_df$V2
    uniqueaid_df$V2.x<-uniqueaid_df$V1
    uniqueaid_df$V1<-NULL
    FriendsM13<-merge(FriendsM11, uniqueaid_df, by="V2.x")
    #HURRAY
    FriendsM13$V3<-FriendsM13$V2.y
    FriendsM13$V2.x<-NULL
    FriendsM13$V1<-NULL
    FriendsM13$V2.y<-NULL
    rownames(FriendsM13)<-NULL
    g=graph.data.frame(FriendsM13, directed=TRUE)
    g1<-simplify(g)
    FriendsM111<-get.adjacency(g1,sparse=FALSE)
    
    #M222
    #Create number, starting with 1, for each unique sender value
    FriendsM22<-FriendsM22[order(FriendsM22$V1),]
    rownames(FriendsM22)<-NULL
    uniqueaid<-unique(FriendsM22[,1])
    uniqueaid_df<-data.frame(V1=uniqueaid, V2=rownames(uniqueaid_df))
    FriendsM22<-merge(FriendsM22, uniqueaid_df, by="V1")
    #Reference those numbers to get receiver value
    uniqueaid_df$V3<-uniqueaid_df$V2
    uniqueaid_df$V2.x<-uniqueaid_df$V1
    uniqueaid_df$V1<-NULL
    FriendsM23<-merge(FriendsM22, uniqueaid_df, by="V2.x")
    #HURRAY
    FriendsM23$V3<-FriendsM23$V2.y
    FriendsM23$V2.x<-NULL
    FriendsM23$V1<-NULL
    FriendsM23$V2.y<-NULL
    rownames(FriendsM23)<-NULL
    g2=graph.data.frame(FriendsM23, directed=TRUE)
    g2<-simplify(g2)
    FriendsM222<-get.adjacency(g2,sparse=FALSE)
    #No longer needed - Remove duplicates of people who actually nominated themselves.
    #table(FriendsM111)
    #FriendsM111[FriendsM111==2]<-1
    #FriendsM222[FriendsM222==2]<-1    
    Geph1<-get.edgelist(g1)
    Geph2<-get.edgelist(g2)
    #For Gephi
    GephiEdges<-data.frame(Source=Geph1[,1], Target=Geph1[,2])
    write.csv(GephiEdges, file="edges.csv")
    #Create a list of unique nodes
    GephiNodes<-data.frame(ID=rownames(ADHDFriendsM1),ADHD=ADHDFriendsM1$ADHD,DifficultyAttention=ADHDFriendsM1$DifficultyAttention, EasilyBored=ADHDFriendsM1$EasilyBored, GutFeeling=ADHDFriendsM1$GutFeeling, TroubleRelaxing=ADHDFriendsM1$TroubleRelaxing, EasilyDistracted=ADHDFriendsM1$EasilyDistracted, TroubleRemembering=ADHDFriendsM1$TroubleRemembering, ADHDScale1=ADHDFriendsM1$ADHDScale1, ADHDScale2=ADHDFriendsM1$ADHDScale2)
    write.csv(GephiNodes, file="nodes.csv")
    
    #Create longitudinal datasets for key covariate 
    ADHD2<-as.matrix(data.frame(V1=ADHDFriendsM1$ADHD, V2=ADHDFriendsM2$ADHD))
    ADHDScale2<-as.matrix(data.frame(V1=ADHDFriendsM1$ADHDScale1, V2=ADHDFriendsM2$ADHDScale1))
    DifficultyAttention2<-as.matrix(data.frame(V1=ADHDFriendsM1$DifficultyAttention, V2=ADHDFriendsM2$DifficultyAttention))
    EasilyBored2<-as.matrix(data.frame(V1=ADHDFriendsM1$EasilyBored, V2=ADHDFriendsM2$EasilyBored))
    GutFeeling2<-as.matrix(data.frame(V1=ADHDFriendsM1$GutFeeling, V2=ADHDFriendsM2$GutFeeling))
    TroubleRelaxing2<-as.matrix(data.frame(V1=ADHDFriendsM1$TroubleRelaxing, V2=ADHDFriendsM2$TroubleRelaxing))
    EasilyDistracted2<-as.matrix(data.frame(V1=ADHDFriendsM1$EasilyDistracted, V2=ADHDFriendsM2$EasilyDistracted))
    Procrastinates2<-as.matrix(data.frame(V1=ADHDFriendsM1$Procrastinates, V2=ADHDFriendsM2$Procrastinates))
    #Count sample size
    table(ADHDFriendsM1$scid==58)
    #Array
    FriendsArrayT2 <- array( c( FriendsM111, FriendsM222),
                             dim = c( 600, 600, 2 ) )
    
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
