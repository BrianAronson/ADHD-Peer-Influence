#Create Propensity scores for having ADHD
  #Run the model
  PropScore<-glm(ADHD~DifficultyAttention+Suspended+EasilyBored+EasilyDistracted+LowSelfEsteem+MathSkills+LowSocialSkills+GutFeeling+TroubleRelaxing+ReadingSkills+White+Black+Sex+HomeLanguage+MotherEducation+factor(scid),
                 data = ADHDFriends, family = "binomial"(link="logit"), na.action=na.omit )
  PropScore<-glm(ADHD~DifficultyAttention+Suspended+EasilyBored+EasilyDistracted+LowSelfEsteem+MathSkills+LowSocialSkills+GutFeeling+TroubleRelaxing+ReadingSkills+White+Black+Sex+HomeLanguage+MotherEducation+factor(scid),
                 data = ADHDFriends2, family = "binomial"(link="logit"), na.action=na.omit )

#Since propensity scores omit missing data, cannot put in propensity scores into the dataset directly.  First need to create a dataset the same size
  ADHDFriendsT1<-ADHDFriends[!(is.na(ADHDFriends$DifficultyAttention)|is.na(ADHDFriends$Suspended)|is.na(ADHDFriends$EasilyBored)|is.na(ADHDFriends$EasilyDistracted)|is.na(ADHDFriends$LowSelfEsteem)|is.na(ADHDFriends$MathSkills)|is.na(ADHDFriends$LowSocialSkills)|is.na(ADHDFriends$GutFeeling)|is.na(ADHDFriends$TroubleRelaxing)|is.na(ADHDFriends$ReadingSkills)|is.na(ADHDFriends$White)|is.na(ADHDFriends$Black)|is.na(ADHDFriends$Sex)|is.na(ADHDFriends$HomeLanguage)|is.na(ADHDFriends$MotherEducation)|is.na(ADHDFriends$scid)), ]
  ADHDFriendsT2<-ADHDFriends2[!(is.na(ADHDFriends2$DifficultyAttention)|is.na(ADHDFriends2$Suspended)|is.na(ADHDFriends2$EasilyBored)|is.na(ADHDFriends2$EasilyDistracted)|is.na(ADHDFriends2$LowSelfEsteem)|is.na(ADHDFriends2$MathSkills)|is.na(ADHDFriends2$LowSocialSkills)|is.na(ADHDFriends2$GutFeeling)|is.na(ADHDFriends2$TroubleRelaxing)|is.na(ADHDFriends2$ReadingSkills)|is.na(ADHDFriends2$White)|is.na(ADHDFriends2$Black)|is.na(ADHDFriends2$Sex)|is.na(ADHDFriends2$HomeLanguage)|is.na(ADHDFriends2$MotherEducation)|is.na(ADHDFriends2$scid)), ]
  
#Make prop scores
  ADHDFriendsT1$Prob_ADHD<-fitted(PropScore, type="response")
  ADHDFriendsT2$Prob_ADHD<-fitted(PropScore, type="response")
  
#Merge back into main dataframe
  ADHDFriends<-merge(ADHDFriends,ADHDFriendsT1[,c("aid","Prob_ADHD")], by=("aid"), all=TRUE)
  ADHDFriends2<-merge(ADHDFriends2,ADHDFriendsT2[,c("aid","Prob_ADHD")], by=("aid"), all=TRUE)

#Create ADHD scale based on core ADHD variables
  ADHDFriends$ADHDScale1<-(ADHDFriends$DifficultyAttention+ADHDFriends$EasilyBored+ADHDFriends$TroubleRelaxing+ADHDFriends$EasilyDistracted)/4
  ADHDFriends2$ADHDScale1<-(ADHDFriends2$DifficultyAttention+ADHDFriends2$EasilyBored+ADHDFriends2$TroubleRelaxing+ADHDFriends2$EasilyDistracted)/4
  
#Create Secondary ADHD scale based off worst 5-20%
  ADHDFriends$DifficultyAttention1<-ifelse((ADHDFriends$DifficultyAttention)>2,1,0)
  ADHDFriends$TroubleRelaxing1<-ifelse((ADHDFriends$TroubleRelaxing)>1,1,0)
  ADHDFriends$EasilyDistracted1<-ifelse((ADHDFriends$EasilyDistracted)>1,1,0)
  ADHDFriends$ADHDScale2<-(ADHDFriends$DifficultyAttention1+ADHDFriends$EasilyBored+ADHDFriends$TroubleRelaxing1+ADHDFriends$EasilyDistracted1+ADHDFriends$Procrastinates1)/6
  
  ADHDFriends2$DifficultyAttention1<-ifelse((ADHDFriends2$DifficultyAttention)>2,1,0)
  ADHDFriends2$TroubleRelaxing1<-ifelse((ADHDFriends2$TroubleRelaxing)>1,1,0)
  ADHDFriends2$EasilyDistracted1<-ifelse((ADHDFriends2$EasilyDistracted)>1,1,0)
  ADHDFriends2$ADHDScale2<-(ADHDFriends2$DifficultyAttention1+ADHDFriends2$EasilyBored+ADHDFriends2$TroubleRelaxing1+ADHDFriends2$EasilyDistracted1+ADHDFriends2$Procrastinates1)/6
#Test which the scale is predictive of ADHD
  #summary(glm(ADHD~ADHDScale1,data=ADHDFriends, family="binomial"(link="logit"), na.action=na.omit))
  #summary(glm(ADHD~ADHDScale2,data=ADHDFriends, family="binomial"(link="logit"), na.action=na.omit))  
