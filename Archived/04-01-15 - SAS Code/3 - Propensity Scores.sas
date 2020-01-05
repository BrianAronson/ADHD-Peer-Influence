proc logistic data=work.ADHDFriends descending;
class SCID;
model ADHD = DifficultyAttention RepeatGrades Suspended EasilyBored EasilyDistracted LowSelfEsteem MathSkills LowSocialSkills GutFeeling TroubleRelaxing readingskills White Black Sex HomeLanguage MotherEducation SCID;
*DifficultyAttention RepeatGrades Suspended EasilyBored LowSocialSkills GutFeeling TroubleRelaxing readingskills LowSelfEsteem White Black Sex HomeLanguage MotherEducation;
 output out=ADHDFriends
 pred = PROB_ADHD;
 run;
 proc means;
 class ADHD; 
 var prob_ADHD;
 run;
Data PropScoresRounded;
set ADHDFriends;
RoundPropADHD=round(prob_ADHD,.05);
run;
proc sort data=PropScoresRounded;
by ADHD;
run;

proc freq data=PropScoresRounded;
tables RoundPropADHD;
by ADHD;
run;
