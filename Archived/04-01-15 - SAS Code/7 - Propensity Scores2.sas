proc logistic data=work.ADHDFriends2 descending;
class SCIDr;
model ADHD = DifficultyAttention RepeatGrades Suspended EasilyBored EasilyDistracted LowSelfEsteem MathSkills LowSocialSkills GutFeeling TroubleRelaxing readingskills White Black Sex HomeLanguage MotherEducation SCIDr;
*DifficultyAttention RepeatGrades Suspended EasilyBored easily LowSocialSkills GutFeeling TroubleRelaxing readingskills LowSelfEsteem White Black Sex HomeLanguage MotherEducation;
 output out=ADHDFriends2
 pred = PROB_ADHD;
 run;
 proc means;
 class ADHD; 
 var prob_ADHD;
 run;
Data PropScoresRounded2;
set ADHDFriends;
RoundPropADHD=round(prob_ADHD,.05);
run;
proc sort data=PropScoresRounded2;
by ADHD;
run;

proc freq data=PropScoresRounded2;
tables RoundPropADHD;
by ADHD;
run;
