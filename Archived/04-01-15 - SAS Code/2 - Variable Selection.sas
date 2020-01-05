*Needed variables:
SpecialEducation
More anxiety
Worse at sharing, cooperating, and turn taking
Trouble finishing projects
Difficulty with organization-related tasks
Fidget/squirm
Talks too much in social situations
Finish other's conversations
Difficulty waiting turn to talk
Interupt others who are busy

*Scales;
Proc Corr Data=ADHDFriends Alpha nomiss;
	var H1PF32 H1PF33 ;
run;

Data ADHDFriends;
set ADHDFriends;
*Outcomes;
array STDs (10) H1CO16A H1CO16B H1CO16C H1CO16D H1CO16E H1CO16F H1CO16G H1CO16H H1CO16I H1CO16J;
	do i=1 to 10;
	if STDs(i)>1 then STDs(i)=.;
	end;
Readingskills= 1*H1ED11;
if Readingskills>4 then Readingskills=.;
MathSkills= 1*H1ED12;
if MathSkills>4 then MathSkills=.;
RepeatGrades= 1*H1ED5;
if RepeatGrades>1 then RepeatGrades=.;
LowSocialSkills= 1*H1PF35;
if LowSocialSkills>5 then LowSocialSkills=.;
Depression= 1*H1FS6;
if H1PF32>5 then H1PF32=.;
if H1PF33>5 then H1PF33=.;
if H1PF30>5 then H1PF30=.;
LowSelfEsteem=(H1PF32+H1PF33+H1PF30)/3;
if Depression>3 then Depression=.;
DropOut= 1*H1GI18;
*Network;
Rejected=1*H1ED18; 
if Rejected>5 then Rejected=.;
FewCloseFriends= 1*H1DA7;
ConflictsWithTeachers= 1*H1ED15;
if ConflictsWithTeachers>5 then ConflictsWithTeachers=.;
ConflictsFamily= 1*H1WS3A;
MoreSexualPartners= 1*H1NR6; /*or H1CO1 */
if MoreSexualPartners>500 then MoreSexualPartners=.;
else if MoreSexualPartners>5 then MoreSexualPartners=5;
MorePregnancies= 1*H1FP8; 
MoreCig= 1*H1TO3;
if MoreCig>1 then MoreCig=.;
MoreAlc=1*H1TO12; 
if MoreAlc>1 then MoreAlc=.;
MorePot=1*H1TO32;
If MorePot=0 then MorePot=0;
else if MorePot<801 then MorePot=1;
Else MorePot=.;
Married= 1*H1GI15;
Suspended= 1*H1ED7;
if Suspended>1 then Suspended=.;
*Confounders:; 
Sex=1*BIO_SEX;
White=1*H1GI6A;
Black=1*H1GI6B;
Native=1*H1GI6C; 
Asian=1*H1GI6D;
Birthmonth=1*H1GI1M; 
Birthyear=1*H1GI1Y;
Grade=1*H1GI20;
HomeLanguage=1*H1GI10;
if HomeLanguage=3 then HomeLanguage=2;
if HomeLanguage>3 then HomeLanguage=.;
BirthCountry=1*H1GI11;
if H1GI11>2 then BirthCountry=.;
NewHome=1*H1GI3;
Grade=1*H1GI20;
Counseling=1*H1HS3;
if H1HS3>2 then Counseling=.;
MotherEducation=1*H1RM1;
if MotherEducation>9 then MotherEducation=.;
FatherEducation=1*H1RF1;
if FatherEducation>9 then FatherEducation=.;
*DSM/Criteria/Questionaire:;
HelpAround= 1*H1DA1 ;
TroubleRemembering=1*H1BC3;
if H1BC3>5 then TroubleRemembering=.;
AvoidHomework=1*H1ED17 ;
if AvoidHomework>5 then AvoidHomework=.;
TroubleFinishing=1*H1FS18;
if H1FS18>5 then TroubleFinishing=.;
GutFeeling=1*H1PF16;
if GutFeeling>5 then GutFeeling=.;
TroublePlanning=1*H1SE2;
if TroublePlanning>5 then TroublePlanning=.;
DifficultyAttention=1*H1ED16;
if H1ED16>5 then DifficultyAttention=.;
EasilyDistracted=1*H1FS5;
if H1FS5>4 then EasilyDistracted=.;
EasilyBored=1*H1IR19; 
if H1IR19>2 then EasilyBored=.;
HighEnergy=1*H1PF26;
TroubleRelaxing=1*H1GH19;
if H1GH19>5 then TroubleRelaxing=.;
run;

*Outcomes;
proc means;
class ADHD;
var Readingskills MathSkills RepeatGrades LowSocialSkills Depression DropOut;  
weight GSWGT1;
run;
*Network;
proc means;
class ADHD;
var Rejected FewCloseFriends ConflictsWithTeachers ConflictsFamily MoreSexualPartners MorePregnancies MoreCig MoreAlc MorePot Married Suspended;  
weight GSWGT1;
run;
*Confounders;
proc means;
class ADHD;
var Sex White Black Native Asian Birthmonth Birthyear Grade HomeLanguage BirthCountry NewHome Grade Counseling MotherEducation FatherEducation;  
weight GSWGT1;
run;
*DSM/Criteria/Questionaire;
proc means;
class ADHD;
var HelpAround TroubleRemembering AvoidHomework TroubleFinishing GutFeeling TroublePlanning DifficultyAttention EasilyDistracted EasilyBored HighEnergy TroubleRelaxing;
weight GSWGT1;
run;

*scales;
proc means;
class ADHD;
var LowSelfEsteem H1PF32 H1PF33 H1PF30;
weight GSWGT1;
run;


*Good Vars:

Outcomes:
Readingskills
MathSkills
RepeatGrades
LowSocialSkills

Network:
Rejected
ConflictsWithTeachers
MoreCig
MoreAlc
MorePot
Suspended

Confounders:
Sex
White
Black
HomeLanguage
BirthCountry
MotherEducation
FatherEducation

DSM Criteria:
*TroubleRemembering - Trouble remembering appointments/obligations - H1BC3/H2BC3 - It takes too much planning ahead of time to have birth control on hand
AvoidHomework - Avoid starting projects that require thought - H1ED17/H2ED13 - trouble getting your homework done
TroubleFinishing - Avoid starting projects that require thought - H1FS1/H2FS18 - It was hard to get started doing things
GutFeeling - Active/compelled to do things - H1PF16/H2PF15 - When making decisions, you usually go with your “gut feeling”
DifficultyAttention - Difficulty paying attention during boring work - H1ED16/H2ED12 - trouble paying attention in school
EasilyDistracted - Easily distracted by noise or activity - H1FS5/H2FS5 - You had trouble keeping your mind on what you were doing
EasilyBored - Leave seats during meetings when you shouldn't - H1IR19/H2IR19 - Did the respondent ever seem bored or impatient during the interview?
TroubleRelaxing  - Difficulty Unwinding/Relaxing - H1GH19/H2GH24 - trouble relaxing

Other Propensity Score Criteria
Suspended  - H1ED7/H2ED3
LowSelfEsteem - You have a lot of good qualities, You have a lot to be proud of You like yourself just the way you are.  - H1PF32/H2PF21+H1PF33/H2PF23+H1PF30/H2PF24)/3 
MathSkills - And what was your grade in mathematics? - H1ED12/H2ED8
LowSocialSkills - You feel socially accepted - H1PF35/H2PF26
readingskills - what was your grade in English or language arts - H1ED11/H2ED7

*from Wave1
White Black Sex HomeLanguage MotherEducation RepeatGrades SCID;


*Check all variables;
proc means;
class adhd;
var AvoidHomework TroubleFinishing GutFeeling DifficultyAttention EasilyDistracted EasilyBored TroubleRelaxing Rejected ConflictsWithTeachers MoreCig MoreAlc MorePot Suspended Readingskills MathSkills RepeatGrades LowSocialSkills LowSelfEsteem;
run;

*full model;
 *proc logistic descending;
 *model ADHD = AvoidHomework TroubleFinishing GutFeeling DifficultyAttention EasilyDistracted EasilyBored TroubleRelaxing Rejected ConflictsWithTeachers MoreAlc Suspended Readingskills MathSkills RepeatGrades LowSocialSkills LowSelfEsteem;
 *run;
*stepwise;
*DifficultyAttention RepeatGrades Suspended EasilyBored LowSocialSkills GutFeeling TroubleRelaxing readingskills White Black Sex HomeLanguage MotherEducation;
*proc logistic;
*class SCID;
*model ADHD(event='1')= AvoidHomework TroubleFinishing GutFeeling DifficultyAttention EasilyDistracted EasilyBored TroubleRelaxing Rejected ConflictsWithTeachers MoreAlc Suspended Readingskills MathSkills RepeatGrades LowSocialSkills LowSelfEsteem SCID White Black Sex HomeLanguage MotherEducation
				 / selection=stepwise
                 slentry=0.9
                 slstay=0.05
                 details
                 lackfit;
*run;

proc logistic data=work.ADHDFriends descending;
class SCID;
model ADHD = DifficultyAttention EasilyBored GutFeeling TroubleRelaxing EasilyDistracted TroubleRemembering AvoidHomework;
run;
