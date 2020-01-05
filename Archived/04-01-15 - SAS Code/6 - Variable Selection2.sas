
Data ADHDFriends2;
set ADHDFriends2;
*Outcomes;
Readingskills= 1*H2ED7;
if Readingskills>4 then Readingskills=.;
MathSkills= 1*H2ED8;
if MathSkills>4 then MathSkills=.;
LowSocialSkills= 1*H2PF26;
if LowSocialSkills>5 then LowSocialSkills=.;
if H2PF21>5 then H2PF21=.;
if H2PF23>5 then H2PF23=.;
if H2PF24>5 then H2PF24=.;
LowSelfEsteem=(H2PF21+H2PF23+H2PF24)/3;
*Network;
Suspended= 1*H2ED3;
if Suspended>1 then Suspended=.;
*Confounders:; 
*DSM/Criteria/Questionaire:;
TroubleRemembering=1*H2BC3;
if H2BC3>5 then TroubleRemembering=.;
AvoidHomework=1*H2ED13 ;
if AvoidHomework>5 then AvoidHomework=.;
TroubleFinishing=1*H2FS18;
if H2FS18>5 then TroubleFinishing=.;
GutFeeling=1*H2PF15;
if GutFeeling>5 then GutFeeling=.;
DifficultyAttention=1*H2ED12;
if H2ED12>5 then DifficultyAttention=.;
EasilyDistracted=1*H2FS5;
if H2FS5>4 then EasilyDistracted=.;
EasilyBored=1*H2IR19; 
if H2IR19>2 then EasilyBored=.;
TroubleRelaxing=1*H2GH24;
if H2GH24>5 then TroubleRelaxing=.;
run;

proc logistic data=work.ADHDFriends2 descending ;
class SCIDr;
model ADHD = EasilyBored DifficultyAttention GutFeeling TroubleRelaxing EasilyDistracted TroubleRemembering AvoidHomework SCIDr;
run;

*
