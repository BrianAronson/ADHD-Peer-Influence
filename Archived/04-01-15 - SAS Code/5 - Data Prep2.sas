data work.Friends2;
 set AdHd.Friends2; *Wave2 in home friends file, preconverted to SAS;
 aidr=aid*1;
run;
proc sort data=Friends2 out=Friends2 tagsort;
 *by AID;
 by aidr;
 run;

/*CHECK THIS*/
data friends2;
 set friends2;
   array frnds (10) mf_aid1 mf_aid2 mf_aid3 mf_aid4 mf_aid5 ff_aid1 ff_aid2 ff_aid3 ff_aid4 ff_aid5; 
   array frndsnum (10) mf_aid1n mf_aid2n mf_aid3n mf_aid4n mf_aid5n ff_aid1n ff_aid2n ff_aid3n ff_aid4n ff_aid5n; 
    do i=1 to 10;
	   frndsnum(i)=frnds(i)*1;
	   end;
	****Optional - Remove 2nd-5th nominations;
	*do i=2 to 5;
	*   frndsnum(i)=.;
	*   end;
	*do i=7 to 10;
	*   frndsnum(i)=.;
	*   end;
	****;
	drop mf_aid1 mf_aid2 mf_aid3 mf_aid4 mf_aid5 ff_aid1 ff_aid2 ff_aid3 ff_aid4 ff_aid5;
run;


data Friends2; /* drop missing data */
 set Friends2;
   array frnds mf_aid1n mf_aid2n mf_aid3n mf_aid4n mf_aid5n ff_aid1n ff_aid2n ff_aid3n ff_aid4n ff_aid5n; 
    do over frnds;
		if frnds = 55555555 then frnds = .; /* nominated friend was also nominated as one of the partners */;
		if frnds = 77777777 then frnds = .; /* nominations to another school */;
	   	if frnds = 88888888 then frnds = .; /* goes to sister school, not in directory */;
	   	if frnds = 99999999 then frnds = .; /* not found in the directory */;
	   	if frnds = 99959995 then frnds = .; /* bad nomination, miskeyed, etc. */;
		*frnds=frnds*1; *convert to number; 
		*frnds=put(frnds,5.); *convert to character;
	end;
 run;
*merge friend network and ADHD diagnosis;
*Will start a macro for each school ID here;

Data ADHDFriends2;
	netid=1;
	merge work.Friends2 adhd.adhd2; *adhd.adhd2 refers to a SAS file with three variables - 
	wave 4 AID, ADHD diagnosis (to be compared/replaced with diagnosis based on propensity scores, and wave 1 SCID;
	by aidr; /* NOTE YOU NEED A BY FOR A MERGE! */
  run;

data adhdfriends2;
 set adhdfriends2;
	ADHD=H4ID5L;
	drop H4ID5L;
	if ADHD=6 then ADHD=.;
	SCIDR=SCID*1; /* can't convert by writing over, need a new name */	
	*AID=AID*1; *convert to number;
	*AID=put(AID,5.); *convert to character;
	if ADHD=. then delete; *remove people who did not complete wave 4 or who did not respond to question;
run;
*Select variables from wave 1 to bring into wave2;
Data ADHDFriends3;
set ADHDFriends (keep=White Black Sex HomeLanguage MotherEducation RepeatGrades SCID AIDR FR_flag);
SCIDr=1*SCID;
drop SCID;
run;
*Merge variables into wave 2;
Data ADHDFriends2;
merge ADHDFriends2 ADHDFriends3;
by aidr;
****Optional - remove those who only nominated one friend;
*if fr_flag=1 then delete;
****;
run;
