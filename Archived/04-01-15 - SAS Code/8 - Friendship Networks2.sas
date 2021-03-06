%let datdir = C:\Users\bdaro_000\Sociology\728 - Networks\Week 4\; /* data directory */
%let outdir = C:\Users\bdaro_000\Sociology\728 - Networks\Week 4\; /* where you want to write files */
%let moddir = C:\Users\bdaro_000\Sociology\728 - Networks\Week 4\span\; /* SPAN modules */
%let macdir = C:\Users\bdaro_000\Sociology\728 - Networks\Week 4\span\; /* SPAN macros */

%include "&macdir.ego_xmat.mac";
%include "&macdir.words.mac";
%include "&macdir.numobs.mac";



Data ADHDFriends006;
	set Adhdfriends2;
	if SCIDR>45 then delete; *just look at first 9 schools in sample.  If I use much more, I get errors related to running out of memory;
run;
Data ADHDFriends007;
	set Adhdfriends2;
	if SCIDR>75 | SCIDR<46 then delete; *just look at subset of schools.  If I use much more, I get errors related to running out of memory;
run;
Data ADHDFriends008;
	set Adhdfriends2;
	if SCIDR>130 | SCIDR<76 then delete; *just look at subset of schools.  If I use much more, I get errors related to running out of memory;
run;
Data ADHDFriends009;
	set Adhdfriends2;
	if SCIDR<131 then delete; *just look at subset of schools.  If I use much more, I get errors related to running out of memory;
run;


*FIRST SET OF SCHOOLS;


proc iml;
    %include "&moddir.adj.mod"; /* this points to your SPAN directory */
	%include "&moddir.pajwrite.mod"; /* ditto */
	%include "&moddir.pajpart.mod"; /* ditto */

	use work.ADHDFriends006;
	read all var{AIDR} into sndr;
	read all var{mf_aid1n mf_aid2n mf_aid3n mf_aid4n mf_aid5n}
	  into mnoms; /* male friends */
	read all var{ff_aid1n ff_aid2n ff_aid3n ff_aid4n ff_aid5n} into fnoms; /* female friends */
	noms=mnoms||fnoms; /* you could, of course, skip this line and modify below for 
						   only male or female networks */
	read all var{adhd}into adhd;

	adjmat=adj(sndr,noms); /* create the adjacency matrix, using the SPAN module */
	adjid=adjmat[,1]; /* pull off the id variable, which is in the first column */

	adjmat=adjmat[,2:ncol(adjmat)]; /* now have a square n by n adj. matrix */

	chk=nrow(adjmat);
	/* need to pull out people who are not also sampled. These are kids who were nominated, 
       but not sampled in the survey. The next lines creates a vector, called sampled,
       that tells us who is in the sample. */

	sampled=j(nrow(adjmat),1,0);
	do i=1 to nrow(adjid); /* look over every person in the network */
	  iloc=loc(sndr=adjid[i]); /* see if you can find them in the AID matrix */
	  if type(iloc)='N' then do; 
	     sampled[i]=1; /* if so, they were sampled, change value to 1 */
		 free iloc;
	  end;
	end;

keep=loc(sampled=1); /* reduce the network to people sampled in the in-home data */
	adjid=adjid[keep,1];
	adjmat=adjmat[keep,keep];

	reset storage=work.tempnet;
	store adjmat adjid; /* store the network, for now, we reload it in the macro below */

   quit;
    %ego_xmat(netid,
		 AIDR, /* nominator ID */
 	     adjmat, /* name of adjacency matrix */ 
         ADHD prob_ADHD DifficultyAttention EasilyBored GutFeeling TroubleRelaxing EasilyDistracted TroubleRemembering AvoidHomework,  /* this is the list of variables I want the means for */
              "SOR",  /* get the 'sent' or 'Recieved' network - S for sent, R for received, SNR for Sent and Received, SOR for sent or received*/
         work.tempnet, /* where you stored the data */
         work.ADHDfriends006,  /* name of the atribute file */
         work.s_emean); /* name of the output file */
		
		 
proc means data=work.s_emean;
run;
/* now we want to calculate some measures on the _structure_ of the ego-network, using 
   the full network data as our input.  Note, that you can calculate many of these 
   using UCINET directly, by exporting the data to UCINET, then going to EGO-NETWORK
   on the window. */

proc iml;
 %include "&moddir.density.mod"; /* program that calculates density */
 reset storage=work.tempnet;
 load adjmat adjid;

 /*to calculate ego-network density, we need to pull out a small adjacency matrix 
   of only the people that ego sends or recieves ties from.  Below I spell out the 
   code, so you can see how it works.  But you could also use egonet.mod to get the 
   same thing. */


  egoden=j(nrow(adjid),1,.); /* make an empty matrix to store results in */

  do i=1 to nrow(adjid);  /* do for each person in the network */
    sendi=loc(adjmat[i,]^=0); /* cols that ego sends to */
	recvi=loc(adjmat[,i]^=0); /* rows that nominate ego */
	if type(sendi)='N' & type(recvi)='N' then do;
	    snr_i=union(sendi,recvi); /* by taking union, we get everyone ego nominates or who nominates
								ego. */
	end;

	else if type(sendi)='N' then do;
	  snr_i=sendi;
	end;

	else if type(recvi)='N' then do;
	  snr_i=recvi;
	end;

	if ncol(snr_i)>1 then do; /* if they are connected to more than one person */
	   submati=adjmat[snr_i,snr_i];
	   deni=density(submati);
	   egoden[i]=deni;
	end;
  end;

  outv=adjid||egoden; /* make one matrix with both id and egoden #s */

  create ed_dat from outv [colname={AIDR egoden}];
  append from outv;
quit;
proc means data=ed_dat;
run;
*Variable meanings: 
emadhd: proportion of friends with ADHD
NEADHD: number of alters
egoden: ego-network density
;
data ADHDNetwork6;
 merge ed_dat s_emean ADHDfriends006;
 by AIDR;
run;


*SECOND SET OF SCHOOLS;


proc iml;
    %include "&moddir.adj.mod"; /* this points to your SPAN directory */
	%include "&moddir.pajwrite.mod"; /* ditto */
	%include "&moddir.pajpart.mod"; /* ditto */

	use work.ADHDFriends007;
	read all var{AIDR} into sndr;
	read all var{mf_aid1n mf_aid2n mf_aid3n mf_aid4n mf_aid5n}
	  into mnoms; /* male friends */
	read all var{ff_aid1n ff_aid2n ff_aid3n ff_aid4n ff_aid5n} into fnoms; /* female friends */
	noms=mnoms||fnoms; /* you could, of course, skip this line and modify below for 
						   only male or female networks */
	read all var{adhd}into adhd;

	adjmat=adj(sndr,noms); /* create the adjacency matrix, using the SPAN module */
	adjid=adjmat[,1]; /* pull off the id variable, which is in the first column */

	adjmat=adjmat[,2:ncol(adjmat)]; /* now have a square n by n adj. matrix */

	chk=nrow(adjmat);
	/* need to pull out people who are not also sampled. These are kids who were nominated, 
       but not sampled in the survey. The next lines creates a vector, called sampled,
       that tells us who is in the sample. */

	sampled=j(nrow(adjmat),1,0);
	do i=1 to nrow(adjid); /* look over every person in the network */
	  iloc=loc(sndr=adjid[i]); /* see if you can find them in the AID matrix */
	  if type(iloc)='N' then do; 
	     sampled[i]=1; /* if so, they were sampled, change value to 1 */
		 free iloc;
	  end;
	end;

keep=loc(sampled=1); /* reduce the network to people sampled in the in-home data */
	adjid=adjid[keep,1];
	adjmat=adjmat[keep,keep];

	reset storage=work.tempnet;
	store adjmat adjid; /* store the network, for now, we reload it in the macro below */

   quit;
    %ego_xmat(netid,
		 AIDR, /* nominator ID */
 	     adjmat, /* name of adjacency matrix */ 
         ADHD prob_ADHD DifficultyAttention EasilyBored GutFeeling TroubleRelaxing EasilyDistracted TroubleRemembering AvoidHomework,  /* this is the list of variables I want the means for */
              "SOR",  /* get the 'sent' or 'Recieved' network - S for sent, R for received, SNR for Sent and Received, SOR for sent or received*/
         work.tempnet, /* where you stored the data */
         work.ADHDFriends007,  /* name of the atribute file */
         work.s_emean); /* name of the output file */
		 *This is where I get ERROR: (execution) Invalid subscript or subscript out of range;
 
		 
proc means data=work.s_emean;
run;
/* now we want to calculate some measures on the _structure_ of the ego-network, using 
   the full network data as our input.  Note, that you can calculate many of these 
   using UCINET directly, by exporting the data to UCINET, then going to EGO-NETWORK
   on the window. */

proc iml;
 %include "&moddir.density.mod"; /* program that calculates density */
 reset storage=work.tempnet;
 load adjmat adjid;

 /*to calculate ego-network density, we need to pull out a small adjacency matrix 
   of only the people that ego sends or recieves ties from.  Below I spell out the 
   code, so you can see how it works.  But you could also use egonet.mod to get the 
   same thing. */


  egoden=j(nrow(adjid),1,.); /* make an empty matrix to store results in */

  do i=1 to nrow(adjid);  /* do for each person in the network */
    sendi=loc(adjmat[i,]^=0); /* cols that ego sends to */
	recvi=loc(adjmat[,i]^=0); /* rows that nominate ego */
	if type(sendi)='N' & type(recvi)='N' then do;
	    snr_i=union(sendi,recvi); /* by taking union, we get everyone ego nominates or who nominates
								ego. */
	end;

	else if type(sendi)='N' then do;
	  snr_i=sendi;
	end;

	else if type(recvi)='N' then do;
	  snr_i=recvi;
	end;

	if ncol(snr_i)>1 then do; /* if they are connected to more than one person */
	   submati=adjmat[snr_i,snr_i];
	   deni=density(submati);
	   egoden[i]=deni;
	end;
  end;

  outv=adjid||egoden; /* make one matrix with both id and egoden #s */

  create ed_dat from outv [colname={AIDR egoden}];
  append from outv;
quit;
proc means data=ed_dat;
run;
*Variable meanings: 
emadhd: proportion of friends with ADHD
NEADHD: number of alters
egoden: ego-network density
;
data ADHDNetwork7;
 merge ed_dat s_emean ADHDFriends007;
 by AIDR;
run;



*THIRD SET OF SCHOOLS;


proc iml;
    %include "&moddir.adj.mod"; /* this points to your SPAN directory */
	%include "&moddir.pajwrite.mod"; /* ditto */
	%include "&moddir.pajpart.mod"; /* ditto */

	use work.ADHDFriends008;
	read all var{AIDR} into sndr;
	read all var{mf_aid1n mf_aid2n mf_aid3n mf_aid4n mf_aid5n}
	  into mnoms; /* male friends */
	read all var{ff_aid1n ff_aid2n ff_aid3n ff_aid4n ff_aid5n} into fnoms; /* female friends */
	noms=mnoms||fnoms; /* you could, of course, skip this line and modify below for 
						   only male or female networks */
	read all var{adhd}into adhd;

	adjmat=adj(sndr,noms); /* create the adjacency matrix, using the SPAN module */
	adjid=adjmat[,1]; /* pull off the id variable, which is in the first column */

	adjmat=adjmat[,2:ncol(adjmat)]; /* now have a square n by n adj. matrix */

	chk=nrow(adjmat);
	/* need to pull out people who are not also sampled. These are kids who were nominated, 
       but not sampled in the survey. The next lines creates a vector, called sampled,
       that tells us who is in the sample. */

	sampled=j(nrow(adjmat),1,0);
	do i=1 to nrow(adjid); /* look over every person in the network */
	  iloc=loc(sndr=adjid[i]); /* see if you can find them in the AID matrix */
	  if type(iloc)='N' then do; 
	     sampled[i]=1; /* if so, they were sampled, change value to 1 */
		 free iloc;
	  end;
	end;

keep=loc(sampled=1); /* reduce the network to people sampled in the in-home data */
	adjid=adjid[keep,1];
	adjmat=adjmat[keep,keep];

	reset storage=work.tempnet;
	store adjmat adjid; /* store the network, for now, we reload it in the macro below */

   quit;
    %ego_xmat(netid,
		 AIDR, /* nominator ID */
 	     adjmat, /* name of adjacency matrix */ 
         ADHD prob_ADHD DifficultyAttention EasilyBored GutFeeling TroubleRelaxing EasilyDistracted TroubleRemembering AvoidHomework,  /* this is the list of variables I want the means for */
              "SOR",  /* get the 'sent' or 'Recieved' network - S for sent, R for received, SNR for Sent and Received, SOR for sent or received*/
         work.tempnet, /* where you stored the data */
         work.ADHDFriends008,  /* name of the atribute file */
         work.s_emean); /* name of the output file */
		 *This is where I get ERROR: (execution) Invalid subscript or subscript out of range;
 
		 
proc means data=work.s_emean;
run;
/* now we want to calculate some measures on the _structure_ of the ego-network, using 
   the full network data as our input.  Note, that you can calculate many of these 
   using UCINET directly, by exporting the data to UCINET, then going to EGO-NETWORK
   on the window. */

proc iml;
 %include "&moddir.density.mod"; /* program that calculates density */
 reset storage=work.tempnet;
 load adjmat adjid;

 /*to calculate ego-network density, we need to pull out a small adjacency matrix 
   of only the people that ego sends or recieves ties from.  Below I spell out the 
   code, so you can see how it works.  But you could also use egonet.mod to get the 
   same thing. */


  egoden=j(nrow(adjid),1,.); /* make an empty matrix to store results in */

  do i=1 to nrow(adjid);  /* do for each person in the network */
    sendi=loc(adjmat[i,]^=0); /* cols that ego sends to */
	recvi=loc(adjmat[,i]^=0); /* rows that nominate ego */
	if type(sendi)='N' & type(recvi)='N' then do;
	    snr_i=union(sendi,recvi); /* by taking union, we get everyone ego nominates or who nominates
								ego. */
	end;

	else if type(sendi)='N' then do;
	  snr_i=sendi;
	end;

	else if type(recvi)='N' then do;
	  snr_i=recvi;
	end;

	if ncol(snr_i)>1 then do; /* if they are connected to more than one person */
	   submati=adjmat[snr_i,snr_i];
	   deni=density(submati);
	   egoden[i]=deni;
	end;
  end;

  outv=adjid||egoden; /* make one matrix with both id and egoden #s */

  create ed_dat from outv [colname={AIDR egoden}];
  append from outv;
quit;
proc means data=ed_dat;
run;
*Variable meanings: 
emadhd: proportion of friends with ADHD
NEADHD: number of alters
egoden: ego-network density
;
data ADHDNetwork8;
 merge ed_dat s_emean ADHDFriends008;
 by AIDR;
run;




*FOURTH SET OF SCHOOLS;


proc iml;
    %include "&moddir.adj.mod"; /* this points to your SPAN directory */
	%include "&moddir.pajwrite.mod"; /* ditto */
	%include "&moddir.pajpart.mod"; /* ditto */

	use work.ADHDFriends009;
	read all var{AIDR} into sndr;
	read all var{mf_aid1n mf_aid2n mf_aid3n mf_aid4n mf_aid5n}
	  into mnoms; /* male friends */
	read all var{ff_aid1n ff_aid2n ff_aid3n ff_aid4n ff_aid5n} into fnoms; /* female friends */
	noms=mnoms||fnoms; /* you could, of course, skip this line and modify below for 
						   only male or female networks */
	read all var{adhd}into adhd;

	adjmat=adj(sndr,noms); /* create the adjacency matrix, using the SPAN module */
	adjid=adjmat[,1]; /* pull off the id variable, which is in the first column */

	adjmat=adjmat[,2:ncol(adjmat)]; /* now have a square n by n adj. matrix */

	chk=nrow(adjmat);
	/* need to pull out people who are not also sampled. These are kids who were nominated, 
       but not sampled in the survey. The next lines creates a vector, called sampled,
       that tells us who is in the sample. */

	sampled=j(nrow(adjmat),1,0);
	do i=1 to nrow(adjid); /* look over every person in the network */
	  iloc=loc(sndr=adjid[i]); /* see if you can find them in the AID matrix */
	  if type(iloc)='N' then do; 
	     sampled[i]=1; /* if so, they were sampled, change value to 1 */
		 free iloc;
	  end;
	end;

keep=loc(sampled=1); /* reduce the network to people sampled in the in-home data */
	adjid=adjid[keep,1];
	adjmat=adjmat[keep,keep];

	reset storage=work.tempnet;
	store adjmat adjid; /* store the network, for now, we reload it in the macro below */

   quit;
    %ego_xmat(netid,
		 AIDR, /* nominator ID */
 	     adjmat, /* name of adjacency matrix */ 
         ADHD prob_ADHD DifficultyAttention EasilyBored GutFeeling TroubleRelaxing EasilyDistracted TroubleRemembering AvoidHomework,  /* this is the list of variables I want the means for */
              "SOR",  /* get the 'sent' or 'Recieved' network - S for sent, R for received, SNR for Sent and Received, SOR for sent or received*/
         work.tempnet, /* where you stored the data */
         work.ADHDFriends009,  /* name of the atribute file */
         work.s_emean); /* name of the output file */
		 *This is where I get ERROR: (execution) Invalid subscript or subscript out of range;
 
		 
proc means data=work.s_emean;
run;
/* now we want to calculate some measures on the _structure_ of the ego-network, using 
   the full network data as our input.  Note, that you can calculate many of these 
   using UCINET directly, by exporting the data to UCINET, then going to EGO-NETWORK
   on the window. */

proc iml;
 %include "&moddir.density.mod"; /* program that calculates density */
 reset storage=work.tempnet;
 load adjmat adjid;

 /*to calculate ego-network density, we need to pull out a small adjacency matrix 
   of only the people that ego sends or recieves ties from.  Below I spell out the 
   code, so you can see how it works.  But you could also use egonet.mod to get the 
   same thing. */


  egoden=j(nrow(adjid),1,.); /* make an empty matrix to store results in */

  do i=1 to nrow(adjid);  /* do for each person in the network */
    sendi=loc(adjmat[i,]^=0); /* cols that ego sends to */
	recvi=loc(adjmat[,i]^=0); /* rows that nominate ego */
	if type(sendi)='N' & type(recvi)='N' then do;
	    snr_i=union(sendi,recvi); /* by taking union, we get everyone ego nominates or who nominates
								ego. */
	end;

	else if type(sendi)='N' then do;
	  snr_i=sendi;
	end;

	else if type(recvi)='N' then do;
	  snr_i=recvi;
	end;

	if ncol(snr_i)>1 then do; /* if they are connected to more than one person */
	   submati=adjmat[snr_i,snr_i];
	   deni=density(submati);
	   egoden[i]=deni;
	end;
  end;

  outv=adjid||egoden; /* make one matrix with both id and egoden #s */

  create ed_dat from outv [colname={AIDR egoden}];
  append from outv;
quit;
proc means data=ed_dat;
run;
*Variable meanings: 
emadhd: proportion of friends with ADHD
NEADHD: number of alters
egoden: ego-network density;

data ADHDNetwork9;
 merge ed_dat s_emean ADHDFriends009;
 by AIDR;
run;

*Merge all ADHD school subsets;
data ADHDNetworkFull2;
 merge ADHDNetwork6 ADHDNetwork7 ADHDNetwork8 ADHDNetwork9;
 by AIDR;
 PropADHDFriends=emadhd;
 TotADHDFriends=emadhd*NEADHD;
 Alters=NEADHD;
 FriendPS=EMprob_ADHD;
 if ADHD=6 then ADHD=.;
run;

*Homophily Results;
	*ADHD;
proc means data=ADHDNetworkFull2;
 class ADHD;
 var Alters egoden FriendPS PropADHDFriends TotADHDFriends;
 weight GSWGT2;
 run;
 	*DifficultyAttention;
proc means;
 class DifficultyAttention ;
 var Alters egoden EMDifficultyAttention;
 weight GSWGT2;
run;
	*EasilyBored - IS CONFOUNDED by FR_flag=0;
proc means;
 class easilybored;
 var Alters egoden EMEasilyBored;
 weight GSWGT2;
run;
	*GutFeeling might be confounded by FR_flag=1;
proc means;
 class GutFeeling;
 var Alters egoden EMGutFeeling;
 weight GSWGT2;
run;
	*TroubleRelaxing might be confounded by fr_flag=0;
proc means;
 class TroubleRelaxing;
 var Alters egoden EMTroubleRelaxing;
 weight GSWGT2;
run;
	*EasilyDistracted ;
proc means;
 class EasilyDistracted;
 var Alters egoden EMEasilyDistracted;
 weight GSWGT2;
run;
	*TroubleRemembering ;
proc means;
 class TroubleRemembering;
 var Alters egoden EMTroubleRemembering;
 weight GSWGT2;
run;
	*AvoidHomework;
proc means;
 class AvoidHomework fr_flag;
 var Alters egoden EMAvoidHomework;
 weight GSWGT2;
run;
