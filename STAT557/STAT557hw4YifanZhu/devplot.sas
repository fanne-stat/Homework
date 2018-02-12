
/*  This program is stored as devplot.sas */

/*  This program uses  PROC LOGISTIC  in  
    SAS  to fit models to the BPD data and
    use PROC CLUSTER to create deviance  
    plots.  SASGRAPH is used to make high 
    quality plots. */

  ods rtf file="bpddev.rtf";

DATA SET1;
 INFILE 'c:\stat557\data\bpd.dat';
 INPUT SEX 1 YOB 2-3 APGAR 4-5 GEST 6-8 
       BWT 9-12 AGSYM 13-14 AGVEN 15-17 
       INTUB 18-21 VENTL 22-25 
       LOWO2 26-30 
       MEDO2 31-34 HIO2 35-38 SURV 39
       DSURV 40-44 RDS 45 BPDHI 46-47;
   LNL=LOG(LOWO2+1); LNM=LOG(MEDO2+1);
   LNH=LOG(HIO2+1);  LNV=LOG(VENTL);
   L2=LOWO2**2;      M2=MEDO2**2;
   H2=HIO2**2;       V2=VENTL**2;
   LL2 = LNL**2;     LM2 = LNM**2;
   LH2 = LNH**2;     LV2 = LNV**2;
   LNI = LOG(INTUB+1);
     RDS1 = 0; IF(RDS=1) THEN RDS1=1;
     RDS2 = 0; IF(RDS=2) THEN RDS2=1;
     RDS3 = 0; IF(RDS=3) THEN RDS3=1;
     RDS4 = 0; IF(RDS=4) THEN RDS4=1;
     RDS5 = 0; IF(RDS=5) THEN RDS5=1;

  IF BPDHI>20 THEN BPD=1; ELSE BPD=2;

 
*    Retain only babies who lived at least 72 hours;

    IF(DSURV > 72);
run;


*     establish formats for classification variables;

PROC FORMAT; VALUE SEX 0 = 'FEMALE'
                       1 = 'MALE';
            VALUE SURV 0 = 'DEAD'
                       1 = 'ALIVE';
             VALUE RDS 0 = 'NONE'
                       1 = 'SLIGHT'
                       2 = 'MODERATE'
                       3 = 'SUBSTANTIAL'
                       4 = 'SEVERE'
                       5 = 'VERY SEVERE';
            VALUE BPD 1 = 'YES'
                      2 = 'NO';
run;

/*  This is SAS code for hierarchical 
    cluster analysis. It uses PROC CLUSTER 
    followed by PROC TREE to produce a 
    file, callled OUT, containing the 
    original data and a variable called 
    CLUSTER that defines the clusters. */

PROC CLUSTER DATA=set1 OUT=TREE METHOD=CENTROID p=60 pseudo ccc rsquare;
   VAR LNL LNH LNM LNV LNI AGVEN
        AGSYM BWT GEST YOB SEX RDS1-RDS5;
 
PROC TREE DATA=TREE NOPRINT OUT=OUT NCL=15;
   RUN;

Data out; merge out set1;

PROC SORT DATA=OUT; BY CLUSTER; RUN;


/* NEXT FIT A LOGISTIC REGRESSION MODEL INCORPORATING 
   CLUSTER EFFECTS AS DUMMY VARIABLES
*/


PROC LOGISTIC DATA=OUT COVOUT OUTEST=SETP1;
   CLASS CLUSTER;
   MODEL BPD = LNL LNM LNH LNV LM2 LH2 CLUSTER
         / ITPRINT CTABLE MAXITER=120 CONVERGE=.00001  ;
   OUTPUT OUT=SETR1 RESDEV=RESDEV PRED=PHAT;
RUN;

/*  Compute local deviance values */

DATA SETR1; SET SETR1;
   Y = 2-BPD;
   D  = -2*(Y*LOG(PHAT)+(1-Y)*LOG(1-PHAT));
   RUN;

/* Order clusters by within cluster variation and 
   compute the contibution to the local deviance 
   for each cluster */

PROC MEANS DATA=SETR1 NOPRINT; BY CLUSTER;
   VAR Y PHAT D LNL LNH LNM LNV LNI AGVEN
        AGSYM BWT GEST YOB SEX RDS1-RDS5 ;
   OUTPUT OUT=MEANS  MEAN= BPD PHAT 
       VAR = VBPD VP VD V1-V16 SUM = SBPD SPAT SD  N=N;
   RUN;

DATA MEANS; SET MEANS;
   SS = SUM(OF V1-V16);
   DF = N-1;
   KEEP CLUSTER BPD PHAT SD SS DF;
   RUN;

PROC SORT DATA=MEANS; BY SS;
   RUN;

PROC PRINT DATA=MEANS; RUN;


/* Go into IML to cummulative local 
   deviance values and degrees of 
   freedom across clusters  */

PROC IML;
  START CUMSUM;
  USE MEANS;
  READ ALL INTO X;
  X[ ,4] = CUSUM(X[ ,4]);
  X[ ,6] = CUSUM(X[ ,6]);
  X[ ,4] = X[ ,4]/X[ ,6];
  CREATE MEANS2 FROM X [COLNAME = {'CLUSTER' 
    'BPD' 'PHAT' 'SD' 'SS' 'DF'}];
  APPEND FROM X;
FINISH;

RUN CUMSUM;

PROC PRINT DATA=MEANS2; RUN;


/*  Fit the logistic regression model without the 
    cluster effects to compute the global deviance */


PROC LOGISTIC DATA=OUT COVOUT OUTEST=SETP2;
   MODEL BPD =  LNL LNM LNH LNV LM2 LH2
             / MAXITER=30 CONVERGE=.00001;
   OUTPUT OUT=SETR2 RESDEV=RESDEV PRED=PHAT;
RUN;

/*  Obtain the total sample size  */

PROC MEANS DATA=SETR2;
  VAR BPD;
  OUTPUT OUT=MEANG N=N;
  RUN;

/*  Compute the global deviance and 
    compute the number of parameters
    in the original model  */

DATA SETP2; SET SETP2;
  IF(_TYPE_ = 'COV');
  KEEP _LNLIKE_;
  RUN;

PROC MEANS DATA=SETP2;
  VAR _LNLIKE_;
  OUTPUT OUT=MEANN MEAN=GDEV N=NP;
  RUN;

DATA MEANG; MERGE MEANG MEANN;
  GDEV = -2*GDEV/(N-NP);
  KEEP GDEV;
  RUN;

/*  Merge the data files  */

DATA MEANS2; IF(_N_=1) THEN SET MEANG;
  SET MEANS2;
  RUN;

PROC PRINT DATA=MEANS2; RUN;

/* Display the local deviance plot */

proc sgplot data=means2;
  scatter x=df y=sd /   markerattrs=(size=10 symbol=CircleFilled color=black);
  loess x=df y=gdev / markerattrs=(size=0);
  yaxis label="Cummulative Deviance" labelattrs=(size=20) valueattrs=(size=15);
  xaxis label="df" labelattrs=(size=20) valueattrs=(size=15);
run;



ods rtf close;
