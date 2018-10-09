

 *import churn;
libname sas_data "C:\Users\sanja\Desktop\SAS_KashaDehnad\SAS_data";
*access=read;
run;

 PROC IMPORT OUT= WORK.telco 
            DATAFILE= "C:\Users\sanja\Desktop\SAS_KashaDehnad\SAS_data\WA_Fn-UseC_-T
elco-Customer-Churn.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;




/*split the data into train and test*/
PROC SURVEYSELECT DATA=telco outall OUT=telco_total METHOD=srs 
SAMPRATE=0.4 seed=1803;

data train test;
set telco_total;
if Selected=1 then output test;
  else output train;
run;

/*Running Stepwise logistic with all variables*/
proc logistic data=train plots(only)=ROC SIMPLE outest=train_est;
class     churn gender           SeniorCitizen    Partner         
Dependents        PhoneService     MultipleLines   
InternetService  OnlineSecurity   OnlineBackup     DeviceProtection
TechSupport      StreamingTV      StreamingMovies  Contract        
PaperlessBilling PaymentMethod/param=ref;


model churn(descending)   = gender  SeniorCitizen   Partner  Dependents   tenure   PhoneService MultipleLines   
InternetService  OnlineSecurity   OnlineBackup     DeviceProtection TechSupport StreamingTV
StreamingMovies  Contract PaperlessBilling PaymentMethod    MonthlyCharges   TotalCharges/slentry=0.01 selection=NONE ;

OUTPUT OUT=train_out ;
quit;
run;


/*Running Stepwise logistic with all variables*/
proc logistic data=train plots(only)=ROC SIMPLE outest=train_est;
class     churn gender           SeniorCitizen    Partner         
Dependents        PhoneService     MultipleLines   
InternetService  OnlineSecurity   OnlineBackup     DeviceProtection
TechSupport      StreamingTV      StreamingMovies  Contract        
PaperlessBilling PaymentMethod/param=ref;


model churn(descending)   = gender  SeniorCitizen   Partner  Dependents   tenure   PhoneService MultipleLines   
InternetService  OnlineSecurity   OnlineBackup     DeviceProtection TechSupport StreamingTV
StreamingMovies  Contract PaperlessBilling PaymentMethod    MonthlyCharges   TotalCharges/ selection=STEPWISE ;

OUTPUT OUT=train_out ;
quit;
run;


/*storing the final predicted model in trained model*/

PROC logistic data=train plots(only)=ROC outmodel=sasuser.TrainedModel COVOUT  ;

class Churn Contract InternetService PaperlessBilling StreamingMovies OnlineSecurity 
MultipleLines PaymentMethod TechSupport  Dependents;

model churn(descending)=Contract InternetService tenure PaperlessBilling StreamingMovies OnlineSecurity 
MultipleLines TotalCharges PaymentMethod TechSupport  Dependents  / ctable outroc=ROCDATA;
Output out=telco_out  h=lev ;
run;




/*Scoring Test Data using Trained model*/
PROC logistic inmodel=sasuser.TrainedModel plots(only)=ROC ;

score data=test out=ScoredTelco fitstat outroc=ROCDATA_TEST ;


run;



 /*"Cut Off Calculation"*/
data CUTOFF_VALUES;
set ROCDATA;
_SPECIF_ = (1 - _1MSPEC_);
J = _SENSIT_ + _SPECIF_ - 1;
D= Sqrt((1-_SENSIT_)**2 + (1-_SPECIF_)**2);
run;

proc sql print;
create table cutoff as
select _PROB_ , J,D
from CUTOFF_VALUES
having D = min(D) | J=Max(J) ;
quit;
run;
/*Using Cut off value close to 0.20*/
data ScoredTelco_Pred;
set ScoredTelco;
if P_Yes>0.290 then Pred_Churn="Yes"; else Pred_Churn="No ";
run;
/*Display Classification matrix*/
proc freq data=ScoredTelco_Pred; 
      table Churn*Pred_Churn / out=Telco_Output nocol nocum nopercent; 
 run;
