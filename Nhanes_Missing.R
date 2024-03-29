library(nhanesA)
library(tidyverse)
library(dplyr)
library(survey)
library(caret)
library(xgboost)
library(Matrix) 
library(data.table)
library(glmnet)
library(pROC)
library(tidymodels)
library(questionr) 

#Nhanes 2003-2004
DEMO_C <- nhanes('DEMO_C')
ALQ_C <- nhanes('ALQ_C')
BPQ_C <- nhanes('BPQ_C')
DIQ_C <- nhanes('DIQ_C') 
SMQ_C <- nhanes('SMQ_C')
BMX_C <- nhanes('BMX_C')
MCQ_C <- nhanes('MCQ_C')
l13_c <- nhanes('l13_c')
DR1TOT_C <- nhanes('DR1TOT_C')
BPX_C <- nhanes('BPX_C')
L10AM_C <- nhanes('L10AM_C')

DEMO_C <- DEMO_C %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_C <- ALQ_C %>% select(SEQN, ALQ130)
BPQ_C <- BPQ_C %>% select(SEQN, BPQ020)
DIQ_C <- DIQ_C %>% select(SEQN, DIQ010)
SMQ_C <- SMQ_C %>% select(SEQN, SMD030) 
BMX_C <- BMX_C %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_C <- MCQ_C %>% select(SEQN, MCQ250A)
l13_c <- l13_c %>% select(SEQN, LBXTC)
DR1TOT_C <- DR1TOT_C %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_C <- BPX_C %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
L10AM_C <- L10AM_C %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_C, ALQ_C, by="SEQN")
C2 <- left_join(C1, BPQ_C, by="SEQN")
C3 <- left_join(C2, DIQ_C, by="SEQN")
C4 <- left_join(C3, SMQ_C, by="SEQN")
C5 <- left_join(C4, BMX_C, by="SEQN")
C6 <- left_join(C5, MCQ_C, by="SEQN")
C7 <- left_join(C6, L10AM_C, by="SEQN")
C8 <- left_join(C7, l13_c, by="SEQN")
C9 <- left_join(C8, DR1TOT_C, by="SEQN")
NHANES2003 <- left_join(C9, BPX_C, by="SEQN")
xnhanes2003 <- NHANES2003

NHANES2003 <- NHANES2003 %>% mutate(INDHHINC=recode(INDHHINC, "$     0 to $ 4,999" ='$ 0 to $ 4,999'))
NHANES2003 <- NHANES2003 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "3"="2003_2004"))

NHANES2003 <- as_tibble(NHANES2003)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHINC",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ250A",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2003 <- rename (NHANES2003, all_of(lookup))

#Nhanes 2005-2006
DEMO_D <- nhanes('DEMO_D')
ALQ_D <- nhanes('ALQ_D')
BPQ_D <- nhanes('BPQ_D')
DIQ_D <- nhanes('DIQ_D') 
SMQ_D <- nhanes('SMQ_D')
BMX_D <- nhanes('BMX_D')
MCQ_D <- nhanes('MCQ_D')
TCHOL_D <- nhanes('TCHOL_D')
DR1TOT_D <- nhanes('DR1TOT_D')
BPX_D <- nhanes('BPX_D')
GLU_D <- nhanes('GLU_D')

DEMO_D <- DEMO_D %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_D <- ALQ_D %>% select(SEQN, ALQ130)
BPQ_D <- BPQ_D %>% select(SEQN, BPQ020)
DIQ_D <- DIQ_D %>% select(SEQN, DIQ010)
SMQ_D <- SMQ_D %>% select(SEQN, SMD030) 
BMX_D <- BMX_D %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_D <- MCQ_D %>% select(SEQN, MCQ300C)
TCHOL_D <- TCHOL_D %>% select(SEQN, LBXTC)
DR1TOT_D <- DR1TOT_D %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_D <- BPX_D %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_D <- GLU_D %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_D, ALQ_D, by="SEQN")
C2 <- left_join(C1, BPQ_D, by="SEQN")
C3 <- left_join(C2, DIQ_D, by="SEQN")
C4 <- left_join(C3, SMQ_D, by="SEQN")
C5 <- left_join(C4, BMX_D, by="SEQN")
C6 <- left_join(C5, MCQ_D, by="SEQN")
C7 <- left_join(C6, GLU_D, by="SEQN")
C8 <- left_join(C7, TCHOL_D, by="SEQN")
C9 <- left_join(C8, DR1TOT_D, by="SEQN")
NHANES2005 <- left_join(C9, BPX_D, by="SEQN")
xnhanes2005 <- NHANES2005

NHANES2005 <- NHANES2005 %>% mutate(INDHHINC=recode(INDHHINC, "$     0 to $ 4,999" ='$ 0 to $ 4,999'))
NHANES2005 <- NHANES2005 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "4"="2005_2006"))


NHANES2005 <- as_tibble(NHANES2005)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHINC",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2005 <- rename (NHANES2005, all_of(lookup))

#Nhanes 2007-2008 E
DEMO_E <- nhanes('DEMO_E')
ALQ_E <- nhanes('ALQ_E')
BPQ_E <- nhanes('BPQ_E')
DIQ_E <- nhanes('DIQ_E') 
SMQ_E <- nhanes('SMQ_E')
BMX_E <- nhanes('BMX_E')
MCQ_E <- nhanes('MCQ_E')
TCHOL_E <- nhanes('TCHOL_E')
DR1TOT_E <- nhanes('DR1TOT_E')
BPX_E <- nhanes('BPX_E')
GLU_E <- nhanes('GLU_E')

DEMO_E <- DEMO_E %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_E <- ALQ_E %>% select(SEQN, ALQ130)
BPQ_E <- BPQ_E %>% select(SEQN, BPQ020)
DIQ_E <- DIQ_E %>% select(SEQN, DIQ010)
SMQ_E <- SMQ_E %>% select(SEQN, SMD030) 
BMX_E <- BMX_E %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_E <- MCQ_E %>% select(SEQN, MCQ300C)
TCHOL_E <- TCHOL_E %>% select(SEQN, LBXTC)
DR1TOT_E <- DR1TOT_E %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_E <- BPX_E %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_E <- GLU_E %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_E, ALQ_E, by="SEQN")
C2 <- left_join(C1, BPQ_E, by="SEQN")
C3 <- left_join(C2, DIQ_E, by="SEQN")
C4 <- left_join(C3, SMQ_E, by="SEQN")
C5 <- left_join(C4, BMX_E, by="SEQN")
C6 <- left_join(C5, MCQ_E, by="SEQN")
C7 <- left_join(C6, GLU_E, by="SEQN")
C8 <- left_join(C7, TCHOL_E, by="SEQN")
C9 <- left_join(C8, DR1TOT_E, by="SEQN")
NHANES2007 <- left_join(C9, BPX_E, by="SEQN")
xnhanes2007 <- NHANES2007

NHANES2007 <- NHANES2007 %>% mutate(INDHHIN2=recode(INDHHIN2, "$     0 to $ 4,999" ='$ 0 to $ 4,999',
                                                    '$75,000 to $99,999'='$75,000 and Over',
                                                    '$100,000 and Over'='$75,000 and Over'))
NHANES2007 <- NHANES2007 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "5"="2007_2008"))

NHANES2007 <- as_tibble(NHANES2007)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHIN2",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2007 <- rename (NHANES2007, all_of(lookup))

#Nhanes 2009-2010 F
DEMO_F <- nhanes('DEMO_F')
ALQ_F <- nhanes('ALQ_F')
BPQ_F <- nhanes('BPQ_F')
DIQ_F <- nhanes('DIQ_F') 
SMQ_F <- nhanes('SMQ_F')
BMX_F <- nhanes('BMX_F')
MCQ_F <- nhanes('MCQ_F')
TCHOL_F <- nhanes('TCHOL_F')
DR1TOT_F <- nhanes('DR1TOT_F')
BPX_F <- nhanes('BPX_F')
GLU_F <- nhanes('GLU_F')

DEMO_F <- DEMO_F %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_F <- ALQ_F %>% select(SEQN, ALQ130)
BPQ_F <- BPQ_F %>% select(SEQN, BPQ020)
DIQ_F <- DIQ_F %>% select(SEQN, DIQ010)
SMQ_F <- SMQ_F %>% select(SEQN, SMD030) 
BMX_F <- BMX_F %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_F <- MCQ_F %>% select(SEQN, MCQ300C)
TCHOL_F <- TCHOL_F %>% select(SEQN, LBXTC)
DR1TOT_F <- DR1TOT_F %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_F <- BPX_F %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_F <- GLU_F %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_F, ALQ_F, by="SEQN")
C2 <- left_join(C1, BPQ_F, by="SEQN")
C3 <- left_join(C2, DIQ_F, by="SEQN")
C4 <- left_join(C3, SMQ_F, by="SEQN")
C5 <- left_join(C4, BMX_F, by="SEQN")
C6 <- left_join(C5, MCQ_F, by="SEQN")
C7 <- left_join(C6, GLU_F, by="SEQN")
C8 <- left_join(C7, TCHOL_F, by="SEQN")
C9 <- left_join(C8, DR1TOT_F, by="SEQN")
NHANES2009 <- left_join(C9, BPX_F, by="SEQN")
xnhanes2009 <- NHANES2009

NHANES2009 <- NHANES2009 %>% mutate(INDHHIN2=recode(INDHHIN2, "$     0 to $ 4,999" ='$ 0 to $ 4,999', 
                                                    '$75,000 to $99,999'='$75,000 and Over', 
                                                    '$100,000 and Over'='$75,000 and Over'))
NHANES2009 <- NHANES2009 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "6"="2009_2010"))

NHANES2009 <- as_tibble(NHANES2009)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHIN2",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2009 <- rename (NHANES2009, all_of(lookup))

#Nhanes 2011-2012 G
DEMO_G <- nhanes('DEMO_G')
ALQ_G <- nhanes('ALQ_G')
BPQ_G <- nhanes('BPQ_G')
DIQ_G <- nhanes('DIQ_G') 
SMQ_G <- nhanes('SMQ_G')
BMX_G <- nhanes('BMX_G')
MCQ_G <- nhanes('MCQ_G')
TCHOL_G <- nhanes('TCHOL_G')
DR1TOT_G <- nhanes('DR1TOT_G')
BPX_G <- nhanes('BPX_G')
GLU_G <- nhanes('GLU_G')

DEMO_G <- DEMO_G %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_G <- ALQ_G %>% select(SEQN, ALQ130)
BPQ_G <- BPQ_G %>% select(SEQN, BPQ020)
DIQ_G <- DIQ_G %>% select(SEQN, DIQ010)
SMQ_G <- SMQ_G %>% select(SEQN, SMD030) 
BMX_G <- BMX_G %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_G <- MCQ_G %>% select(SEQN, MCQ300C)
TCHOL_G <- TCHOL_G %>% select(SEQN, LBXTC)
DR1TOT_G <- DR1TOT_G %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_G <- BPX_G %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_G <- GLU_G %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_G, ALQ_G, by="SEQN")
C2 <- left_join(C1, BPQ_G, by="SEQN")
C3 <- left_join(C2, DIQ_G, by="SEQN")
C4 <- left_join(C3, SMQ_G, by="SEQN")
C5 <- left_join(C4, BMX_G, by="SEQN")
C6 <- left_join(C5, MCQ_G, by="SEQN")
C7 <- left_join(C6, GLU_G, by="SEQN")
C8 <- left_join(C7, TCHOL_G, by="SEQN")
C9 <- left_join(C8, DR1TOT_G, by="SEQN")
NHANES2011 <- left_join(C9, BPX_G, by="SEQN")
xnhanes2011 <- NHANES2011


NHANES2011 <- NHANES2011 %>% mutate(DMDEDUC2=recode(DMDEDUC2, 'Less than 9th grade'='Less Than 9th Grade',
                                                    '9-11th grade (Includes 12th grade with no diploma)'='9-11th Grade (Includes 12th grade with no diploma)',
                                                    'High school graduate/GED or equivalent'='High School Grad/GED or Equivalent',
                                                    'Some college or AA degree'='Some College or AA degree', 
                                                    'College graduate or above'='College Graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2011 <- NHANES2011 %>% mutate(INDHHIN2=recode(INDHHIN2, '$75,000 to $99,999'='$75,000 and Over',
                                                    '$100,000 and Over'='$75,000 and Over'))
NHANES2011 <- NHANES2011 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "7"="2011_2012"))

NHANES2011 <- as_tibble(NHANES2011)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHIN2",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2011 <- rename (NHANES2011, all_of(lookup))

#Nhanes 2013-2014 H
DEMO_H <- nhanes('DEMO_H')
ALQ_H <- nhanes('ALQ_H')
BPQ_H <- nhanes('BPQ_H')
DIQ_H <- nhanes('DIQ_H') 
SMQ_H <- nhanes('SMQ_H')
BMX_H <- nhanes('BMX_H')
MCQ_H <- nhanes('MCQ_H')
TCHOL_H <- nhanes('TCHOL_H')
DR1TOT_H <- nhanes('DR1TOT_H')
BPX_H <- nhanes('BPX_H')
GLU_H <- nhanes('GLU_H')

DEMO_H <- DEMO_H %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_H <- ALQ_H %>% select(SEQN, ALQ130)
BPQ_H <- BPQ_H %>% select(SEQN, BPQ020)
DIQ_H <- DIQ_H %>% select(SEQN, DIQ010)
SMQ_H <- SMQ_H %>% select(SEQN, SMD030) 
BMX_H <- BMX_H %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_H <- MCQ_H %>% select(SEQN, MCQ300C)
TCHOL_H <- TCHOL_H %>% select(SEQN, LBXTC)
DR1TOT_H <- DR1TOT_H %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_H <- BPX_H %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_H <- GLU_H %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_H, ALQ_H, by="SEQN")
C2 <- left_join(C1, BPQ_H, by="SEQN")
C3 <- left_join(C2, DIQ_H, by="SEQN")
C4 <- left_join(C3, SMQ_H, by="SEQN")
C5 <- left_join(C4, BMX_H, by="SEQN")
C6 <- left_join(C5, MCQ_H, by="SEQN")
C7 <- left_join(C6, GLU_H, by="SEQN")
C8 <- left_join(C7, TCHOL_H, by="SEQN")
C9 <- left_join(C8, DR1TOT_H, by="SEQN")
NHANES2013 <- left_join(C9, BPX_H, by="SEQN")
xnhanes2013 <- NHANES2013


NHANES2013 <- NHANES2013 %>% mutate(DMDEDUC2=recode(DMDEDUC2, 'Less than 9th grade'='Less Than 9th Grade',
                                                    '9-11th grade (Includes 12th grade with no diploma)'='9-11th Grade (Includes 12th grade with no diploma)',
                                                    'High school graduate/GED or equivalent'='High School Grad/GED or Equivalent',
                                                    'Some college or AA degree'='Some College or AA degree', 
                                                    'College graduate or above'='College Graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2013 <- NHANES2013 %>% mutate(INDHHIN2=recode(INDHHIN2, '$75,000 to $99,999'='$75,000 and Over',
                                                    '$100,000 and Over'='$75,000 and Over'))
NHANES2013 <- NHANES2013 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "8"="2013_2014"))

NHANES2013 <- as_tibble(NHANES2013)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHIN2",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2013 <- rename (NHANES2013, all_of(lookup))

#Nhanes 2015-2016 I
DEMO_I <- nhanes('DEMO_I')
ALQ_I <- nhanes('ALQ_I')
BPQ_I <- nhanes('BPQ_I')
DIQ_I <- nhanes('DIQ_I') 
SMQ_I <- nhanes('SMQ_I')
BMX_I <- nhanes('BMX_I')
MCQ_I <- nhanes('MCQ_I')
TCHOL_I <- nhanes('TCHOL_I')
DR1TOT_I <- nhanes('DR1TOT_I')
BPX_I <- nhanes('BPX_I')
GLU_I <- nhanes('GLU_I')

DEMO_I <- DEMO_I %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_I <- ALQ_I %>% select(SEQN, ALQ130)
BPQ_I <- BPQ_I %>% select(SEQN, BPQ020)
DIQ_I <- DIQ_I %>% select(SEQN, DIQ010)
SMQ_I <- SMQ_I %>% select(SEQN, SMD030) 
BMX_I <- BMX_I %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_I <- MCQ_I %>% select(SEQN, MCQ300C)
TCHOL_I <- TCHOL_I %>% select(SEQN, LBXTC)
DR1TOT_I <- DR1TOT_I %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_I <- BPX_I %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_I <- GLU_I %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_I, ALQ_I, by="SEQN")
C2 <- left_join(C1, BPQ_I, by="SEQN")
C3 <- left_join(C2, DIQ_I, by="SEQN")
C4 <- left_join(C3, SMQ_I, by="SEQN")
C5 <- left_join(C4, BMX_I, by="SEQN")
C6 <- left_join(C5, MCQ_I, by="SEQN")
C7 <- left_join(C6, GLU_I, by="SEQN")
C8 <- left_join(C7, TCHOL_I, by="SEQN")
C9 <- left_join(C8, DR1TOT_I, by="SEQN")
NHANES2015 <- left_join(C9, BPX_I, by="SEQN")
xnhanes2015 <- NHANES2015


NHANES2015 <- NHANES2015 %>% mutate(DMDEDUC2=recode(DMDEDUC2, 'Less than 9th grade'='Less Than 9th Grade',
                                                    '9-11th grade (Includes 12th grade with no diploma)'='9-11th Grade (Includes 12th grade with no diploma)',
                                                    'High school graduate/GED or equivalent'='High School Grad/GED or Equivalent',
                                                    'Some college or AA degree'='Some College or AA degree', 
                                                    'College graduate or above'='College Graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2015 <- NHANES2015 %>% mutate(INDHHIN2=recode(INDHHIN2, '$75,000 to $99,999'='$75,000 and Over',
                                                    '$100,000 and Over'='$75,000 and Over'))
NHANES2015 <- NHANES2015 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "9"="2015_2016"))

NHANES2015 <- as_tibble(NHANES2015)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHIN2",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2015 <- rename (NHANES2015, all_of(lookup))

#Nhanes 2017-2018 J
DEMO_J <- nhanes('DEMO_J')
ALQ_J <- nhanes('ALQ_J')
BPQ_J <- nhanes('BPQ_J')
DIQ_J <- nhanes('DIQ_J') 
SMQ_J <- nhanes('SMQ_J')
BMX_J <- nhanes('BMX_J')
MCQ_J <- nhanes('MCQ_J')
TCHOL_J <- nhanes('TCHOL_J')
DR1TOT_J <- nhanes('DR1TOT_J')
BPX_J <- nhanes('BPX_J')
GLU_J <- nhanes('GLU_J')

DEMO_J <- DEMO_J %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR, SDDSRVYR, SDMVPSU, SDMVSTRA)
ALQ_J <- ALQ_J %>% select(SEQN, ALQ130)
BPQ_J <- BPQ_J %>% select(SEQN, BPQ020)
DIQ_J <- DIQ_J %>% select(SEQN, DIQ010)
SMQ_J <- SMQ_J %>% select(SEQN, SMD030) 
BMX_J <- BMX_J %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_J <- MCQ_J %>% select(SEQN, MCQ300C)
TCHOL_J <- TCHOL_J %>% select(SEQN, LBXTC)
DR1TOT_J <- DR1TOT_J %>% select(SEQN, DR1TKCAL, DR1TSODI, DR1TCARB, DR1TFIBE,
                                DR1TCALC, DR1TCAFF)
BPX_J <- BPX_J %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)
GLU_J <- GLU_J %>% select(SEQN, LBXGLU)

C1 <- left_join(DEMO_J, ALQ_J, by="SEQN")
C2 <- left_join(C1, BPQ_J, by="SEQN")
C3 <- left_join(C2, DIQ_J, by="SEQN")
C4 <- left_join(C3, SMQ_J, by="SEQN")
C5 <- left_join(C4, BMX_J, by="SEQN")
C6 <- left_join(C5, MCQ_J, by="SEQN")
C7 <- left_join(C6, GLU_J, by="SEQN")
C8 <- left_join(C7, TCHOL_J, by="SEQN")
C9 <- left_join(C8, DR1TOT_J, by="SEQN")
NHANES2017 <- left_join(C9, BPX_J, by="SEQN")
xnhanes2017 <- NHANES2017


NHANES2017 <- NHANES2017 %>% mutate(DMDEDUC2=recode(DMDEDUC2, 'Less than 9th grade'='Less Than 9th Grade',
                                                    '9-11th grade (Includes 12th grade with no diploma)'='9-11th Grade (Includes 12th grade with no diploma)',
                                                    'High school graduate/GED or equivalent'='High School Grad/GED or Equivalent',
                                                    'Some college or AA degree'='Some College or AA degree', 
                                                    'College graduate or above'='College Graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2017 <- NHANES2017 %>% mutate(INDHHIN2=recode(INDHHIN2, '$75,000 to $99,999'='$75,000 and Over',
                                                    '$100,000 and Over'='$75,000 and Over'))
NHANES2017 <- NHANES2017 %>% mutate(SDDSRVYR=recode(SDDSRVYR, "10"="2017_2018"))

NHANES2017 <- as_tibble(NHANES2017)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHIN2",
            "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMD030", "Weight" ="BMXWT",
            "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ300C",
            "Sodium"="DR1TSODI", "Carb"="DR1TCARB", "Fiber"="DR1TFIBE",
            "Calcium"="DR1TCALC", "KcalIntake"="DR1TKCAL", "Caffeine"="DR1TCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1", "BloodGlucose"="LBXGLU",
            "Year"="SDDSRVYR", "PSU"="SDMVPSU", "Stratum"="SDMVSTRA")
NHANES2017 <- rename (NHANES2017, all_of(lookup))

#Merging all datasets together
#C1 <- rbind(NHANES2001, NHANES2003)
C2 <- rbind(NHANES2003, NHANES2005)
C3 <- rbind(C2, NHANES2007)
C4 <- rbind(C3, NHANES2009)
C5 <- rbind(C4, NHANES2011)
C6 <- rbind(C5, NHANES2013)
C7 <- rbind(C6, NHANES2015)
NHANES_full <- rbind(C7, NHANES2017)
ls(NHANES_full)

#Removing those 19 and younger, and creating survey weights
NHANES_full = subset(NHANES_full, NHANES_full$Age > 19)
check = subset(C6, C6$Age > 19)
NHANES_full <- NHANES_full %>% mutate(surveyweight = 1/8 * WTMEC2YR)

gx <- data.table(NHANES_full, keep.rownames = FALSE)
gx[, SEQN := NULL]
gx[, WTMEC2YR := NULL]
head(gx)

gx[gx == "Refused"] <- NA
gx[gx == "Don't know"] <- NA
gx[gx == "Don't Know"] <- NA

head(gx)
categorical <- gx[,c(1,3:6,10:11,20)]
head(categorical)

#Frequencies of categorical variables
freq1 <- table(gx$Gender, useNA = "ifany")
freq2 <- table(gx$Education, useNA = "ifany")
freq3 <- table(gx$Race, useNA = "ifany")
freq4 <- table(gx$Income, useNA = "ifany")
freq5 <- table(gx$Year, useNA = "ifany")
freq6 <- table(gx$HBP, useNA = "ifany")
freq7 <- table(gx$Diabetes, useNA = "ifany")
freq8 <- table(gx$FamilyHistory, useNA = "ifany")
print(freq1)
barplot(freq1)
print(freq2)
barplot(freq2)
print(freq3)
barplot(freq3)
print(freq4)
barplot(freq4)
print(freq5)
barplot(freq5)
print(freq6)
barplot(freq6)
print(freq7)
barplot(freq7)
print(freq8)
barplot(freq8)
freq(gx$Gender)
freq(gx$Education)
freq(gx$Race)
freq(gx$Income)
freq(gx$Year)
freq(gx$HBP)
freq(gx$Diabetes)
freq(gx$FamilyHistory)

#Percentiles of continuous variables
quantile(gx$Alcohol,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Smoking,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Weight,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Height,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Waist,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Leg,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$BMI,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Arm,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$ArmCircumference,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$BloodGlucose,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Cholesterol,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$KcalIntake,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Sodium,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Carb,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Fiber,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Calcium,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Caffeine,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Pulse,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Systolic,c(.25, .50, .75, .99), na.rm=TRUE)
quantile(gx$Diastolic,c(.25, .50, .75, .99), na.rm=TRUE)
freq(gx$Alcohol)
freq(gx$Smoking)
a <- freq(gx$Weight)
tail(a)
b<- freq(gx$Height)
tail(b)
c<-freq(gx$Waist)
tail(c)
d<-freq(gx$Leg)
tail(d)
e<-freq(gx$BMI)
tail(e)
f<-freq(gx$Arm)
tail(f)
g<-freq(gx$ArmCircumference)
tail(g)
h<-freq(gx$BloodGlucose)
tail(h)
i<-freq(gx$Cholesterol)
tail(i)
j<-freq(gx$KcalIntake)
tail(j)
k<-freq(gx$Sodium)
tail(k)
l<-freq(gx$Carb)
tail(l)
m<-freq(gx$Fiber)
tail(m)
n<-freq(gx$Calcium)
tail(n)
o<-freq(gx$Caffeine)
tail(o)
p<-freq(gx$Pulse)
tail(p)
q<-freq(gx$Systolic)
tail(q)
r<-freq(gx$Diastolic)
tail(r)

nrow(NHANES_full)
NHANES_fullA <- NHANES_full
NHANES_fullA$Smoking <-  NHANES_fullA$Smoking %>% replace(is.na(.), 0)
NHANES_fullA$Alcohol <-  NHANES_fullA$Alcohol %>% replace(is.na(.), 0)
gx <- NHANES_fullA

gxt <- gx %>% mutate(Education=recode(Education, "Less Than 9th Grade" = 1,
                                      "9-11th Grade (Includes 12th grade with no diploma)" =2,
                                      "High School Grad/GED or Equivalent" =3, 
                                      "Some College or AA degree" =4, "College Graduate or above"=5,
                                      "Refused"= 0, "Don't Know"=0))
gxt <- gxt %>% mutate(Income=recode(Income, "$ 0 to $ 4,999"=1, "$ 5,000 to $ 9,999"=2,
                                    "$10,000 to $14,999"=3, "$15,000 to $19,999"=4,
                                    "$20,000 to $24,999"=5, "$25,000 to $34,999"=6,
                                    "$35,000 to $44,999"=7, "$45,000 to $54,999"=8,
                                    "$55,000 to $64,999"=9, "$65,000 to $74,999"=10,
                                    "$75,000 and Over"=11, "Over $20,000"=0, "Under $20,000"=0,
                                    "Refused"=0, "Don't know"=0, "$20,000 and Over"=0))
gx <- gxt

gdmy <- dummyVars(" ~ .", data = gx)
NHANES_dummy <- data.frame(predict(gdmy, newdata = gx))
dummy <- NHANES_dummy
head(dummy)
#1, 3, 12, 13-20, 21, 22, 26, 28-31, 
#Removing FamilyHistory.Don.t.know as reference category for family history of diabetes
dummy= dummy[,-42:-43]
#removing all other instances of doctor told you have diabetes beyond the yes
#column, X.Doctor.told.you.have.diabetes.Yes is a binary and should be all I need
dummy= dummy[, -28:-31]
#Removing HPB.Don.t.know as reference category for high blood pressure
dummy= dummy[, -26]
#Removing WTMEC2YR, Year, PSU, Stratum
dummy = dummy[,-12:-22]
#Removing female as reference category for gender
dummy= dummy[,-3]
#Removing SEQN
dummy = dummy[,-1]
head(dummy)
ncol(dummy)


dummy$Q <- ifelse(dummy$Diabetes >= "Yes", 1,0)
dummy$Q
dummy$Diabetes.Yes
dummy$Z <- ifelse(dummy$BloodGlucose >=126, 1, 0)
dummy$Z
dummy$Diabetes <- ifelse(dummy$Q>=1 | dummy$Z>=1, 1, 0)

dummy= dummy[, -36:-37]
dummy= dummy[, -24]
dummy= dummy[, -13]

dummyA <- dummy %>% drop_na(Diabetes)
head(dummyA)
nrow(dummyA)
nrow(dummy)
dummy2 <- drop_na(dummy)
dummy3 <- drop_na(dummyA)
nrow(dummy2)
nrow(dummy3)