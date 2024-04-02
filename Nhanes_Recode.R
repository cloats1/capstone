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
ALQ_C <- ALQ_C %>% select(SEQN, ALQ101)
BPQ_C <- BPQ_C %>% select(SEQN, BPQ020)
DIQ_C <- DIQ_C %>% select(SEQN, DIQ010)
SMQ_C <- SMQ_C %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_D <- ALQ_D %>% select(SEQN, ALQ101)
BPQ_D <- BPQ_D %>% select(SEQN, BPQ020)
DIQ_D <- DIQ_D %>% select(SEQN, DIQ010)
SMQ_D <- SMQ_D %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_E <- ALQ_E %>% select(SEQN, ALQ101)
BPQ_E <- BPQ_E %>% select(SEQN, BPQ020)
DIQ_E <- DIQ_E %>% select(SEQN, DIQ010)
SMQ_E <- SMQ_E %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_F <- ALQ_F %>% select(SEQN, ALQ101)
BPQ_F <- BPQ_F %>% select(SEQN, BPQ020)
DIQ_F <- DIQ_F %>% select(SEQN, DIQ010)
SMQ_F <- SMQ_F %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_G <- ALQ_G %>% select(SEQN, ALQ101)
BPQ_G <- BPQ_G %>% select(SEQN, BPQ020)
DIQ_G <- DIQ_G %>% select(SEQN, DIQ010)
SMQ_G <- SMQ_G %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_H <- ALQ_H %>% select(SEQN, ALQ101)
BPQ_H <- BPQ_H %>% select(SEQN, BPQ020)
DIQ_H <- DIQ_H %>% select(SEQN, DIQ010)
SMQ_H <- SMQ_H %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_I <- ALQ_I %>% select(SEQN, ALQ101)
BPQ_I <- BPQ_I %>% select(SEQN, BPQ020)
DIQ_I <- DIQ_I %>% select(SEQN, DIQ010)
SMQ_I <- SMQ_I %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ101", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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
ALQ_J <- ALQ_J %>% select(SEQN, ALQ111)
BPQ_J <- BPQ_J %>% select(SEQN, BPQ020)
DIQ_J <- DIQ_J %>% select(SEQN, DIQ010)
SMQ_J <- SMQ_J %>% select(SEQN, SMQ020) 
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
            "Alcohol"= "ALQ111", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
            "Smoking"= "SMQ020", "Weight" ="BMXWT",
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

freq(gx$Gender)
freq(gx$Education)
freq(gx$Race)
freq(gx$Income)
freq(gx$Year)
freq(gx$HBP)
freq(gx$Diabetes)
freq(gx$FamilyHistory)
freq(gx$Alcohol)
freq(gx$Smoking)

#Percentiles of continuous variables
quantile(gx$Weight, na.rm=TRUE)
quantile(gx$Height, na.rm=TRUE)
quantile(gx$Waist, na.rm=TRUE)
quantile(gx$Leg, na.rm=TRUE)
quantile(gx$BMI, na.rm=TRUE)
quantile(gx$Arm, na.rm=TRUE)
quantile(gx$ArmCircumference, na.rm=TRUE)
quantile(gx$BloodGlucose, na.rm=TRUE)
quantile(gx$Cholesterol, na.rm=TRUE)
quantile(gx$KcalIntake, na.rm=TRUE)
quantile(gx$Sodium, na.rm=TRUE)
quantile(gx$Carb, na.rm=TRUE)
quantile(gx$Fiber, na.rm=TRUE)
quantile(gx$Calcium, na.rm=TRUE)
quantile(gx$Caffeine, na.rm=TRUE)
quantile(gx$Pulse, na.rm=TRUE)
quantile(gx$Systolic, na.rm=TRUE)
quantile(gx$Diastolic, na.rm=TRUE)

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
                                    "$75,000 and Over"=11, "Over $20,000"=8, "Under $20,000"=2.5,
                                    "Refused"=0, "Don't know"=0, "$20,000 and Over"=9))

gdmy <- dummyVars(" ~ .", data = gxt)
NHANES_dummy <- data.frame(predict(gdmy, newdata = gxt))

dummy <- NHANES_dummy
head(dummy)

#Removing family history no, don't know and refused
dummy= dummy[,-44:-46]
#Removing smoking no, don't know, and refused
dummy= dummy[,-33:-35]
#removing all other instances of doctor told you have diabetes beyond the yes
#column, X.Doctor.told.you.have.diabetes.Yes is a binary and should be all I need
dummy= dummy[, -28:-31]
#Removing HPB.Don.t.know and no 
dummy= dummy[, -25:-26]
#Removing Alcohol no and don't know
dummy= dummy[,-22:-23]
#Removing  Year, PSU, Stratum
dummy = dummy[,-11:-20]
#Removing female as reference category for gender
dummy= dummy[,-2]
head(dummy); dim(dummy)
ncol(dummy)

dummy$Q <- ifelse(dummy$Diabetes.Yes >= 1, 1,0)
dummy$Q
dummy$Diabetes.Yes
dummy$Z <- ifelse(dummy$BloodGlucose >=126, 1, 0)
dummy$Z

dummy$Diabetes <- ifelse(dummy$Q>=1 | dummy$Z>=1, 1, 0)

# add diabetics values of Z when Q is NA
id.naQ = which(is.na(dummy$Q)&dummy$Z>=0)
dummy$Diabetes[id.naQ]=dummy$Z[id.naQ]
#table(dummy$Diabetes)

# id.Z.1 = which(dummy$Z>0)
# dummy[id.naQ,"Diabetes"]=dummy[id.naQ,"Z"]
# dummy[id.Z.1,"Diabetes"]=dummy[id.Z.1,"Z"]

head(dummy);tail(dummy)

ncol(dummy)
dummy$Diabetes
#removing Q and Z temp columns
dummy= dummy[, -34:-35]
#removing BloodGlucose column
dummy= dummy[, -22]
#removing Diabetes.Yes column
dummy= dummy[, -12]
head(dummy)

dummyA <- dummy %>% drop_na(Diabetes)
head(dummyA)
nrow(dummyA)
nrow(dummy)
dummyB <- drop_na(dummyA)
nrow(dummyB)

#splitting white and black datasets for complete cases
NHANES_dmywhite <- dummyB[dummyB[, "Race.Non.Hispanic.White"] == 1,]
head(NHANES_dmywhite)
NHANES_dmyblack <- dummyB[dummyB[, "Race.Non.Hispanic.Black"] == 1,]
head(NHANES_dmyblack)
#removing other racial categories from black subset
NHANES_dmyblack= NHANES_dmyblack[, -3:-7]
head(NHANES_dmyblack)
#removing other racial categories from white subset
NHANES_dmywhite= NHANES_dmywhite[, -3:-7]
head(NHANES_dmywhite)
ncol(NHANES_dmywhite)
ncol(NHANES_dmyblack)

#Creating train and test sets for complete cases
set.seed(13785)
trainwhite <- createDataPartition(NHANES_dmywhite$Diabetes, p=.8, list=FALSE, times=1)
train <- NHANES_dmywhite[trainwhite,]
trainL <- train
test <- NHANES_dmywhite[-trainwhite,]
test2 <- NHANES_dmywhite[-trainwhite,]
#output vector contains Diabetes values used in XGBoost syntax
output_vector <- train[ 'Diabetes'] == 1 
#establishing weight vector for commands that have a weight optio
weights <- train['surveyweight']
weights <- as.double(unlist(weights))
surveyweights <- weights
weights2 <- test['surveyweight']
weights2 <- as.double(unlist(weights2))

#removing surveyweight column 
#####################column 26 is the variable of survey weight, not 29
train = train[,-26] #train[, -29]
#removing Y column for XGBoost
train2= train[, -26]
train2 <- as.matrix(train2)

#removing surveyweight column
test2= test2[,-26]
#Removing Y Column
test3= test2[,-26]
test3 <- as.matrix(test3)


##############-survey weighted logistic analysis
dsgn.wt = svydesign(ids=~1,strata=NULL, weights = surveyweights,data=train)
logistic2 <- svyglm(Diabetes ~ Gender.Male + Age + Education + Income + Alcohol.Yes + HBP.Yes + Smoking.Yes + Weight + Height + Waist + Leg + BMI + Arm + ArmCircumference + FamilyHistory.Yes + Cholesterol + KcalIntake + Sodium + Carb + Fiber + Calcium + Caffeine + Pulse + Systolic + Diastolic,
                    family = quasibinomial(link = 'logit'), design = dsgn.wt)

###################-Yan Li new codes for survey-wieghted logistic analysis 
summary(logistic2)

importances2 <- varImp(logistic2)

importances2 %>%
  arrange(desc(Overall)) %>%
  top_n(20)

probs <- predict(logistic2, newdata = test2, type = "response")
predL <- ifelse(probs > 0.5, 1, 0)
head(probs)
head(predL)
#produces error: the data cannot have more levels than the reference
confusionMatrix(factor(predL), factor(test2$Diabetes), positive = as.character(1))
mean(predL)
mean(test2$Diabetes)
auc(test2$Diabetes, predL)
nrow(test2)
length(predL)