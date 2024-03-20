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

DEMO_B <- nhanes('DEMO_B')
ALQ_B <- nhanes('ALQ_B')
BPQ_B <- nhanes('BPQ_B')
DIQ_B <- nhanes('DIQ_B') 
SMQ_B <- nhanes('SMQ_B')
BMX_B <- nhanes('BMX_B')
MCQ_B <- nhanes('MCQ_B')
l13_b <- nhanes('l13_b')
DRXTOT_B <- nhanes('DRXTOT_B')
BPX_B <- nhanes('BPX_B')

DEMO_B <- DEMO_B %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, WTMEC2YR)
ALQ_B <- ALQ_B %>% select(SEQN, ALQ130)
BPQ_B <- BPQ_B %>% select(SEQN, BPQ020)
DIQ_B <- DIQ_B %>% select(SEQN, DIQ010)
SMQ_B <- SMQ_B %>% select(SEQN, SMD030) 
BMX_B <- BMX_B %>% select(SEQN, BMXWT, BMXHT, BMXWAIST, BMXLEG, BMXBMI, BMXARML, BMXARMC)
MCQ_B <- MCQ_B %>% select(SEQN, MCQ250A)
l13_b <- l13_b %>% select(SEQN, LBXTC)
DRXTOT_B <- DRXTOT_B %>% select(SEQN, DRXTKCAL, DRDTSODI, DRXTCARB, DRXTFIBE,
                                DRXTCALC, DRXTCAFF)
BPX_B <- BPX_B %>% select(SEQN, BPXPLS, BPXSY1, BPXDI1)


C1 <- left_join(DEMO_B, ALQ_B, by="SEQN")
C2 <- left_join(C1, BPQ_B, by="SEQN")
C3 <- left_join(C2, DIQ_B, by="SEQN")
C4 <- left_join(C3, SMQ_B, by="SEQN")
C5 <- left_join(C4, BMX_B, by="SEQN")
C6 <- left_join(C5, MCQ_B, by="SEQN")
C8 <- left_join(C6, l13_b, by="SEQN")
C9 <- left_join(C8, DRXTOT_B, by="SEQN")
NHANES2001 <- left_join(C9, BPX_B, by="SEQN")
xnhanes2001 <- NHANES2001

NHANES2001 <- NHANES2001 %>% mutate(INDHHINC=recode(INDHHINC, "$     0 to $ 4,999" ='$ 0 to $ 4,999'))

NHANES2001 <- as_tibble(NHANES2001)
lookup <- c("Gender"="RIAGENDR", "Age" ="RIDAGEYR", 
            "Race"= "RIDRETH1", 
            "Education" = "DMDEDUC2", "Income"= "INDHHINC",
             "Alcohol"= "ALQ130", "HBP"= "BPQ020", "Diabetes"= "DIQ010",
             "Smoking"= "SMD030", "Weight" ="BMXWT",
             "Height"="BMXHT", "Waist"= "BMXWAIST", "Leg"="BMXLEG", "BMI"="BMXBMI",
            "Cholesterol"="LBXTC", "FamilyHistory"="MCQ250A",
            "Sodium"="DRDTSODI", "Carb"="DRXTCARB", "Fiber"="DRXTFIBE",
            "Calcium"="DRXTCALC", "KcalIntake"="DRXTKCAL", "Caffeine"="DRXTCAFF",
            "ArmCircumference"="BMXARMC", "Arm"="BMXARML", "Pulse"="BPXPLS",
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
NHANES2001 <- rename (NHANES2001, all_of(lookup))

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

DEMO_C <- DEMO_C %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, WTMEC2YR)
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

C1 <- left_join(DEMO_C, ALQ_C, by="SEQN")
C2 <- left_join(C1, BPQ_C, by="SEQN")
C3 <- left_join(C2, DIQ_C, by="SEQN")
C4 <- left_join(C3, SMQ_C, by="SEQN")
C5 <- left_join(C4, BMX_C, by="SEQN")
C6 <- left_join(C5, MCQ_C, by="SEQN")
C8 <- left_join(C6, l13_c, by="SEQN")
C9 <- left_join(C8, DR1TOT_C, by="SEQN")
NHANES2003 <- left_join(C9, BPX_C, by="SEQN")
xnhanes2003 <- NHANES2003

NHANES2003 <- NHANES2003 %>% mutate(INDHHINC=recode(INDHHINC, "$     0 to $ 4,999" ='$ 0 to $ 4,999'))

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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
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

DEMO_D <- DEMO_D %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, WTMEC2YR)
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

C1 <- left_join(DEMO_D, ALQ_D, by="SEQN")
C2 <- left_join(C1, BPQ_D, by="SEQN")
C3 <- left_join(C2, DIQ_D, by="SEQN")
C4 <- left_join(C3, SMQ_D, by="SEQN")
C5 <- left_join(C4, BMX_D, by="SEQN")
C6 <- left_join(C5, MCQ_D, by="SEQN")
C8 <- left_join(C6, TCHOL_D, by="SEQN")
C9 <- left_join(C8, DR1TOT_D, by="SEQN")
NHANES2005 <- left_join(C9, BPX_D, by="SEQN")
xnhanes2005 <- NHANES2005

NHANES2005 <- NHANES2005 %>% mutate(INDHHINC=recode(INDHHINC, "$     0 to $ 4,999" ='$ 0 to $ 4,999'))

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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
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

DEMO_E <- DEMO_E %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR)
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

C1 <- left_join(DEMO_E, ALQ_E, by="SEQN")
C2 <- left_join(C1, BPQ_E, by="SEQN")
C3 <- left_join(C2, DIQ_E, by="SEQN")
C4 <- left_join(C3, SMQ_E, by="SEQN")
C5 <- left_join(C4, BMX_E, by="SEQN")
C6 <- left_join(C5, MCQ_E, by="SEQN")
C8 <- left_join(C6, TCHOL_E, by="SEQN")
C9 <- left_join(C8, DR1TOT_E, by="SEQN")
NHANES2007 <- left_join(C9, BPX_E, by="SEQN")
xnhanes2007 <- NHANES2007

NHANES2007 <- NHANES2007 %>% mutate(INDHHIN2=recode(INDHHIN2, "$     0 to $ 4,999" ='$ 0 to $ 4,999',
                                                    '$75,000 to $99,999'='$75,000 and Over',
                                                    '$100,000 and Over'='$75,000 and Over'))

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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
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

DEMO_F <- DEMO_F %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR)
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

C1 <- left_join(DEMO_F, ALQ_F, by="SEQN")
C2 <- left_join(C1, BPQ_F, by="SEQN")
C3 <- left_join(C2, DIQ_F, by="SEQN")
C4 <- left_join(C3, SMQ_F, by="SEQN")
C5 <- left_join(C4, BMX_F, by="SEQN")
C6 <- left_join(C5, MCQ_F, by="SEQN")
C8 <- left_join(C6, TCHOL_F, by="SEQN")
C9 <- left_join(C8, DR1TOT_F, by="SEQN")
NHANES2009 <- left_join(C9, BPX_F, by="SEQN")
xnhanes2009 <- NHANES2009

NHANES2009 <- NHANES2009 %>% mutate(INDHHIN2=recode(INDHHIN2, "$     0 to $ 4,999" ='$ 0 to $ 4,999', 
                                                    '$75,000 to $99,999'='$75,000 and Over', 
                                                    '$100,000 and Over'='$75,000 and Over'))

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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
NHANES2009 <- rename (NHANES2009, all_of(lookup))

#Nhanes 2010-2011 G
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

DEMO_G <- DEMO_G %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR)
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

C1 <- left_join(DEMO_G, ALQ_G, by="SEQN")
C2 <- left_join(C1, BPQ_G, by="SEQN")
C3 <- left_join(C2, DIQ_G, by="SEQN")
C4 <- left_join(C3, SMQ_G, by="SEQN")
C5 <- left_join(C4, BMX_G, by="SEQN")
C6 <- left_join(C5, MCQ_G, by="SEQN")
C8 <- left_join(C6, TCHOL_G, by="SEQN")
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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
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

DEMO_H <- DEMO_H %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR)
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

C1 <- left_join(DEMO_H, ALQ_H, by="SEQN")
C2 <- left_join(C1, BPQ_H, by="SEQN")
C3 <- left_join(C2, DIQ_H, by="SEQN")
C4 <- left_join(C3, SMQ_H, by="SEQN")
C5 <- left_join(C4, BMX_H, by="SEQN")
C6 <- left_join(C5, MCQ_H, by="SEQN")
C8 <- left_join(C6, TCHOL_H, by="SEQN")
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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
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

DEMO_I <- DEMO_I %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR)
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

C1 <- left_join(DEMO_I, ALQ_I, by="SEQN")
C2 <- left_join(C1, BPQ_I, by="SEQN")
C3 <- left_join(C2, DIQ_I, by="SEQN")
C4 <- left_join(C3, SMQ_I, by="SEQN")
C5 <- left_join(C4, BMX_I, by="SEQN")
C6 <- left_join(C5, MCQ_I, by="SEQN")
C8 <- left_join(C6, TCHOL_I, by="SEQN")
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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
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

DEMO_J <- DEMO_J %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHIN2, WTMEC2YR)
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

C1 <- left_join(DEMO_J, ALQ_J, by="SEQN")
C2 <- left_join(C1, BPQ_J, by="SEQN")
C3 <- left_join(C2, DIQ_J, by="SEQN")
C4 <- left_join(C3, SMQ_J, by="SEQN")
C5 <- left_join(C4, BMX_J, by="SEQN")
C6 <- left_join(C5, MCQ_J, by="SEQN")
C8 <- left_join(C6, TCHOL_J, by="SEQN")
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
            "Diastolic"="BPXDI1", "Systolic"="BPXSY1")
NHANES2017 <- rename (NHANES2017, all_of(lookup))

#Merging all datasets together
C1 <- rbind(NHANES2001, NHANES2003)
C2 <- rbind(C1, NHANES2005)
C3 <- rbind(C2, NHANES2007)
C4 <- rbind(C3, NHANES2009)
C5 <- rbind(C4, NHANES2011)
C6 <- rbind(C5, NHANES2013)
C7 <- rbind(C6, NHANES2015)
NHANES_full <- rbind(C7, NHANES2017)
ls(NHANES_full)

#Removing those 19 and younger, and creating survey weights
NHANES_full = subset(NHANES_full, NHANES_full$Age > 19)
NHANES_full <- NHANES_full %>% mutate(surveyweight = 1/9 * WTMEC2YR)

#Removing sequence identifier and unadjusted weights
gx <- data.table(NHANES_full, keep.rownames = FALSE)
gx[, SEQN := NULL]
gx[, WTMEC2YR := NULL]
head(gx)

#creating dummy variables for the categorical variables
gdmy <- dummyVars(" ~ .", data = gx)
NHANES_dummy <- data.frame(predict(gdmy, newdata = gx))
dummy <- NHANES_dummy  %>% drop_na(Diabetes.Yes)
head(dummy)
ncol(dummy)
#removing all other instances of doctor told you have diabetes beyond the yes
#column, X.Doctor.told.you.have.diabetes.Yes is a binary and should be all I need
dummy= dummy[, -37:-40]
head(dummy)
# 2 ref, 4-8 race, 15 ref, 27-28 cut, 30 ref, 31 cut (income), 35 ref, 48 ref, 
#Removing FamilyHistory.Don.t.know as reference category for family history of diabetes
dummy= dummy[, -48]
#Removing HPB.Don.t.know as reference category for high blood pressure
dummy= dummy[, -35]
#Removing Income. . 20.000.and.0ver as uneeded column covered by other columns
dummy= dummy [, -31]
#removing Income.Don.t.know as reference category for income
dummy= dummy [,-30]
#Removing Income over/under 20,000
dummy= dummy[, -27:-28]
#Removing Education.Don.t.Know as reference category
dummy= dummy[, -15]
#Removing female as reference category for gender
dummy= dummy[,-2]
dummy$Race.Non.Hispanic.White
NHANES_dmywhite <- dummy[dummy[, "Race.Non.Hispanic.White"] == 1,]
head(NHANES_dmywhite)
NHANES_dmyblack <- dummy[dummy[, "Race.Non.Hispanic.Black"] == 1,]
head(NHANES_dmyblack)
#removing other racial categories from black subset
NHANES_dmyblack= NHANES_dmyblack[, -3:-7]
head(NHANES_dmyblack)
#removing other racial categories from white subset
NHANES_dmywhite= NHANES_dmywhite[, -3:-7]
head(NHANES_dmywhite)
ncol(NHANES_dmywhite)
ncol(NHANES_dmyblack)

trainwhite <- createDataPartition(NHANES_dmywhite$Diabetes.Yes, p=.8, list=FALSE, times=1)
train <- NHANES_dmywhite[trainwhite,]
test <- NHANES_dmywhite[-trainwhite,]
test2 <- NHANES_dmywhite[-trainwhite,]
output_vector <- train[ 'Diabetes.Yes'] == 1 
weights <- train['surveyweight']
weights <- as.double(unlist(weights))
weights2 <- test['surveyweight']
weights2 <- as.double(unlist(weights2))

#removing survey weights
train = train[, -46]
#removing Y column
train2= train[, -24]
train2 <- as.matrix(train2)
bsty <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 4,
                eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")
importance <- xgb.importance(feature_names = colnames(train2), model = bsty)
head(importance)

#removing survey weights from test set
test2 = test2[, -46]
#removing Y column from test set
test3= test2[, -24]
test3 <- as.matrix(test3)
predy <- predict(bsty, test3)
print(head(predy))
predictiony <- as.numeric(predy > 0.5)
print(head(predictiony))
mean(test2$Diabetes.Yes)
mean(predictiony)

#separating weights for dummy Black set
weights3 <- NHANES_dmyblack['surveyweight']
weights3 <- as.double(unlist(weights3))

#removing survey weights from Black set
btest= NHANES_dmyblack[,-46]
#removing Y column from Black set
btest= btest[,-24]
btest <- as.matrix(btest)
btest <- xgb.DMatrix(btest)
nrow(btest)
ncol(btest)

predB <- predict(bsty, btest)
print(head(predB))
predictionB <- as.numeric(predB > 0.5)
print(head(predictionB))
mean(NHANES_dmyblack$Diabetes.Yes)
mean(predictionB)

#Cross Validation of XGBoost parameters

ctrain <- NHANES_dmywhite[trainwhite,]
ctest <- NHANES_dmywhite[-trainwhite,]
ctrain = ctrain[, -46]
ctest = ctest[, -46]
output_vector <- train[ 'Diabetes.Yes'] == 1 

dtrain <- xgb.DMatrix(data = as.matrix(select(ctrain, -Diabetes.Yes))
                      , label = ctrain$Diabetes.Yes)
dtest <- xgb.DMatrix(data = as.matrix(select(ctest, -Diabetes.Yes)),
                     label = ctest$Diabetes.Yes)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3,
               gamma=0, max_depth=20, min_child_weight=1, subsample=1, colsample_bytree=1)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3,
               gamma=10, max_depth=20, min_child_weight=1, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 1000, nfold = 10, showsd = T,
                 stratified = T, print_every_n = 10, early_stopping_round = 20, maximize = F)
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 13, watchlist = 
                     list(val=dtest,train=dtrain), print_ever_n = 10, early_stopping_round = 10,
                   maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)
head(xgbpred)
mean(test2$Diabetes.Yes)
mean(xgbpred)

params2 <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3,
                gamma=0, max_depth=20, max_leaves=1023, min_child_weight=1,
                subsample=1, colsample_bytree=1)
xgbcv2 <- xgb.cv( params = params2, data = dtrain, nrounds = 1000, nfold = 10, showsd = T,
                  stratified = T, print_every_n = 10, early_stopping_round = 20, maximize = F)
xgb2 <- xgb.train (params = params2, data = dtrain, nrounds = 12, watchlist = 
                     list(val=dtest,train=dtrain), print_ever_n = 10, early_stopping_round = 10,
                   maximize = F , eval_metric = "error")

xgbpred2 <- predict (xgb2,dtest)
xgbpred2 <- ifelse (xgbpred2 > 0.5,1,0)
mean(test2$Diabetes.Yes)
mean(xgbpred2)

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 

auc(ctest$Diabetes.Yes, xgbpred)
auc(ctest$Diabetes.Yes, xgbpred2)

sim_roc <- roc(response = ctest$Diabetes.Yes,
               predictor = xgbpred,
               levels = c(0, 1))

ggroc(sim_roc, legacy.axes = TRUE) +
  labs(x = 'False-positive rate', y = 'True-positive rate', title = 'Simulated ROC curve')

#More complex cross validation
paramDF <- expand.grid(
  max_depth = seq(14, 60, by = 2),
  max_leaves = c(63, 127, 255, 511, 1023, 2047, 4095),
  eta = 0.1)
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
bestResults <- tibble()
pb <- txtProgressBar(style = 3)
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]],
                 data = dtrain, 
                 nrounds = 500, 
                 nfold = 10,
                 early_stopping_rounds = 10,
                 verbose = FALSE)
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() 
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb)

depth_leaves <- bind_cols(paramDF, bestResults)
View(depth_leaves)
#depth 58, leaves 63, eta 0.1
diabetestested <- xgb.train(data = dtrain, verbose = 0,
                            watchlist = list(train = dtrain, test = dtest), 
                            nrounds = 10000,
                            early_stopping_rounds = 50,
                            max_depth = 58,
                            max_leaves = 63,
                            subsample = 0.8,
                            colsample_bytree = 0.7,
                            eta = 0.05)
diabetestested
diabetestested$evaluation_log %>% 
  pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") %>% 
  ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()
print(diabetestested)

paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)

xgbpred1 <- predict (diabetestested,dtest)
xgbpred1 <- ifelse (xgbpred1 > 0.5,1,0)

mean(test2$Diabetes.Yes)
mean(xgbpred2)
mean(xgbpred)
mean(xgbpred1)

auc(ctest$Diabetes.Yes, xgbpred)
auc(ctest$Diabetes.Yes, xgbpred2)
auc(ctest$Diabetes.Yes, xgbpred1)


bstw <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 50, 
                max_leaves = 5000, eta = 0.001, nthread = 4, nrounds = 3000, 
                objective = "binary:logistic", eval_metric = "error")

predw <- predict(bstw, test3)
print(head(predw))
predictionw <- as.numeric(predw > 0.5)
print(head(predictionw))
mean(test2$Diabetes.Yes)
mean(predictionw)
auc(test2$Diabetes.Yes, predictionw)
#AUC = 0.6079777


#Running the training data back thru the model to confirm that the AUC function
#works as intended
bstauc <- xgboost(data = train2, label = output_vector, weight = weights, max_depth = 100, 
                max_leaves = 10000, eta = 0.001, nthread = 4, nrounds = 3000, 
                objective = "binary:logistic")

predauc <- predict(bstauc, train2)
print(head(predauc))
predictionauc <- as.numeric(predauc > 0.5)
print(head(predictionauc))
mean(train$Diabetes.Yes)
mean(predictionauc)
auc(train$Diabetes.Yes, predictionauc)
#AUC of 0.9758053

accuracy(test2$Diabetes.Yes, predictionw)

mean(test2$Diabetes.Yes)
mean(predictionw)

#Logistic Regression model
# Train a logistic regression model using tidymodel package
train3 <- as.data.frame(train)
train3$Diabetes.Yes <- as.factor(train3$Diabetes.Yes)
model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Diabetes.Yes ~ ., data = train3)

# Model summary
tidy(model)

#Should I be using test data that has the Y column for this form of logistic
#regression?
test4 <- as.data.frame(test3)
# Class Predictions
pred_class <- predict(model,
                      new_data = test4,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data = test4,
                      type = "prob")

results <- test4 %>%
  select(Diabetes.Yes) %>%
  bind_cols(pred_class, pred_proba)
results$Diabetes.Yes <- as.factor(results$Diabetes.Yes)
accuracy(results, truth = Diabetes.Yes, estimate = .pred_class)

#Using default glm in r
logistic <- glm(Diabetes.Yes ~ ., data = train, family = "binomial",
                na.action = na.exclude)
#look into other methods of dealing with NAs 
#Excluding unneeded columns from dummy variable expansion
#Need to look at my variables in general and possibly refactor again
head(train)
trainL = train[, -51]
trainL = trainL[, -38]
trainL = trainL[, -34]
trainL = trainL[, -15]
trainL = trainL[, -9]
trainL = trainL[, -2]
logistic <- glm(Diabetes.Yes ~ ., data = trainL, family = "binomial",
                na.action = na.exclude)
summary(logistic)

importances <- varImp(logistic)

importances %>%
  arrange(desc(Overall)) %>%
  top_n(20)

probs <- predict(logistic, newdata = test, type = "response")
predL <- ifelse(probs > 0.5, 1, 0)
head(probs)
head(predL)
#large amounts of NA values, need to address
confusionMatrix(factor(predL), factor(test$Diabetes.Yes), positive = as.character(1))
mean(na.exclude(predL))
mean(test$Diabetes.Yes)
auc(test$Diabetes.Yes, predL)

edu <- unique(unlist(strsplit(as.character(NHANES_full$Education), ",")))
print(edu)
inc <- unique(unlist(strsplit(as.character(NHANES_full$Income), ",")))
print(inc)