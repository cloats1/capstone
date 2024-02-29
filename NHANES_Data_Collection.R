library(nhanesA)
library(tidyverse)
library(dplyr)
library(survey)
library(caret)
library(xgboost)
library(Matrix) 
library(data.table)

DEMO_B <- nhanes('DEMO_B')
ALQ_B <- nhanes('ALQ_B')
BPQ_B <- nhanes('BPQ_B')
DIQ_B <- nhanes('DIQ_B') 
SMQ_B <- nhanes('SMQ_B')
BMX_B <- nhanes('BMX_B')

DEMO_B <- DEMO_B %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                  INDHHINC, INDFMINC, WTMEC2YR)
ALQ_B <- ALQ_B %>% select(SEQN, ALD100)
BPQ_B <- BPQ_B %>% select(SEQN, BPQ020)
DIQ_B <- DIQ_B %>% select(SEQN, DIQ010)
SMQ_B <- SMQ_B %>% select(SEQN, SMQ020) 
BMX_B <- BMX_B %>% select(SEQN, BMXWT, BMXHT, BMXWAIST)


C1 <- left_join(DEMO_B, ALQ_B, by="SEQN")
C2 <- left_join(C1, BPQ_B, by="SEQN")
C3 <- left_join(C2, DIQ_B, by="SEQN")
C4 <- left_join(C3, SMQ_B, by="SEQN")
NHANES2001 <- left_join(C4, BMX_B, by="SEQN")

NHANES2001 <- NHANES2001 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2001 <- NHANES2001 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2001 <- NHANES2001 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2001 <- NHANES2001 %>% mutate(INDHHINC=recode(INDHHINC, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '11'='$75,000 and Over', '12'='$20,000 and Over', '13'='Under $20,000', 
                                                    '77'='Refused', '99'='Do not know'))
NHANES2001 <- NHANES2001 %>% mutate(INDFMINC=recode(INDFMINC, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '11'='$75,000 and Over', '12'='$20,000 and Over', '13'='Under $20,000', 
                                                    '77'='Refused', '99'='Do not know'))
NHANES2001 <- NHANES2001 %>% mutate(ALD100=recode(ALD100, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2001 <- NHANES2001 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2001 <- NHANES2001 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2001 <- NHANES2001 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2001 <- as_tibble(NHANES2001)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHINC",
            "Annual family income"= "INDFMINC", "Had at least 12 alcohol drinks/ 1 yr?"= "ALD100",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2001 <- rename (NHANES2001, all_of(lookup))

#2003-2005 NHANES
DEMO_C <- nhanes('DEMO_C')
ALQ_C <- nhanes('ALQ_C')
BPQ_C <- nhanes('BPQ_C')
DIQ_C <- nhanes('DIQ_C')
SMQ_C <- nhanes('SMQ_C')
BMX_C <- nhanes('BMX_C')

DEMO_C <- DEMO_C %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, INDFMINC, WTMEC2YR)
ALQ_C <- ALQ_C %>% select(SEQN, ALQ101)
BPQ_C <- BPQ_C %>% select(SEQN, BPQ020)
DIQ_C <- DIQ_C %>% select(SEQN, DIQ010)
SMQ_C <- SMQ_C %>% select(SEQN, SMQ020)
BMX_C <- BMX_C %>% select(SEQN, BMXWT, BMXHT, BMXWAIST)

C1 <- left_join(DEMO_C, ALQ_C, by="SEQN")
C2 <- left_join(C1, BPQ_C, by="SEQN")
C3 <- left_join(C2, DIQ_C, by="SEQN")
C4 <- left_join(C3, SMQ_C, by="SEQN")
NHANES2003 <- left_join(C4, BMX_C, by="SEQN")
# install.packages("gtsummary")
NHANES2003 <- NHANES2003 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2003 <- NHANES2003 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2003 <- NHANES2003 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2003 <- NHANES2003 %>% mutate(INDHHINC=recode(INDHHINC, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '11'='$75,000 and Over', '12'='$20,000 and Over', '13'='Under $20,000', 
                                                    '77'='Refused', '99'='Do not know'))
NHANES2003 <- NHANES2003 %>% mutate(INDFMINC=recode(INDFMINC, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '11'='$75,000 and Over', '12'='$20,000 and Over', '13'='Under $20,000', 
                                                    '77'='Refused', '99'='Do not know'))
NHANES2003 <- NHANES2003 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2003 <- NHANES2003 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2003 <- NHANES2003 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2003 <- NHANES2003 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2003 <- as_tibble(NHANES2003)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHINC",
            "Annual family income"= "INDFMINC", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2003 <- rename (NHANES2003, all_of(lookup))

#2005-2006 NHANES
DEMO_D <- nhanes('DEMO_D')
ALQ_D <- nhanes('ALQ_D')
BPQ_D <- nhanes('BPQ_D')
DIQ_D <- nhanes('DIQ_D')
SMQ_D <- nhanes('SMQ_D')
BMX_D <- nhanes('BMX_D')

DEMO_D <- DEMO_D %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2,
                            INDHHINC, INDFMINC, WTMEC2YR)
ALQ_D <- ALQ_D %>% select(SEQN, ALQ101)
BPQ_D <- BPQ_D %>% select(SEQN, BPQ020)
DIQ_D <- DIQ_D %>% select(SEQN, DIQ010)
SMQ_D <- SMQ_D %>% select(SEQN, SMQ020)
BMX_D <- BMX_D %>% select(SEQN, BMXWT, BMXHT, BMXWAIST)

C1 <- left_join(DEMO_D, ALQ_D, by="SEQN")
C2 <- left_join(C1, BPQ_D, by="SEQN")
C3 <- left_join(C2, DIQ_D, by="SEQN")
C4 <- left_join(C3, SMQ_D, by="SEQN")
NHANES2005 <- left_join(C4, BMX_D, by="SEQN")
# install.packages("gtsummary")
NHANES2005 <- NHANES2005 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2005 <- NHANES2005 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2005 <- NHANES2005 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2005 <- NHANES2005 %>% mutate(INDHHINC=recode(INDHHINC, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '11'='$75,000 and Over', '12'='$20,000 and Over', '13'='Under $20,000', 
                                                    '77'='Refused', '99'='Do not know'))
NHANES2005 <- NHANES2005 %>% mutate(INDFMINC=recode(INDFMINC, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '11'='$75,000 and Over', '12'='$20,000 and Over', '13'='Under $20,000', 
                                                    '77'='Refused', '99'='Do not know'))
NHANES2005 <- NHANES2005 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2005 <- NHANES2005 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2005 <- NHANES2005 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2005 <- NHANES2005 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2005 <- as_tibble(NHANES2005)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1",
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHINC",
            "Annual family income"= "INDFMINC", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2005 <- rename (NHANES2005, all_of(lookup))

#Commands to download sections of the 2007-2008 [E] NHANES and grab variables needed
#Family history of diabetes not in set
download.file("https://wwwn.cdc.gov/nchs/nhanes/2007-2008/DEMO_E.XPT", tf <- tempfile(), mode="wb")
DEMO_E <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                                     "INDHHIN2","INDFMIN2","WTMEC2YR")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/ALQ_E.XPT", tf <- tempfile(), mode="wb")
ALQ_E <- foreign::read.xport(tf)[,c("SEQN","ALQ101")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BPQ_E.XPT", tf <- tempfile(), mode="wb")
BPQ_E <- foreign::read.xport(tf)[,c("SEQN","BPQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DIQ_E.XPT", tf <- tempfile(), mode="wb")
DIQ_E <- foreign::read.xport(tf)[,c("SEQN","DIQ010")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/SMQ_E.XPT", tf <- tempfile(), mode="wb")
SMQ_E <- foreign::read.xport(tf)[,c("SEQN","SMQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT", tf <- tempfile(), mode="wb")
BMX_E <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT","BMXWAIST")]

library(dplyr)
library(survey)
C1 <- left_join(DEMO_E, ALQ_E, by="SEQN")
C2 <- left_join(C1, BPQ_E, by="SEQN")
C3 <- left_join(C2, DIQ_E, by="SEQN")
C4 <- left_join(C3, SMQ_E, by="SEQN")
NHANES2007 <- left_join(C4, BMX_E, by="SEQN")
# install.packages("gtsummary")
NHANES2007 <- NHANES2007 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2007 <- NHANES2007 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2007 <- NHANES2007 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2007 <- NHANES2007 %>% mutate(INDHHIN2=recode(INDHHIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2007 <- NHANES2007 %>% mutate(INDFMIN2=recode(INDFMIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2007 <- NHANES2007 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2007 <- NHANES2007 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2007 <- NHANES2007 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2007 <- NHANES2007 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2007 <- as_tibble(NHANES2007)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHIN2",
            "Annual family income"= "INDFMIN2", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2007 <- rename (NHANES2007, all_of(lookup))


#Commands to download sections of the 2009-2010 [F] NHANES and grab variables needed
#Family history not in the set
download.file("https://wwwn.cdc.gov/nchs/nhanes/2009-2010/DEMO_F.XPT", tf <- tempfile(), mode="wb")
DEMO_F <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                                     "INDHHIN2","INDFMIN2","WTMEC2YR")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/ALQ_F.XPT", tf <- tempfile(), mode="wb")
ALQ_F <- foreign::read.xport(tf)[,c("SEQN","ALQ101")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BPQ_F.XPT", tf <- tempfile(), mode="wb")
BPQ_F <- foreign::read.xport(tf)[,c("SEQN","BPQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DIQ_F.XPT", tf <- tempfile(), mode="wb")
DIQ_F <- foreign::read.xport(tf)[,c("SEQN","DIQ010")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/SMQ_F.XPT", tf <- tempfile(), mode="wb")
SMQ_F <- foreign::read.xport(tf)[,c("SEQN","SMQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT", tf <- tempfile(), mode="wb")
BMX_F <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT","BMXWAIST")]

library(dplyr)
library(survey)
C1 <- left_join(DEMO_F, ALQ_F, by="SEQN")
C2 <- left_join(C1, BPQ_F, by="SEQN")
C3 <- left_join(C2, DIQ_F, by="SEQN")
C4 <- left_join(C3, SMQ_F, by="SEQN")
NHANES2009 <- left_join(C4, BMX_F, by="SEQN")
# install.packages("gtsummary")
NHANES2009 <- NHANES2009 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2009 <- NHANES2009 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2009 <- NHANES2009 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2009 <- NHANES2009 %>% mutate(INDHHIN2=recode(INDHHIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2009 <- NHANES2009 %>% mutate(INDFMIN2=recode(INDFMIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2009 <- NHANES2009 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2009 <- NHANES2009 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2009 <- NHANES2009 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2009 <- NHANES2009 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2009 <- as_tibble(NHANES2009)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHIN2",
            "Annual family income"= "INDFMIN2", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2009 <- rename (NHANES2009, all_of(lookup))


#Commands to download sections of the 2011-2012 [G] NHANES and grab variables needed
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT", tf <- tempfile(), mode="wb")
DEMO_G <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                                     "INDHHIN2","INDFMIN2","WTMEC2YR")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ALQ_G.XPT", tf <- tempfile(), mode="wb")
ALQ_G <- foreign::read.xport(tf)[,c("SEQN","ALQ101")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPQ_G.XPT", tf <- tempfile(), mode="wb")
BPQ_G <- foreign::read.xport(tf)[,c("SEQN","BPQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT", tf <- tempfile(), mode="wb")
DIQ_G <- foreign::read.xport(tf)[,c("SEQN","DIQ010","DIQ175A")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SMQ_G.XPT", tf <- tempfile(), mode="wb")
SMQ_G <- foreign::read.xport(tf)[,c("SEQN","SMQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT", tf <- tempfile(), mode="wb")
BMX_G <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT","BMXWAIST")]

library(dplyr)
library(survey)
C1 <- left_join(DEMO_G, ALQ_G, by="SEQN")
C2 <- left_join(C1, BPQ_G, by="SEQN")
C3 <- left_join(C2, DIQ_G, by="SEQN")
C4 <- left_join(C3, SMQ_G, by="SEQN")
NHANES2011 <- left_join(C4, BMX_G, by="SEQN")
# install.packages("gtsummary")
NHANES2011 <- NHANES2011 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2011 <- NHANES2011 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2011 <- NHANES2011 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2011 <- NHANES2011 %>% mutate(INDHHIN2=recode(INDHHIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2011 <- NHANES2011 %>% mutate(INDFMIN2=recode(INDFMIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2011 <- NHANES2011 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2011 <- NHANES2011 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2011 <- NHANES2011 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2011 <- NHANES2011 %>% mutate(DIQ175A=recode(DIQ175A, "10" ='Family History', "77" ='Refused', 
                                                   "99"="Do not know"))
NHANES2011 <- NHANES2011 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2011 <- as_tibble(NHANES2011)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHIN2",
            "Annual family income"= "INDFMIN2", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Family history of Diabetes" ="DIQ175A", "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2011 <- rename (NHANES2011, all_of(lookup))


#Commands to download sections of the 2013-2014 [H] NHANES and grab variables needed
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", tf <- tempfile(), mode="wb")
DEMO_H <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                                     "INDHHIN2","INDFMIN2","WTMEC2YR")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/ALQ_H.XPT", tf <- tempfile(), mode="wb")
ALQ_H <- foreign::read.xport(tf)[,c("SEQN","ALQ101")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPQ_H.XPT", tf <- tempfile(), mode="wb")
BPQ_H <- foreign::read.xport(tf)[,c("SEQN","BPQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.XPT", tf <- tempfile(), mode="wb")
DIQ_H <- foreign::read.xport(tf)[,c("SEQN","DIQ010","DIQ175A")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.XPT", tf <- tempfile(), mode="wb")
SMQ_H <- foreign::read.xport(tf)[,c("SEQN","SMQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT", tf <- tempfile(), mode="wb")
BMX_H <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT","BMXWAIST")]

library(dplyr)
library(survey)
C1 <- left_join(DEMO_H, ALQ_H, by="SEQN")
C2 <- left_join(C1, BPQ_H, by="SEQN")
C3 <- left_join(C2, DIQ_H, by="SEQN")
C4 <- left_join(C3, SMQ_H, by="SEQN")
NHANES2013 <- left_join(C4, BMX_H, by="SEQN")
# install.packages("gtsummary")
NHANES2013 <- NHANES2013 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2013 <- NHANES2013 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2013 <- NHANES2013 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2013 <- NHANES2013 %>% mutate(INDHHIN2=recode(INDHHIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2013 <- NHANES2013 %>% mutate(INDFMIN2=recode(INDFMIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2013 <- NHANES2013 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2013 <- NHANES2013 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2013 <- NHANES2013 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2013 <- NHANES2013 %>% mutate(DIQ175A=recode(DIQ175A, "10" ='Family History', "77" ='Refused', 
                                                   "99"="Do not know"))
NHANES2013 <- NHANES2013 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2013 <- as_tibble(NHANES2013)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHIN2",
            "Annual family income"= "INDFMIN2", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Family history of Diabetes" ="DIQ175A", "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2013 <- rename (NHANES2013, all_of(lookup))


#Commands to download sections of the 2015-2016 [I] NHANES and grab variables needed
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_I <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                                     "INDHHIN2","INDFMIN2","WTMEC2YR")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/ALQ_I.XPT", tf <- tempfile(), mode="wb")
ALQ_I <- foreign::read.xport(tf)[,c("SEQN","ALQ101")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPQ_I.XPT", tf <- tempfile(), mode="wb")
BPQ_I <- foreign::read.xport(tf)[,c("SEQN","BPQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT", tf <- tempfile(), mode="wb")
DIQ_I <- foreign::read.xport(tf)[,c("SEQN","DIQ010","DIQ175A")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT", tf <- tempfile(), mode="wb")
SMQ_I <- foreign::read.xport(tf)[,c("SEQN","SMQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT", tf <- tempfile(), mode="wb")
BMX_I <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT","BMXWAIST")]

library(dplyr)
library(survey)
C1 <- left_join(DEMO_I, ALQ_I, by="SEQN")
C2 <- left_join(C1, BPQ_I, by="SEQN")
C3 <- left_join(C2, DIQ_I, by="SEQN")
C4 <- left_join(C3, SMQ_I, by="SEQN")
NHANES2015 <- left_join(C4, BMX_I, by="SEQN")
# install.packages("gtsummary")
NHANES2015 <- NHANES2015 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2015 <- NHANES2015 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2015 <- NHANES2015 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2015 <- NHANES2015 %>% mutate(INDHHIN2=recode(INDHHIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2015 <- NHANES2015 %>% mutate(INDFMIN2=recode(INDFMIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2015 <- NHANES2015 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                                  "9"="Do not know"))
NHANES2015 <- NHANES2015 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2015 <- NHANES2015 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2015 <- NHANES2015 %>% mutate(DIQ175A=recode(DIQ175A, "10" ='Family History', "77" ='Refused', 
                                                   "99"="Do not know"))
NHANES2015 <- NHANES2015 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2015 <- as_tibble(NHANES2015)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHIN2",
            "Annual family income"= "INDFMIN2", "Had at least 12 alcohol drinks/ 1 yr?"= "ALQ101",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Family history of Diabetes" ="DIQ175A", "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2015 <- rename (NHANES2015, all_of(lookup))


#Commands to download sections of the 2017-2018 [J] NHANES and grab variables needed
#Alcohol use variable has changed
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
DEMO_J <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2",
                                     "INDHHIN2","INDFMIN2","WTMEC2YR")]
#download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/ALQ_J.XPT", tf <- tempfile(), mode="wb")
#ALQ_J <- foreign::read.xport(tf)[,c("SEQN","ALQ101")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPQ_J.XPT", tf <- tempfile(), mode="wb")
BPQ_J <- foreign::read.xport(tf)[,c("SEQN","BPQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT", tf <- tempfile(), mode="wb")
DIQ_J <- foreign::read.xport(tf)[,c("SEQN","DIQ010","DIQ175A")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT", tf <- tempfile(), mode="wb")
SMQ_J <- foreign::read.xport(tf)[,c("SEQN","SMQ020")]
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT", tf <- tempfile(), mode="wb")
BMX_J <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT","BMXWAIST")]

library(dplyr)
library(survey)
#C1 <- left_join(DEMO_H, ALQ_J, by="SEQN")
C2 <- left_join(DEMO_H, BPQ_J, by="SEQN")
C3 <- left_join(C2, DIQ_J, by="SEQN")
C4 <- left_join(C3, SMQ_J, by="SEQN")
NHANES2017 <- left_join(C4, BMX_J, by="SEQN")
# install.packages("gtsummary")
NHANES2017 <- NHANES2017 %>% mutate(RIAGENDR=recode(RIAGENDR, "1" ='Male', "2" ='Female'))
NHANES2017 <- NHANES2017 %>% mutate(RIDRETH1=recode(RIDRETH1, "1" ='Mexican American',
                                                    "2" ='Other Hispanic', "3"="Non-Hispanic White",
                                                    "4"='Non-Hispanic Black', '5'= 'Other Race - Including Multi-Racial'))
NHANES2017 <- NHANES2017 %>% mutate(DMDEDUC2=recode(DMDEDUC2, '1'='Less than 9th grade',
                                                    '2'='9-11th grade (Includes 12th grade with no diploma)',
                                                    '3'='High school graduate/GED or equivalent',
                                                    '4'='Some college or AA degree', '5'='College graduate or above',
                                                    '7'='Refused', '9'='Do not know'))
NHANES2017 <- NHANES2017 %>% mutate(INDHHIN2=recode(INDHHIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
NHANES2017 <- NHANES2017 %>% mutate(INDFMIN2=recode(INDFMIN2, "1" ='$ 0 to $ 4,999',
                                                    "2" ='$ 5,000 to $ 9,999', '3'='$10,000 to $14,999', '4'='$15,000 to $19,999',
                                                    '5'='$20,000 to $24,999', '6'='$25,000 to $34,999', '7'='$35,000 to $44,999',
                                                    '8'='$45,000 to $54,999', '9'='$55,000 to $64,999', '10'='$65,000 to $74,999',
                                                    '12'='$20,000 and Over', '13'='Under $20,000', '14'='$75,000 and Over',
                                                    '15'='$75,000 and Over', '77'='Refused', '99'='Do not know'))
#NHANES2017 <- NHANES2017 %>% mutate(ALQ101=recode(ALQ101, "1" ='Yes', "2" ='No', 
                                             #     "9"="Do not know"))
NHANES2017 <- NHANES2017 %>% mutate(BPQ020=recode(BPQ020, "1" ='Yes', "2" ='No', 
                                                  '7'='Refused', "9"='Do not know'))
NHANES2017 <- NHANES2017 %>% mutate(DIQ010=recode(DIQ010, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))
NHANES2017 <- NHANES2017 %>% mutate(DIQ175A=recode(DIQ175A, "10" ='Family History', "77" ='Refused', 
                                                   "99"="Do not know"))
NHANES2017 <- NHANES2017 %>% mutate(SMQ020=recode(SMQ020, "1" ='Yes', "2" ='No',
                                                  '3'='Borderline', '7'='Refused',
                                                  "9"="Do not know"))

NHANES2017 <- as_tibble(NHANES2017)
lookup <- c("Gender"="RIAGENDR", "Age in years at screening" ="RIDAGEYR", 
            "Race/Hispanic Origin"= "RIDRETH1", 
            "Education level- Adults 20+" = "DMDEDUC2", "Annual household income"= "INDHHIN2",
            "Annual family income"= "INDFMIN2",
            "Ever told you had high blood pressure"= "BPQ020", "Doctor told you have diabetes"= "DIQ010",
            "Family history of Diabetes" ="DIQ175A", "Smoked at least 100 cigarettes in life"= "SMQ020",
            "Weight (kg)" ="BMXWT", "Standing Height (cm)"="BMXHT", "Waist Circumference (cm)"= "BMXWAIST")
NHANES2017 <- rename (NHANES2017, all_of(lookup))

