#Determination if patinet is cirrhotic
##Algorithm is to identify patients with no biospy or fibroscan data, with fib4 < 2, to review imaging
###Steps 1-3: elimination 
###Steps 4-x: assigment

#Step 1) Set working directory AIH folder, R projects, project name Determine_if_cirrhotic, and rename columns
#setwd("~/Documents/AIH/AIH_R_Projects/Determine_if_cirrhotic")
AIH_cases_QF4 <- read.csv(file = "~/Documents/Informatics/AIH/github/AIH-Project/AIH-metadata/AASLDAutoimmunePilot_DATA_2019-04-03_1202.csv", stringsAsFactors = FALSE)
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="ast_coll")] <- "AST"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="alt_coll")] <- "ALT"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="plt_coll")] <- "Plts"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="te_coll")] <- "fs_SC"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="bx_fib_coll")] <- "Bx_SC"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="ov_fib_coll")] <- "Overall_fibrosis_review"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="bx_fib_bl")] <- "Bx_BL"

colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="te_bl")] <- "fs_BL"
colnames(AIH_cases_QF4)[which(colnames(AIH_cases_QF4)=="age_coll")] <- "age"



#Step 2) add baseline bx and Fib4 data, creating Fib4 algorithm, setting bx_bl_coll as the Bx6m (did they get a biopsy within six months of sample collection?), calculate the fib4 score
Fib4 <- function (Age, AST, Plts, ALT) {
  ((Age)*(AST))/((Plts)*(sqrt(ALT)))
}

AIH_cases_QF4$Bx6m <- ifelse(AIH_cases_QF4$bx_bl_coll == 1, "yes",
                             ifelse(AIH_cases_QF4$bx_bl_coll == 0, "no", NA))
AIH_cases_QF4$fib4 <- Fib4 (AIH_cases_QF4$age, AIH_cases_QF4$AST, AIH_cases_QF4$Plts, AIH_cases_QF4$ALT)



#Step 3) For selection - recasting Stage "Stage 1" as 1 and then making biopsy numberic
#AIH_cases_QF4 [8,7] = 1 (this is already true)
AIH_cases_QF4$Bx_SC <- as.integer(as.character(AIH_cases_QF4$Bx_SC))



#Step 4) Selecting on baseline biopsy 1st and if not avail, biopsy w/in 6 mos,then fibrosacn at sample collection (>= 19 or < 13); 
#for fibrosacan at sample collection, if none of the former data was avaialbe and the fibroscan score was >=13 or < 19, 
#then if fib-4 was > 2.6, it was determined to be cirrhotic; if <= 2.6, the algorithm below applies)
AIH_cases_QF4$F03vF4 <- ifelse ( 
                                AIH_cases_QF4$Bx_SC < 4 & !is.na(AIH_cases_QF4$Bx_SC), print ("F0_F3"),
                                 ifelse (
                                   AIH_cases_QF4$Bx_SC %in% 4, print ("F4"),
                                    ifelse (
                                      (AIH_cases_QF4$Bx6m %in% "yes") & (AIH_cases_QF4$Bx_BL < 4) & !is.na(AIH_cases_QF4$Bx_BL), print ("F0_F3"),
                                      ifelse (
                                        (AIH_cases_QF4$Bx6m %in% "yes") & (AIH_cases_QF4$Bx_BL %in% 4), print ("F4"),
                                      ifelse(
                                        (AIH_cases_QF4$fs_SC >= 19), print("F4"),
                                         ifelse (
                                            (AIH_cases_QF4$fs_SC < 13 & !is.na(AIH_cases_QF4$fs_SC)), print("F0_F3"),
                                            ifelse (
                                              ((AIH_cases_QF4$fs_SC >= 13) & (!is.na(AIH_cases_QF4$fs_SC)) & (AIH_cases_QF4$fs_SC < 19) & (AIH_cases_QF4$fib4 >= 2.6)), print ("F4"), NA
                                            )))))))

AIH_cases_QF4$Fib4Call <- ifelse(
  AIH_cases_QF4$fib4 < 2.6 & !is.na(AIH_cases_QF4$fib4), print ("F0_F3"), NA)


#Step 5) read in updated overall fibrosis review data from "all data" downlod in redcap and then swap in new overall fibrosis review data
library(dplyr)
library(stringr)
library(magrittr)

overall_fibrosis_review <- read.csv ("~/Documents/Informatics/AIH/github/AIH-Project/AIH-metadata/AASLDAutoimmunePilot_DATA_2019-04-03_1202.csv")

#Select just the cases for casting overally fibrosis
ofr_cases <- filter(overall_fibrosis_review, case_hl_du == 1)
ofr_cases <- select(ofr_cases, aasld_id, ov_fib_coll)
AIH_cases_QF4_filter <- filter(AIH_cases_QF4, case_hl_du == 1)

#merge overall fibrosis review column (for cases) in master QF4 data with updated info from redcap red in above and then replace with new data
AIH_cases_QF4_filter$ofr <- ofr_cases$ov_fib_col
AIH_cases_QF4_filter$ofr_id_match <- ofr_cases$aasld_id
all(AIH_cases_QF4_filter$aasld_id == AIH_cases_QF4$ofr_id_match)

#Case CL calls as character, select columns that "start with" CL; update columns with CL to be numeric values
AIH_cases_QF4_filter$ofr <- as.character(AIH_cases_QF4_filter$ofr)

#Reassigning ofr column wtih ofr reassign as needed and then casting as integer
AIH_cases_QF4_filter$ofr <- ifelse (str_detect(AIH_cases_QF4_filter$ofr, 'CL'), str_sub(AIH_cases_QF4_filter$ofr, 12), AIH_cases_QF4_filter$ofr)
AIH_cases_QF4_filter$ofr <- as.numeric(AIH_cases_QF4_filter$ofr)

##Casting overall fibrosis review ofr column into the calls
#AIH_cases_QF4_filter

AIH_cases_QF4_filter$F03_F4 <- ifelse (
                         AIH_cases_QF4_filter$F03vF4 %in% "F0_F3", print ("no"),
                          ifelse (
                            AIH_cases_QF4_filter$F03vF4 %in% "F4", print ("yes"),
                                  ifelse(
                                    AIH_cases_QF4_filter$Fib4Call %in% "F0_F3", print ("no"),
                                         ifelse(
                                           AIH_cases_QF4_filter$Fib4Call %in% "F4", print ("yes"), 
                                            ifelse (AIH_cases_QF4_filter$ofr < 4 & !is.na(AIH_cases_QF4_filter$ofr), print ("no"),
                                                    ifelse(AIH_cases_QF4_filter$ofr == 4, print ("yes"), NA)
                                            )))))

AIH_cases_QF4_filter$F03_F4 <- as.factor(AIH_cases_QF4_filter$F03_F4)
AIH_cases_QF4_calls <- data.frame (aasld_id = AIH_cases_QF4_filter$aasld_id, fib4 = AIH_cases_QF4_filter$fib4, F03_F4 = AIH_cases_QF4_filter$F03_F4)

#Write files for import into main metadata file
#write.csv(AIH_cases_QF4, file = "18_1005_AIH_metadata.csv", row.names = FALSE)
write.csv(AIH_cases_QF4_calls, file = "19_0402_AIH_cases_QF4_calls.csv", row.names = FALSE)



