
##Set working directory
#setwd("~/AIH-Project/AIH metadata/data")
library ("plyr")
library("dplyr")
library("magrittr")
library("ggplot2")
library("tidyverse")

setwd("~/AIH-Project/AIH metadata/data")

#Read in comprehensive metadata file (downloaded from redcap)
AIH_metadata <- read.csv ("./AASLDAutoimmunePilot_DATA_2018-11-30_1600.csv", stringsAsFactors = FALSE)

#Filter for selected columns
AIH_metadata_firstpass <- dplyr::select(AIH_metadata, aasld_id, ind_id, du, spl_plate, dt_isl,  case_hl_du, date_lt, date_coll, sex, race, ethn, aih_type, te_coll, age_coll,  on_tx, igg_coll, ast_coll, alt_coll,  alp_coll, tbili_coll, response, relapse, decomp, lt, liver_death, age_hc, ast_hc, alt_hc, alkphos_hc, tbili_hc, durn_reg, bx_fib_coll, mo_yr_dx, bx_fib_bl, bx_bl_coll, bx_bl_rpt, reg_coll___1, reg_coll___2, reg_coll___3, reg_coll___4, reg_coll___5, reg_coll___6, reg_coll___7, reg_coll___8)
colnames(AIH_metadata_firstpass)
AIH_metadata_firstpass$response

#Data clealrning to remove text from numeric variables [may need re-casting as numeric if used downstream]
AIH_metadata_firstpass[11,32] = 1
AIH_metadata_firstpass[46,31] = 0

#Casting treat duration by months, and then adding a second column to indicated if treatment duration less than 6 months  
AIH_metadata_firstpass$durn_reg <- ifelse (grepl ("week", AIH_metadata_firstpass$durn_reg, ignore.case = TRUE), ((parse_number(AIH_metadata_firstpass$durn_reg))/4), 
                                   ifelse (grepl ("year", AIH_metadata_firstpass$durn_reg, ignore.case = TRUE), ((parse_number(AIH_metadata_firstpass$durn_reg))*12), (parse_number(AIH_metadata_firstpass$durn_reg))
                                 ))

##Turning alt into a factor variable to identify elite controllers (women <=19, men <=30, vs partial control women 19<alt<=60 men 30<alt<=60, vs uncontrolled alt > 60
AIH_metadata_firstpass$alt_elite_v_non <- ifelse ((AIH_metadata_firstpass$case_hl_du == 2), print ("healthy"),
                                          ifelse ((AIH_metadata_firstpass$durn_reg < 6), print ("less_than_6_mos"),
                                          ifelse ((AIH_metadata_firstpass$sex == 1 & AIH_metadata_firstpass$alt_coll <= 30), print ("elite"),
                                          ifelse ((AIH_metadata_firstpass$sex == 1 & AIH_metadata_firstpass$alt_coll > 30 & AIH_metadata_firstpass$alt_coll <= 60), print ("partial"),
                                          ifelse ((AIH_metadata_firstpass$sex == 1 & AIH_metadata_firstpass$alt_coll > 60), print ("uncontrolled"),
                                          ifelse ((AIH_metadata_firstpass$sex == 2 & AIH_metadata_firstpass$alt_coll <= 19), print("elite"),
                                          ifelse ((AIH_metadata_firstpass$sex == 2 & AIH_metadata_firstpass$alt_coll > 19 & AIH_metadata_firstpass$alt_coll <= 60), print ("partial"), print ("uncontrolled"))))))))


#Treatment regimen (Steroid: reg_coll_1 = prednisone,  reg_coll_4 = budesonide,  reg_coll_7: prednisolone / Non-steroid:  reg_coll_2 = azathroprine, reg_coll_3 = 6-MP, reg_coll_5 = MMF, reg_coll_6 = tacrolimus, reg_coll_8 = other)
AIH_metadata_firstpass$no_tx_vs_steroids_vs_NOsteroids <- ifelse ((AIH_metadata_firstpass$case_hl_du == 2 | AIH_metadata_firstpass$on_tx == "no"), print ("off_tx"),
                                                          ifelse ((AIH_metadata_firstpass$reg_coll___1 == 1 | AIH_metadata_firstpass$reg_coll___4 == 1 | AIH_metadata_firstpass$reg_coll___7 == 1), print ("steroids"),
                                                          ifelse ((AIH_metadata_firstpass$reg_coll___2 == 1 | AIH_metadata_firstpass$reg_coll___3 == 1 | AIH_metadata_firstpass$reg_coll___5 == 1 | AIH_metadata_firstpass$reg_coll___6 == 1), print ("NOsteroids"), print (NA)
                                                                  )))
  
  
#Assign complex cases as outlined by Craig the number 4
for (i in 1:nrow(AIH_metadata_firstpass)){
  if (is.na(AIH_metadata_firstpass$case_hl_du[i])){
    AIH_metadata_firstpass$case_hl_du[i] <- 4
  }
}


#Add in data regarding Fib4 and F03_F4
##Requires having run the algorithm data separately (code seperately; uploaded from prior file)

setwd("~/Documents/AIH/AIH_R_Projects/Determine_if_cirrhotic")
AIH_metadata_QF4_join <- read.csv("18_1005_AIH_cases_QF4_calls.csv", stringsAsFactors = FALSE)
AIH_metadata_firstpass <- full_join(AIH_metadata_firstpass, AIH_metadata_QF4_join, "aasld_id")
colnames(AIH_metadata_firstpass)
AIH_metadata_firstpass$F03_F4


#Merge data for duplicate rows into the data frame

for (i in 1:nrow(AIH_metadata_firstpass)){
  
  if (AIH_metadata_firstpass$case_hl_du[i] == 3){
    AIH_metadata_firstpass[i, 5:ncol(AIH_metadata_firstpass)] <-
      AIH_metadata_firstpass[grepl(AIH_metadata_firstpass$du[i], AIH_metadata_firstpass$aasld_id), 5:ncol(AIH_metadata_firstpass)]
  }
}
  

#Merge age, and LFT columns for cases and healthy controls
AIH_metadata_firstpass$age <- ifelse (!is.na(AIH_metadata_firstpass$age_coll), print (AIH_metadata_firstpass$age_coll), print(AIH_metadata_firstpass$age_hc))
AIH_metadata_firstpass$ast <- ifelse (!is.na(AIH_metadata_firstpass$ast_coll), print (AIH_metadata_firstpass$ast_coll), print(AIH_metadata_firstpass$ast_hc))
AIH_metadata_firstpass$alt <- ifelse (!is.na(AIH_metadata_firstpass$alt_coll), print (AIH_metadata_firstpass$alt_coll), print(AIH_metadata_firstpass$alt_hc))
AIH_metadata_firstpass$bili <- ifelse (!is.na(AIH_metadata_firstpass$tbili_coll), print (AIH_metadata_firstpass$tbili_coll), print(AIH_metadata_firstpass$tbili_hc))
AIH_metadata_firstpass$alkp <- ifelse (!is.na(AIH_metadata_firstpass$alp_coll), print (AIH_metadata_firstpass$alp_coll), print(AIH_metadata_firstpass$alkphos_hc))
colnames(AIH_metadata_firstpass)


#Remove duplicate column data from the above 
AIH_metadata_firstpass %<>% select (aasld_id, ind_id, case_hl_du, alt_elite_v_non, no_tx_vs_steroids_vs_NOsteroids, du, spl_plate, dt_isl, date_lt,date_coll, sex, 
                                    race, ethn, aih_type, te_coll, on_tx, igg_coll, response, 
                                    relapse, decomp, lt, liver_death, age, ast, alt, bili, alkp, durn_reg, fib4, F03_F4)
                              

#make du column represent the 'sample' so that we can collapse replicates later
for (i in 1:nrow(AIH_metadata_firstpass)){
  if (AIH_metadata_firstpass$du[i] == ""){
    AIH_metadata_firstpass$du[i] <-AIH_metadata_firstpass$aasld_id[i]
    }
}


#make a new column which states whether liver transplant happened before or after the collection data

for (i in 1:nrow(AIH_metadata_firstpass)){
  if (is.na(AIH_metadata_firstpass$lt[i])){
    AIH_metadata_firstpass$lt_before_coll[i] <- NA
  }
  else if (AIH_metadata_firstpass$lt[i] == 1){
    print(i)
    if (as.Date(AIH_metadata_firstpass$date_coll[i]) > as.Date(AIH_metadata_firstpass$date_lt[i]) ){
      AIH_metadata_firstpass$lt_before_coll[i] <- "yes"
    }
    else {
      AIH_metadata_firstpass$lt_before_coll[i] <- "no"
    }
  }
}

#Note list of duplicate pairs (9/72, 19/11, 29/74, 39/96, 49/27) -- list duplicates of what

#Decoding number inputs so that the spreadsheet easily readible

##Metadata present for both cases and controls 

###case v control v complex (1 - case, 2 - control, 3 - duplicate)
##alternative way to do this re-casting: colData$COPDtxt[colData$COPD=="3"]<-"COPD" -- can try this for tricky "NA" cases in case_hl_du that are complex cases (18/22)
AIH_metadata_firstpass$case_hl_du <- ifelse (AIH_metadata_firstpass$case_hl_du == 1, print ("case"), 
                                             ifelse (AIH_metadata_firstpass$case_hl_du == 2, print ("control"), 
                                                  ifelse (AIH_metadata_firstpass$case_hl_du == 4, print ("complex"), print ("NA"))
                                             ))

###sex (1 - male, 2 - female)
AIH_metadata_firstpass$sex <- ifelse (AIH_metadata_firstpass$sex == 1, print ("male"), print ("female"))

###race (1 - white, 2 - black, 3 - asian)
AIH_metadata_firstpass$race <- ifelse (AIH_metadata_firstpass$race == 1, print ("white"), 
                                       ifelse (AIH_metadata_firstpass$race == 2, print ("black"), print ("asian")))

###ethnicity (1 - hisp/latino, 2 - non hisp latino)
AIH_metadata_firstpass$ethn <- ifelse (AIH_metadata_firstpass$ethn == 1, print ("hisp_latino"), print ("Not_hisp_latino"))
                                  

##Variables present in only for cases

###Type of AIH
AIH_metadata_firstpass$aih_type <- ifelse(AIH_metadata_firstpass$case_hl_du == "case" & AIH_metadata_firstpass$aih_type == 1, print ("type_1_aih"),
  ifelse (AIH_metadata_firstpass$case_hl_du == "case" & AIH_metadata_firstpass$aih_type == 1, print ("type_1_aih"),
    ifelse (AIH_metadata_firstpass$case_hl_du == "case" & AIH_metadata_firstpass$aih_type == 2, print ("type_2_aih"),
        ifelse (AIH_metadata_firstpass$case_hl_du == "case" & AIH_metadata_firstpass$aih_type == 3, print ("drug_induced_aih"),
           ifelse (AIH_metadata_firstpass$case_hl_du == "case" & AIH_metadata_firstpass$aih_type == 4, print ("other_aih"), 
                ifelse (AIH_metadata_firstpass$case_hl_du == "case" & AIH_metadata_firstpass$aih_type == 0, print ("unknown_aih"), 
                   ifelse (AIH_metadata_firstpass$case_hl_du == "control", print ("healthy_cont"), print ("NA")
  )))))))

AIH_metadata_firstpass$on_tx
###on_tx (1 - yes, 0 - no)
AIH_metadata_firstpass$on_tx <- ifelse(AIH_metadata_firstpass$on_tx == 1, print ("yes"), print ("no"))
                                                  

###response (1 - complete, 2 - partial, 3 - non-responder)
AIH_metadata_firstpass$response <- ifelse(AIH_metadata_firstpass$response == 1, print ("complete"), 
                                          ifelse(AIH_metadata_firstpass$response == 2, print ("partial"), print ("non-responder")))

###relapse (0 - no, 1 - yes)
AIH_metadata_firstpass$relapse <- ifelse(AIH_metadata_firstpass$relapse == 1, print ("yes"), print ("no"))

###decomp (0 - no, 1 - yes)
AIH_metadata_firstpass$decomp <- ifelse(AIH_metadata_firstpass$decomp == 1, print ("yes"), print ("no"))

###lt (0 - no, 1 - yes)
AIH_metadata_firstpass$lt <- ifelse(AIH_metadata_firstpass$lt == 1, print ("yes"), print ("no"))

###liver death (na - no, 1 - yes)
AIH_metadata_firstpass$liver_death <- ifelse(AIH_metadata_firstpass$liver_death == 1, print ("yes"), print ("no"))


View(AIH_metadata_firstpass)


###Casting 0 for controls in F03_F4 variable for fibrosis determination 
AIH_metadata_firstpass$F03_F4_final <- ifelse (AIH_metadata_firstpass$case_hl_du == "control", print ("F0"),
                                         ifelse (AIH_metadata_firstpass$F03_F4 == "yes", print ("F4"), print ("F0_F3")
                                         ))
                               

#Adding a column if the sample was positive for pegivirus [Pegivirus 95, 37, 17, 18]
AIH_metadata_firstpass$pegivirus <- ifelse (grepl ("AASLD-095|AASLD-037|AASLD-017|AASLD-018", AIH_metadata_firstpass$aasld_id), print ("yes"), print ("no"))


#Writing to a data file
setwd("~/AIH-Project/AIH metadata/data")
write.csv (AIH_metadata_firstpass, file = "AIH_metadata_firstpass_ALedits.csv", row.names = FALSE)











##################Additional steps as needed: making factors from cont variable from sample colleciton (ast, alt, tbili, alkp, igg)######################

#Filterting tables for Indiana f/u email
elite_control <- AIH_metadata_firstpass %>% filter (alt_elite_v_non == "elite") %>% select (aasld_id, ind_id)
View(elite_control)

off_treatment <- AIH_metadata_firstpass %>% filter (no_tx_vs_steroids_vs_NOsteroids == "off_tx") %>% select (aasld_id, ind_id)
View(off_treatment)

partial_control <- AIH_metadata_firstpass %>% filter (alt_elite_v_non == "partial") %>% select (aasld_id, ind_id)
View(partial_control)



######


AIH_metadata_firstpass_cut <- read.csv ("AIH_metadata_firstpass_ALedits.csv")
AIH_metadata_firstpass_cut$aasld_id <- as.character(AIH_metadata_firstpass_cut$aasld_id)
AIH_metadata_firstpass_cut$response <- as.factor(AIH_metadata_firstpass_cut$response)


#Some summary statistics using dplyr to group and summarize data (mean, sd, iqr to assess break points)
cutpoints_ast <- AIH_metadata_firstpass_cut %>% group_by (case_hl_du) %>% summarize (mean_ast = mean (ast), median_ast = median (ast), iqr_ast = IQR (ast))
cutpoints_alt <- AIH_metadata_firstpass_cut %>% group_by (case_hl_du) %>% summarize (mean_alt = mean (alt), median_alt = median (alt), iqr_alt = IQR (alt))
cutpoints_tbili <- AIH_metadata_firstpass_cut %>% group_by (case_hl_du) %>% summarize (mean_tbili = mean (bili), median_tbili = median (bili), iqr_tbili = IQR (bili))
cutpoints_alkp <- AIH_metadata_firstpass_cut %>% group_by (case_hl_du) %>% summarize (mean_alkp = mean (alkp, na.rm = TRUE), median_alkp = median (alkp, na.rm = TRUE), iqr_alkp = IQR (alkp, na.rm = TRUE))
cutpoints_igg <- AIH_metadata_firstpass_cut %>% group_by (case_hl_du) %>% summarize (mean_igg = mean (igg_coll, na.rm = TRUE), median_igg = median (igg_coll, na.rm = TRUE), iqr_igg = IQR (igg_coll, na.rm = TRUE))


##ast summary stats and plot data; cutting into approx 25 bins (based on stats and #of bars in graph)
cutpoints_ast 

ggplot(AIH_metadata_firstpass, aes (x = ast) )+
  geom_histogram(binwidth = 3) + xlim(0,100) + ylim (0, 40)


AIH_metadata_firstpass_cut$ast <- cut (AIH_metadata_firstpass_cut$ast, 25, labels = 1:25)

#age summary stats cutting into approximately 20 bins (based on the numbers of bars on the graph)
ggplot(AIH_metadata_firstpass, aes (x = age, fill = case_hl_du)) +
  geom_histogram(binwidth = 3) 

AIH_metadata_firstpass_cut$age <- cut (AIH_metadata_firstpass_cut$age, 20, labels = 1:20)


##alt summary stats cutting into approx 30 bins (based on stats and #of bars in graph)
cutpoints_alt 

ggplot(AIH_metadata_firstpass, aes (x = alt, fill = F03_F4_final)) +
  geom_histogram(binwidth = 3) + xlim(0,250) + ylim (0, 40) 

AIH_metadata_firstpass_cut$alt <- cut (AIH_metadata_firstpass_cut$alt, 30, labels = 1:30)

##tbili summary stats cutting into approx 15 bins (based on stats and #of bars in graph)
cutpoints_tbili

ggplot(AIH_metadata_firstpass, aes (x = bili, fill = F03_F4)) +
  geom_histogram(binwidth = 0.25) 

AIH_metadata_firstpass_cut$bili <- cut (AIH_metadata_firstpass_cut$bili, 15, labels = 1:15)


##alkp summary stats cutting into approx 20 bins (based on stats and #of bars in graph)
cutpoints_alkp

ggplot(AIH_metadata_firstpass, aes (x = alkp, fill = F03_F4)) +
  geom_histogram(binwidth = 10) 

AIH_metadata_firstpass_cut$alkp <- cut (AIH_metadata_firstpass_cut$alkp, 20, labels = 1:20)

##igg summary stats cutting into approx 25 bins (based on stats and #of bars in graph)
cutpoints_igg

ggplot(AIH_metadata_firstpass, aes (x = igg_coll, fill = F03_F4)) +
  geom_histogram(binwidth = 100) 

AIH_metadata_firstpass_cut$igg_coll <- cut (AIH_metadata_firstpass_cut$igg_coll, 25, labels = 1:25)

str(AIH_metadata_firstpass_cut)


##Modifying AIH metadata to add in plate numbers (plate 1 vs plate 2), pegivirus, date isolated (dt_isl) and placeholder for water control

#Adding a column to indicate the date of RNA extraction (dt_isl)
AIH_metadata_firstpass_cut$dt_isl <- AIH_metadata$dt_isl

#Casting plate 1 separately 
AIH_metadata_firstpass_cut_1to56 <- AIH_metadata_firstpass_cut[1:55,]

#Adding a row for AASLD-56, the water control
AASLD_056 <- c("AASLD_056", "control", rep("NA", 5))
AIH_metadata_firstpass_cut_1to56 <- rbind (AIH_metadata_firstpass_cut_1to56, AASLD_056)

#Adding plate data as metadata
AIH_metadata_firstpass_cut_1to56$plate <- as.factor ("plate_1")

#Casting plate 2 separately 
AIH_metadata_firstpass_cut_57to111 <- AIH_metadata_firstpass_cut[56:110,]
AIH_metadata_firstpass_cut_57to111$plate <- as.factor ("plate_2")

#Joining data from plates 1 and 2 as well as row 56
AIH_metadata_firstpass_cut <- rbind (AIH_metadata_firstpass_cut_1to56, AIH_metadata_firstpass_cut_57to111)


#Writing AASLD first pass cut
write.csv (AIH_metadata_firstpass_cut, file = "AIH_metadata_firstpass_ALedits2.csv", row.names = FALSE)

