
source("config.R")
source("utility_fun.R")


################### cbcls ################### 
cbcls01 = load_instrument("abcd_cbcls01",psychopathology_files_path)

#get all the t scorse
cbcls_t_score = cbcls01[, grepl("^(src|interview|event|sex)|_t$", colnames(cbcls01))]

summary(cbcls_t_score[cbcls_t_score$eventname == "1_year_follow_up_y_arm_1",]) 

################### Youth Diagnostic Interview for DSM-5 Background Items 5 (lgbt/bullying/drop a class) ################### 
yksad01 = load_instrument("abcd_yksad01",psychopathology_files_path)

yksad01 = yksad01[, !grepl("grade|drop|det", colnames(yksad01))]
summary(yksad01)

#"Decline to answer" will be treated as NA
yksad01[yksad01 == "777"] = NA
yksad01$LGBT = (yksad01$kbi_y_sex_orient == 1 | yksad01$kbi_y_trans_id == 1)*1

yksad01$LGBT = ifelse( (is.na(yksad01$LGBT) & (yksad01$kbi_y_sex_orient %in% c(2,3,4) | yksad01$kbi_y_trans_id %in% c(2,3,4) )), 
                       0, yksad01$LGBT)

#View(yksad01[c("LGBT", "kbi_y_sex_orient" , "kbi_y_trans_id" )])


################### ABCD Prodromal Psychosis Scale ################### 
pps01 = load_instrument("pps01", psychopathology_files_path)

pps01 = pps01[,grepl("src|interview|event|sex|([0-9]_y)$",colnames(pps01))]
summary(pps01)

summary(pps01[pps01$eventname == "baseline_year_1_arm_1",]) 
summary(pps01[pps01$eventname == "1_year_follow_up_y_arm_1",]) 


################### Parent General Behavior Inventory-Mania ###################
pgbi01 = load_instrument("abcd_pgbi01", psychopathology_files_path)

summary(pgbi01[pgbi01$eventname == "baseline_year_1_arm_1",]) 
summary(pgbi01[pgbi01$eventname == "1_year_follow_up_y_arm_1",]) 


################### Youth Brief Problem Monitor ###################
bpm = load_instrument("abcd_bpm01", psychopathology_files_path)

#remove duplicates 
bpm = unique(bpm)
bpm[bpm == "777" | bpm == "999"] = NA


################### Youth NIH Toolbox Positive Affect Items ###################
ytbpai = load_instrument("abcd_ytbpai01", psychopathology_files_path)
ytbpai[ytbpai == "777" | ytbpai == "999"] = NA



psychopathology = merge(pps01,pgbi01)

write.csv(file = "outputs/psychopathology.csv",x = psychopathology, row.names = F, na = "")

