
source("config.R")
source("utility_fun.R")


################### ABCD Prodromal Psychosis Scale ################### 
pps01 = load_instrument("pps01", psychopathology_files_path)

pps01 = pps01[,grepl("src|interview|event|sex|([0-9]_y)$",colnames(pps01))]
summary(pps01)

summary(pps01[pps01$eventname == "baseline_year_1_arm_1",]) 
summary(pps01[pps01$eventname == "1_year_follow_up_y_arm_1",]) 


################### Parent General Behavior Inventory-Mania ###################
pgbi01 = load_instrument("abcd_pgbi01", psychopathology_files_path)
pgbi01 = pgbi01[,-which(colnames(pgbi01) == "gbi_select_language___1")]


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
psychopathology = merge(psychopathology,bpm)
psychopathology = merge(psychopathology,ytbpai)

write.csv(file = "outputs/psychopathology.csv",x = psychopathology, row.names = F, na = "")

