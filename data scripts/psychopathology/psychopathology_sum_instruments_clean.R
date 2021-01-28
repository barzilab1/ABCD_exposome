
source("config.R")
source("utility_fun.R")

################### Sum Scores Mental Health Youth ################### 
mhy = load_instrument("abcd_mhy02", psychopathology_files_path)

mhy = mhy[,grepl("^(src|interview|event|sex|ple_y|pps_)",colnames(mhy))]

#remove nt (Number Total Questions) and nm (Number Missing Answers)
mhy = mhy[,!grepl("_(nm|nt)$",colnames(mhy))]

summary(mhy[mhy$eventname == "baseline_year_1_arm_1" ,])
summary(mhy[mhy$eventname == "1_year_follow_up_y_arm_1" ,])


################### Sum Scores Mental Health Parent ################### 
mhp02 = load_instrument("abcd_mhp02", psychopathology_files_path)
mhp02 = mhp02[,grepl("^(src|interview|event|sex)|(score)$",colnames(mhp02))]

summary(mhp02[mhp02$eventname == "baseline_year_1_arm_1" ,])
summary(mhp02[mhp02$eventname == "1_year_follow_up_y_arm_1" ,])


################### Youth Summary Scores BPM and POA ################### 
yssbpm01 = load_instrument("abcd_yssbpm01", psychopathology_files_path)
yssbpm01 = yssbpm01[,grepl("^(src|interv|event|sex)|_(r|t|mean|sum)$", colnames(yssbpm01))]


psychopathology_sum_scores = merge(mhy,mhp02)


write.csv(file = "outputs/psychopathology_sum_scores.csv",x = psychopathology_sum_scores, row.names = F, na = "")

