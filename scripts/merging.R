library(readr)

NA_percentage_to_remove = 0.2

########### demographics ########### 
demographics_baseline <- read_csv("outputs/demographics_baseline.csv")
demographics_longitudinal <- read_csv("outputs/demographics_longitudinal.csv")

demographics_baseline = demographics_baseline[,!(colnames(demographics_baseline) %in% c("interview_date","interview_age","eventname","demo_prim"))]
demographics_longitudinal = demographics_longitudinal[,!(colnames(demographics_longitudinal) %in% c("demo_prim_l"))]

demographics = merge(demographics_baseline,demographics_longitudinal)

#remove exposome items & general items
demographics_final = demographics[,!grepl("demo_fam|demo_yrs|child_time|inter|event",colnames(demographics))]

#remove columns with too many NA
demographics_final = demographics_final[,-which(colSums(is.na(demographics_final)) >= NA_percentage_to_remove*dim(demographics_final)[1])]

# add the family member id
family_id <- read_csv("outputs/family_id.csv")
demographics_final = merge(demographics_final,family_id)

write.csv(file = "outputs/demographics_final.csv",x = demographics_final, row.names=F, na = "")


########### psychopathology ###########
psychopathology_sum_scores <- read_csv("outputs/psychopathology_sum_scores.csv")
psychopathology_sum_scores = psychopathology_sum_scores[psychopathology_sum_scores$eventname == "1_year_follow_up_y_arm_1",]

psychopathology <- read_csv("outputs/psychopathology.csv")
psychopathology = psychopathology[psychopathology$eventname == "1_year_follow_up_y_arm_1",]

externalize_ksad_symptoms_p <- read_csv("outputs/externalize_ksad_symptoms_p.csv")


###merging
psychopathology_final = merge(psychopathology, externalize_ksad_symptoms_p[,!grepl("ksads_[A-z]", colnames(externalize_ksad_symptoms_p))])
psychopathology_final = psychopathology_final[,!grepl("^(inter|event|sex)",colnames(psychopathology_final))]

write.csv(file = "outputs/psychopathology_final.csv",x = psychopathology_final, row.names=F, na = "")


#remove symptoms sums from ksads 
psychopathology_sum_scores_final = merge(psychopathology_sum_scores, externalize_ksad_symptoms_p[,grepl("src_subject_id|inter|event|ksads_[A-z]", colnames(externalize_ksad_symptoms_p))])

#remove columns with too many NA
psychopathology_sum_scores_final = psychopathology_sum_scores_final[,colSums(is.na(psychopathology_sum_scores_final)) < NA_percentage_to_remove*dim(psychopathology_sum_scores_final)[1]]

#remove general items & exposome items
psychopathology_sum_scores_final = psychopathology_sum_scores_final[,!grepl("^(inter|event|ple_)",colnames(psychopathology_sum_scores_final))]

write.csv(file = "outputs/psychopathology_sum_scores_final.csv",x = psychopathology_sum_scores_final, row.names=F, na = "")


########### exposome ###########
exposome_item_level_main <- read_csv("outputs/exposome_item_level_main.csv")
exposome_sum_main <- read_csv("outputs/exposome_sum_main.csv")
geo_data <- read_csv("outputs/geo_data.csv")

exposome_item_level <- read_csv("outputs/exposome_item_level.csv")
exposome_sum <- read_csv("outputs/exposome_sum.csv")


exposome = merge(exposome_sum_main, exposome_item_level_main)
exposome = merge(exposome, geo_data)

#add from psychopathology_sum_scores 
exposome = merge(exposome, psychopathology_sum_scores[,grepl("^(src|ple_)", colnames(psychopathology_sum_scores))])

#add from demographics
exposome = merge(exposome, demographics[,grepl("src|demo_fam|demo_yrs",colnames(demographics))])


#add more items after first analysis: 
exposome = merge(exposome, exposome_sum[,grepl("src|pmq_y_ss_mean",colnames(exposome_sum))])
exposome = merge(exposome, exposome_item_level[,grepl("src|^ple_(.*)_y$",colnames(exposome_item_level))])


#remove columns with too many NA
exposome = exposome[, colSums(is.na(exposome)) < NA_percentage_to_remove*dim(exposome)[1]]

write.csv(file = "outputs/exposome_main_final.csv",x = exposome, row.names=F, na = "")


exposome_item_level = exposome_item_level[, colSums(is.na(exposome_item_level)) < NA_percentage_to_remove*dim(exposome_item_level)[1]]
write.csv(file = "outputs/exposome_item_level_final.csv",x = exposome_item_level, row.names=F, na = "")


exposome_sum = exposome_sum[, colSums(is.na(exposome_sum)) < NA_percentage_to_remove*dim(exposome_sum)[1]]
write.csv(file = "outputs/exposome_sum_final.csv",x = exposome_sum, row.names=F, na = "")






