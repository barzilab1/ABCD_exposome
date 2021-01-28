
source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",exposome_files_path)

sscey01 = sscey01[, grepl("^(src|interview|event|sex|pmq|fes|crpbi|srpf|dim)", colnames(sscey01))]

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|pr)$",colnames(sscey01))]

#rename
sscey01$school_environment_sum = sscey01$srpf_y_ss_ses
sscey01$positive_school_involvement_sum = sscey01$srpf_y_ss_iiss
sscey01$school_protective_factors = as.numeric(as.character(sscey01$positive_school_involvement_sum)) + as.numeric(as.character(sscey01$school_environment_sum))

sscey01$parent_monitor_mean = sscey01$pmq_y_ss_mean

sscey01 = sscey01[, !(colnames(sscey01) %in% c("srpf_y_ss_ses", "srpf_y_ss_iiss", "pmq_y_ss_mean"))]

summary(sscey01)


########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",exposome_files_path)

ssmty = ssmty[, grepl("(src|interview|event|sex)|(stq_y_ss_weekend)$", colnames(ssmty))]

summary(ssmty)


########### merge all tables
exposome_set = merge(ssmty,sscey01)

# write.csv(file = "outputs/lancent_4_features.csv",x = exposome_set, row.names = F, na = "")




