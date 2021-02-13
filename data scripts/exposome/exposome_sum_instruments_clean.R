
source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",exposome_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered)$",colnames(sscey01))] #pr


sscey01_not_main = sscey01[,grepl("^(src|event|inter|sex|dim|pmq|srpf)",colnames(sscey01))]
sscey01_main = sscey01[,grepl("^(src|event|inter|sex|fes|crpbi)",colnames(sscey01))]


########### Sum Scores Culture & Environment Parent ########### 
sscep = load_instrument("abcd_sscep01",exposome_files_path)

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscep = sscep[,!grepl("_(nm|nt|na|answered)$",colnames(sscep))]

sscep_base = sscep[sscep$eventname == "baseline_year_1_arm_1", ]
sscep_base = sscep_base[, grepl("^(src|sex|inter|event|meim|via)", colnames(sscep_base)) ]

sscep_1_year = sscep[sscep$eventname == "1_year_follow_up_y_arm_1", ]
sscep_1_year = sscep_1_year[, colSums(is.na(sscep_1_year)) != dim(sscep_1_year)[1]]

sscep_1_year = sscep_1_year[,!grepl("^psb",colnames(sscep_1_year))]


########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",exposome_files_path)

ssmty = ssmty[, grepl("(src|interview|event|sex)|_(weekend|weekday)$", colnames(ssmty))]


########### Parent Adult Self Report Scores Aseba (ASR) ###########
asrs = load_instrument("abcd_asrs01",exposome_files_path)

asrs = asrs[, !grepl("_(r|total|nm)$", colnames(asrs))]
asrs = asrs[, grepl("^(src|sex|event|intervi)|_(internal|exter|tot|dep|anxdi|anti|somaticpr|avoi|adhd|inaa|hyper|inatt)", colnames(asrs))]
summary(droplevels(asrs))


########### Summary Scores Developmental History ########### 
devhxss = load_instrument("abcd_devhxss01",exposome_files_path)

devhxss[devhxss == 999] = NA
summary(droplevels(devhxss))


########### family history ########### 
fhx.1 = load_instrument("fhxp102",exposome_files_path)
fhx.2 = load_instrument("fhxp201",exposome_files_path)

fh_dataset = merge(fhx.1, fhx.2)
fh_dataset = fh_dataset[, grep("^(src|interview|event|sex)|famhx_4_p$|^fam_history_([5-9]|1[0-3])_yes_no", colnames(fh_dataset))]

fh_dataset[fh_dataset == 7|fh_dataset == 999] = NA
summary(fh_dataset)

###rename variables to meaningful names
colnames(fh_dataset)[7] = "FH_ALCOHOL"
colnames(fh_dataset)[8] = "FH_DRUGS"
colnames(fh_dataset)[9] = "FH_DEPRESSION"
colnames(fh_dataset)[10] = "FH_BIPOLAR"
colnames(fh_dataset)[11] = "FH_PSYCHOSIS"
colnames(fh_dataset)[12] = "FH_EXTERNALIZING"
colnames(fh_dataset)[13] = "FH_NERVES"
colnames(fh_dataset)[14] = "FH_MENTAL_HEALTH_PROFFESSIONAL"
colnames(fh_dataset)[15] = "FH_PSYCHIATRIC_HOSPITALIZATION"
colnames(fh_dataset)[16] = "FH_SUICIDALITY"


########### Longitudinal Summary Scores Sports Activity ########### 
lsssa = load_instrument("abcd_lsssa01",exposome_files_path)
lsssa[lsssa == 999] = NA

#remove empty columns
lsssa = lsssa[,-which(colSums(is.na(lsssa)) == dim(lsssa)[1])]

lsssa = lsssa[,grepl("^(src|sex|inter|event)|_nmonth", colnames(lsssa))]
summary(droplevels(lsssa))


########### Sum Scores Physical Health Parent ########### 
ssphp01 = load_instrument("abcd_ssphp01",physicalhealth_files_path)

ssphp01 = ssphp01[,grepl("^(src|sex|inter|event|cna)", colnames(ssphp01))]
ssphp01 = ssphp01[,!grepl("_(nm|nt)$", colnames(ssphp01))]
summary(droplevels(ssphp01[ssphp01$eventname == "1_year_follow_up_y_arm_1",]))


########### Sum Scores Traumatic Brain Injury ########### 
tbi01 = load_instrument("abcd_tbi01",exposome_files_path)
tbi01 = unique(tbi01)

tbi01 = tbi01[, !grepl("_nm$", colnames(tbi01))]
tbi01 = tbi01[,colSums(is.na(tbi01)) != dim(tbi01)[1]]


summary(droplevels(tbi01))




########### merge all tables
### main
exposome_sum_main = merge(lsssa, sscep_1_year)
exposome_sum_main = merge(exposome_sum_main, ssphp01)
exposome_sum_main = merge(exposome_sum_main, sscey01_main)
exposome_sum_main = exposome_sum_main[exposome_sum_main$eventname == "1_year_follow_up_y_arm_1",]
exposome_sum_main = exposome_sum_main[,!grepl("^(event|inter|sex)", colnames(exposome_sum_main))]

exposome_sum_main_baseline = merge(asrs , sscep_base)
exposome_sum_main_baseline = merge(exposome_sum_main_baseline , tbi01)
exposome_sum_main_baseline = exposome_sum_main_baseline[,!grepl("^(event|inter|sex)", colnames(exposome_sum_main_baseline))]

exposome_sum_main = merge(exposome_sum_main, exposome_sum_main_baseline)
write.csv(file = "outputs/exposome_sum_main.csv",x = exposome_sum_main, row.names = F, na = "")

### not main
exposome_sum = merge(ssmty, sscey01_not_main)
exposome_sum = exposome_sum[exposome_sum$eventname == "1_year_follow_up_y_arm_1",]
exposome_sum = exposome_sum[,!grepl("^(event|inter|sex)", colnames(exposome_sum))]

exposome_sum_baseline = merge(devhxss, fh_dataset)
exposome_sum_baseline = exposome_sum_baseline[,!grepl("^(event|inter|sex)", colnames(exposome_sum_baseline))]

exposome_sum = merge(exposome_sum,exposome_sum_baseline)
write.csv(file = "outputs/exposome_sum.csv",x = exposome_sum, row.names = F, na = "")




