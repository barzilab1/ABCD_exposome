
source("config.R")
source("utility_fun.R")

########### Discrimination ########### 
ydmes01 = load_instrument("abcd_ydmes01",exposome_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)

summary(ydmes01[ydmes01$eventname == "1_year_follow_up_y_arm_1",])

#check collinearity 
library("psych")
matrix_names = colnames(ydmes01[ ,grep("_matrix_", colnames(ydmes01)) ])
ydmes01[,matrix_names] = apply(ydmes01[,matrix_names], 2, function(x) {as.numeric(as.character(x))})
xcor <- polychoric(ydmes01[,matrix_names])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]


ydmes01$dim_y_ss_sum = rowSums(ydmes01[,matrix_names])
ydmes01$dim_y_ss_sum = ifelse(is.na(ydmes01$dim_y_ss_sum) & rowSums(is.na(ydmes01[,matrix_names])) <= 3, 
                              rowSums(ydmes01[,matrix_names], na.rm = T),ydmes01$dim_y_ss_sum )

################### TRAUMA ################### 
#abcd_ptsd01
ptsd01 = load_instrument("abcd_ptsd01",exposome_files_path)

ptsd01_baseline = ptsd01[which(grepl("baseline", ptsd01$eventname)),]
ptsd01_first_year = ptsd01[which(grepl("1_year_follow_up", ptsd01$eventname)),]

#update first year names
colnames(ptsd01_first_year)[c(2,3)] = paste0(colnames(ptsd01_first_year)[c(2,3)], "_1_year")


########### School Risk and Protective Factors ########### 
srpf01 = load_instrument("srpf01",exposome_files_path)

#remove School Disengagement columns 
srpf01 = srpf01[, !(colnames(srpf01) %in% c("school_15_y", "school_17_y"))]

summary(srpf01)


########### Family Environment Scale: Family Conflict Subscale ########### 
fes01 = load_instrument("abcd_fes01",exposome_files_path)


########### Parental Monitoring Survey ########### 
pmq01 = load_instrument("pmq01",exposome_files_path)
summary(pmq01)


########### Youth Neighborhood Safety/Crime ########### 
nsc01 = load_instrument("abcd_nsc01",exposome_files_path)


########### Parent Neighborhood Safety/Crime ########### 
pnsc01 = load_instrument("abcd_pnsc01",exposome_files_path)


########### family history ########### 
fhx.1 = load_instrument("fhxp102",exposome_files_path)
fhx.2 = load_instrument("fhxp201",exposome_files_path)

fh_dataset = merge(fhx.1, fhx.2)
fh_dataset = fh_dataset[, grep("^(src|interview|event|sex)|famhx_1$|famhx_4_p$|^fam_history_([5-9]|1[0-3])_yes_no", colnames(fh_dataset))]

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


########### ABCD Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",exposome_files_path)

summary(yle01[,grepl("_y$",colnames(yle01))])


########### family relationship section ########### 
acspsw03 = load_instrument("acspsw03",exposome_files_path)

acspsw03 = acspsw03[, grepl("^(src|interview|event|sex)|(rel_family_id)$", colnames(acspsw03))]

summary(acspsw03)



########### Peer Experiences Questionnaire ########### 
#2_year_follow_up only 
peq01 = load_instrument("abcd_peq01",exposome_files_path)



########### merge all tables
exposome_set = merge(srpf01,pmq01)
exposome_set = merge(exposome_set,yle01)
exposome_set = merge(exposome_set,fes01)
exposome_set = merge(exposome_set,nsc01)
exposome_set = merge(exposome_set,pnsc01)

#acspsw03 doesnt have data in 1 year followup
#family history wasn't administered in 1 year followup
family_set = merge(fh_dataset, acspsw03)

write.csv(file = "outputs/lancent_4_features.csv",x = exposome_set, row.names = F, na = "")
write.csv(file = "outputs/family_set.csv",x = family_set, row.names = F, na = "")




