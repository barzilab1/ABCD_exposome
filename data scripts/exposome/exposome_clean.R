
source("config.R")
source("utility_fun.R")

########### Discrimination ########### 
ydmes01 = load_instrument("abcd_ydmes01",exposome_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)

summary(ydmes01[ydmes01$eventname == "1_year_follow_up_y_arm_1",])



################### TRAUMA ################### 
#abcd_ptsd01
ptsd01 = load_instrument("abcd_ptsd01",exposome_files_path)

ptsd01_baseline = ptsd01[which(grepl("baseline", ptsd01$eventname)),]
ptsd01_first_year = ptsd01[which(grepl("1_year_follow_up", ptsd01$eventname)),]

#update first year names
# colnames(ptsd01_first_year)[c(2,3)] = paste0(colnames(ptsd01_first_year)[c(2,3)], "_1_year")


########### School Risk and Protective Factors ########### 
srpf01 = load_instrument("srpf01",exposome_files_path)
summary(srpf01)


########### Youth Family Environment Scale: Family Conflict Subscale ########### 
fes01 = load_instrument("abcd_fes01",exposome_files_path)


########### Parent Family Environment Scale: Family Conflict Subscale ########### 
fes02 = load_instrument("fes02",exposome_files_path)
fes02 = unique(fes02)

fes02 = fes02[, !(colnames(fes02) %in% c("fam_enviro_select_language___1"))]


########### Parental Monitoring Survey ########### 
pmq01 = load_instrument("pmq01",exposome_files_path)
summary(pmq01)


########### Youth Neighborhood Safety/Crime ########### 
nsc01 = load_instrument("abcd_nsc01",exposome_files_path)


########### Parent Neighborhood Safety/Crime ########### 
pnsc01 = load_instrument("abcd_pnsc01",exposome_files_path)
pnsc01 = pnsc01[, !(colnames(pnsc01) %in% c("nei_p_select_language___1"))]


########### Parent Family History Summary Scores ########### 
fhxssp = load_instrument("abcd_fhxssp01",exposome_files_path)

fhxssp_main = fhxssp[,!grepl("momdad|parent", colnames(fhxssp))]
fhxssp_item = fhxssp[,grepl("^(src|sex|inter|event)|momdad|parent", colnames(fhxssp))]

summary(fhxssp_item)

########### Youth Life Events ###########
yle01 = load_instrument("abcd_yle01",exposome_files_path)

summary(droplevels(yle01[yle01$eventname == "1_year_follow_up_y_arm_1",]))


########### Parent Life Events ########### 
ple = load_instrument("abcd_ple01",exposome_files_path)
ple = ple[, !(colnames(ple) %in% c("ple_p_select_language___1"))]

summary(droplevels(ple))


########### family relationship section ########### 
acspsw03 = load_instrument("acspsw03",exposome_files_path)

summary(acspsw03)


########### Parent Community Risk and Protective Factors ########### 
crpf = load_instrument("abcd_crpf01",exposome_files_path)

crpf[crpf == 999] = NA
crpf = crpf[, !(colnames(crpf) %in% c("su_select_language___1"))]

summary(droplevels(crpf))



########### Developmental History ########### 
dhx01 = load_instrument("dhx01",exposome_files_path)

dhx01[dhx01 == 999 | dhx01 == -1] = NA
dhx01 = dhx01[, !(colnames(dhx01) %in% c("accult_select_language", "devhx_1_p"))]

summary(droplevels(dhx01))


########### Longitudinal Parent Sports and Activities Involvement Questionnaire ########### 
lpsaiq = load_instrument("abcd_lpsaiq01",exposome_files_path)

lpsaiq[lpsaiq == 999] = NA
lpsaiq = lpsaiq[, !(colnames(lpsaiq) %in% c("sai_l_p_select_language___1"))]

summary(droplevels(lpsaiq))


########### Parent Multi-Group Ethnic Identity-Revised Survey ########### 
meim = load_instrument("abcd_meim01",exposome_files_path)

#remove empty columns
meim = meim[,-which(colSums(is.na(meim)) == dim(meim)[1])]
summary(droplevels(meim))


########### Youth Screen Time Survey ########### 
stq = load_instrument("abcd_stq01",exposome_files_path)

summary(stq)


########### Youth Screen Time Survey ########### 
crpbi = load_instrument("crpbi01",exposome_files_path)
crpbi = crpbi[, !(colnames(crpbi) %in% c("timept"))]

summary(crpbi[crpbi$eventname == "1_year_follow_up_y_arm_1",])


########### Parent Mexican American Cultural Values Scale Modified ########### 
macv = load_instrument("macv01",exposome_files_path)
macv = macv[, !(colnames(macv) %in% c("mex_american_select_lang_1"))]


########### Parent Acculturation Survey ########### 
pacc = load_instrument("pacc01",exposome_files_path)
pacc = pacc[, !(colnames(pacc) %in% c("accult_select_language___1"))]

#remove empty columns
pacc = pacc[, colSums(is.na(pacc)) != dim(pacc)[1]]

pacc[pacc == 777 | pacc == 999] = NA


########### Parent Adult Self Report Raw Scores Aseba ########### 
pasr = load_instrument("pasr01",exposome_files_path)
pasr = pasr[, !(colnames(pasr) %in% c("asr_select_language___1"))]


########### Parental Rules on Substance Use ########### 
prq = load_instrument("prq01",exposome_files_path)
prq = prq[, !(colnames(prq) %in% c("pr_select_language___1"))]


########### Youth Acculturation Survey Modified from PhenX (ACC) ########### 
yacc = load_instrument("yacc01",exposome_files_path)
yacc[yacc == 777] = NA


########### Child Nutrition Assessment ########### 
cna = load_instrument("abcd_cna01",exposome_files_path)
cna[cna == 999] = NA


########### Parent Ohio State Traumatic Brain Injury Screen ###########
otbi = load_instrument("abcd_otbi01",exposome_files_path)
otbi = otbi[, !(colnames(otbi) %in% c("tbi_select_language___1"))]

#remove empty columns
otbi = otbi[, colSums(is.na(otbi)) != dim(otbi)[1]]


########### Parent Vancouver Index of Acculturation-Short Survey (VIA) ########### 
# via = load_instrument("abcd_via01",exposome_files_path)


########### merge all tables
### main
exposome_item_level_main = merge(crpf, nsc01)
exposome_item_level_main = merge(exposome_item_level_main, stq)
exposome_item_level_main = merge(exposome_item_level_main, pmq01)
exposome_item_level_main = merge(exposome_item_level_main, prq)
exposome_item_level_main = merge(exposome_item_level_main, ydmes01)
exposome_item_level_main = merge(exposome_item_level_main, srpf01)
exposome_item_level_main = merge(exposome_item_level_main, pacc, all.x = T) #11234
exposome_item_level_main = merge(exposome_item_level_main, yacc,  all.x = T) #10326
exposome_item_level_main = exposome_item_level_main[exposome_item_level_main$eventname == "1_year_follow_up_y_arm_1",]
exposome_item_level_main = exposome_item_level_main[,!grepl("^(event|inter|sex)", colnames(exposome_item_level_main))]

exposome_item_level_main_baseline = merge(fhxssp_main, dhx01)
exposome_item_level_main_baseline = exposome_item_level_main_baseline[,!grepl("^(event|inter|sex)", colnames(exposome_item_level_main_baseline))]

exposome_item_level_main = merge(exposome_item_level_main, exposome_item_level_main_baseline)
write.csv(file = "outputs/exposome_item_level_main.csv",x = exposome_item_level_main, row.names = F, na = "")


### not main
exposome_item_level = merge(fes01, lpsaiq)
exposome_item_level = merge(exposome_item_level, ple)
exposome_item_level = merge(exposome_item_level, pnsc01)
exposome_item_level = merge(exposome_item_level, yle01)
exposome_item_level = merge(exposome_item_level, crpbi)
exposome_item_level = merge(exposome_item_level, fes02)
exposome_item_level = merge(exposome_item_level, macv)
exposome_item_level = merge(exposome_item_level, cna)
exposome_item_level = exposome_item_level[exposome_item_level$eventname == "1_year_follow_up_y_arm_1",]
exposome_item_level = exposome_item_level[,!grepl("^(event|inter|sex)", colnames(exposome_item_level))]

exposome_item_level_baseline = merge(fhxssp_item, meim)
exposome_item_level_baseline = merge(exposome_item_level_baseline, pasr)
exposome_item_level_baseline = merge(exposome_item_level_baseline, otbi)
exposome_item_level_baseline = exposome_item_level_baseline[,!grepl("^(event|inter|sex)", colnames(exposome_item_level_baseline))]

exposome_item_level = merge(exposome_item_level,exposome_item_level_baseline)
write.csv(file = "outputs/exposome_item_level.csv",x = exposome_item_level, row.names = F, na = "")


