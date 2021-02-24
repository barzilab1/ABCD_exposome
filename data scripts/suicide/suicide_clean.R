
source("config.R")
source("utility_fun.R")

ksad_y = load_instrument("abcd_ksad501",psychopathology_files_path)

#555 and 888 will be NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA

ksad_y = droplevels(ksad_y)


##########################################
##calculate suicidality
#if one of the items is 1, the result will be 1
#if one of the items is NA and the rest is 0, the result will be NA

#ideation
ksad_y$SI_current_y = apply(ksad_y[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_y$SI_past_y = apply(ksad_y[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_y$SI_y = (ksad_y$SI_current_y == 1 | ksad_y$SI_past_y == 1)*1


#attempt
ksad_y$SA_current_y = apply(ksad_y[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_y$SA_past_y = apply(ksad_y[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_y$SA_y = (ksad_y$SA_current_y == 1 | ksad_y$SA_past_y == 1)*1

#combine ideation and attempt
ksad_y$suicidality_y = (ksad_y$SI_y == 1 | ksad_y$SA_y == 1)*1
  
#nssi
ksad_y$nssi_current_y = apply(ksad_y[,which(grepl("ksads_23_(945|955)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_y$nssi_past_y = apply(ksad_y[,which(grepl("ksads_23_(956|966)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})


##########################################
### get only the 1 year follow up

suicide_set = ksad_y

#select only the 1 year suicide items 
suicide_set_1_year_follow = suicide_set[suicide_set$eventname == "1_year_follow_up_y_arm_1", which(grepl("ksads_23_|src|inter|event|sex|SI|SA|nssi|sui|symptom|diag", colnames(suicide_set)))]

suicide_set_1_year_follow$suicidality_current_y = (suicide_set_1_year_follow$SI_current_y == 1 | suicide_set_1_year_follow$SA_current_y == 1)*1
suicide_set_1_year_follow$suicidality_past_y = (suicide_set_1_year_follow$SI_past_y == 1 | suicide_set_1_year_follow$SA_past_y == 1)*1

#remove all columns with too many NA
suicide_set_1_year_follow = suicide_set_1_year_follow[,which(colSums(is.na(suicide_set_1_year_follow)) < 0.2*11235)]

#remove general items
suicide_set_1_year_follow = suicide_set_1_year_follow[,!grepl("inter|event|sex",colnames(suicide_set_1_year_follow))]


write.csv(file = "outputs/suicide.csv",x = suicide_set_1_year_follow,row.names=F, na = "")


