library(psych)
library(readr)
#########START#########

###SET FILE DIRECTORY
  
##Specify the path and read in a list of all the text files provided.
  #update the code to point to the directory and/or subfolder of the correct ABCD txt files
  
  #1. assign the file folder you will use as the wd
  main.dir = 'exposome_2.01/'
  # main.dir = 'psychopathology_2.01/'
  
  data.dir = 'original data/'
  output.dir = 'outputs/'
  input.dir = 'inputs/'
  
  #2. collect all of the txt files in the specified sub-folder of the directory
  input_list = Sys.glob(paths = c(paste0(main.dir,data.dir,"*.txt")))

###READ TABLES INTO MEMORY
  #While reading the files the alias_mapping spreadsheet is used to replace Element Names from nda with the corresponding alias names (alias column in NDA data dictionaries).
  #if you do not want to replace the Element names comment out the first line of code (alia = read) and run the remaining code. 
  
  #alia = read.csv(paste0(script.dir,'/R.code/fix.release.R.code/NDA_DEAP_names_2.0.1.csv') ) #only keep if replacing Element Names
  tables = list()
  for (p in 1:length(input_list)) {
    print(p)
    table_name = gsub('*.txt$|.txt', '', basename(input_list[p])) 
    input = input_list[p]
    print(paste("import: ", input, " [", p, "/",length(input_list), "]", sep=""))
    
    dt <- tryCatch({
      a = read.csv(file = input, sep = '\t',header = TRUE, row.names=NULL, check.names=FALSE, na.string = c("","NA"))
      a = as.data.frame(sapply(a, function(x) gsub("\"", "", x)))
      names(a) = as.list(sapply(names(a), function(x) gsub("\"", "", x)))
      a
    }, error = function(e) {
      print(e)
      return(read.table(file = input, sep = '\t',header = TRUE))
    })
    tables[[table_name]] = dt
  }

###CLEAN UP DATA TABLES
##The first row under the header in each data table is the element description. Lets remove those from our data tables. 
  #element description can be found in ABCD Data Dictionaries.
  for (p in 1:length(tables)) {
    dt = tables[[p]]
    dt = dt[-1,]
    dt = droplevels(dt)
    tables[[p]] = dt
  }


##Check if "visit" was used instead of "eventnames" in tables
  #eventnames tells what timepoint the data was collected
  ## TODO visit was changed to "eventname"
  visit_instead_eventnames = vector()
  print("visit instead of eventname:")
  for (p in names(tables)) {
    dt = tables[[p]]
    if ("visit" %in% names(dt) ){
      print(p) #if nothing prints out then visit is not a name used in data tables 
      visit_instead_eventnames = append(visit_instead_eventnames, p)
      ind = which(names(dt) == "visit")
      names(dt)[ind] = "eventname"
      tables[[p]] = dt
    } 
  }

  
##Drop columns introduced by NDA, they are not required in the instruments. But keep dataset_id (QC purpose) and will remove later
  for (p in names(tables)) {
    dt = tables[[p]]
    dt = dt[,!(names(dt) %in% c(paste0(p,"_id"),"collection_id", "collection_title", "promoted_subjectkey", "subjectkey", "study_cohort_name", "dataset_id"))]
    tables[[p]] = dt
  }
  
##Remove data not from baseline 
  baseline_rows = matrix(ncol = 2)
  colnames(baseline_rows) = c("tabel", "num_rows_remain")
  for (p in names(tables)) {
    dt = tables[[p]]
    dt = dt[which(grepl("baseline", dt$eventname)),]
    dt = droplevels(dt)
    if(nrow(dt) == 0){
      tables[[p]] = NULL
    }else{
      tables[[p]] = dt
    }
    baseline_rows = rbind(baseline_rows, c(p,nrow(dt)))
  }
  write.csv(file=paste0(main.dir,output.dir,"baseline.rows.csv"),na.omit(baseline_rows),row.names=F) 
  
##Remove tables with not enough data
  remove_tables = read_csv(paste0(main.dir,input.dir,"tables to remove.csv"))
  for(i in 1:nrow(remove_tables)){
    colname = remove_tables$`table to remove`[i]
    tables[[colname]] = NULL
  } 

##Check for columns that are all empty; rm them
  emptycolumns = matrix(ncol = 2)
  colnames(emptycolumns) = c("tabel", "columns")
  for (p in names(tables)) {
    dt = tables[[p]]
    columns = names(dt)[sapply(dt, function(x) all((x=="NA")|(is.na(x))))]
    emptycolumns = rbind(emptycolumns, cbind("tabel" = rep(p,length(columns)),columns))
    dt = dt[!sapply(dt, function(x) all((x=="NA")|(is.na(x))))]
    tables[[p]] = dt
  }
  write.csv(file=paste0(main.dir,output.dir,"empty.col.names.csv"),na.omit(emptycolumns),row.names=F) 

  
##check for columns with sd == 0
  sdcolumns = matrix(ncol = 2)
  colnames(sdcolumns) = c("tabel", "columns")
  for (p in names(tables)) {
    dt = tables[[p]]
    a=describe(apply(dt, 2,function(x){ as.numeric(as.character(x))}))
    columns = names(dt)[a$vars[a$range == 0 ]]
    sdcolumns = rbind(sdcolumns, cbind("tabel" = rep(p,length(columns)),columns))
    dt = dt[ ,!sapply(names(dt), function(x) {x %in% columns})]
    # tables[[p]] = dt
  }
  write.csv(file=paste0(main.dir,output.dir,"sd.zero.col.names.csv"),na.omit(sdcolumns),row.names=F) 

##columns with more than 30% NA (don't remove yet. can be sub questions) 
  nacolumns = matrix(ncol = 2)
  colnames(nacolumns) = c("tabel", "columns")
  for (p in names(tables)) {
    dt = tables[[p]]
    na_threshold = 0.3 * dim(dt)[1]
    columns = names(dt)[apply(dt, 2,function(x){ sum(is.na(x)) >= na_threshold })]
    nacolumns = rbind(nacolumns, cbind("tabel" = rep(p,length(columns)),columns))
    dt = dt[ ,!sapply(names(dt), function(x) {x %in% columns})]
    # tables[[p]] = dt
  }
  write.csv(file=paste0(main.dir,output.dir,"too.many.na.col.names.csv"),na.omit(nacolumns),row.names=F)
  
##Remove irrelevant columns
  remove_columns = as.list(read_csv(paste0(main.dir,input.dir,"irrelevant columns to remove.csv")))
  for (p in names(remove_columns)) {
    dt = tables[[p]]
    dt = dt[ ,!sapply(names(dt), function(x) {x %in% na.omit(remove_columns[[p]])})]
    tables[[p]] = dt
  }
  
  
psychopathology_tables = tables  
 ###MERGE INDIVIUDAL TABLES INTO A SINGLE SPREADSHEET

##The following loop performs repeated merging operations between pairs of instruments.
  #match is done with "src_subject_id","eventname"
  t2 = tables
  # rm(tables) #save space
  cnt=0
  while ( length(t2) > 1 ) {
    cnt=cnt+1
    print(paste0("iteration : ",cnt));
    access= seq(1,length(t2)-1,2)
    for (i in access) {
      bm = dim(t2[[i]])
      by.vars=c("src_subject_id","eventname","gender","interview_age", "interview_date") 
      t2[[i]] = merge(t2[[i]], t2[[i+1]], by=by.vars, all=TRUE)
      print(paste("rows before: ", bm[1], dim(t2[[i+1]])[1], " rows after: ",dim(t2[[i]])[1], "indices: ",i,i+1," columns: ",bm[2],"+",dim(t2[[i+1]])[2], "-",length(by.vars)," = ",dim(t2[[i]])[2]))
    }
    # for odd number of instruments add the last spreadsheet back to the list
    if (length(t2) %% 2 != 0) access = append(access,length(t2))
    # reduce the list
    t2 = t2[access]
  }
  nda18 = t2[[1]]
  dim(nda18)  #check number of rows and columns
  names(nda18) #check names 
  
###QC OF MERGED FILE
##Double check duplicated variables (should only see dataset_id as duplicates)
  #each instrument is given a unique dataset_id 
  
  #1. check for duplicate names
  dup.1=colnames(nda18)[grepl(".x",colnames(nda18),fixed=T)]
  dup.2=colnames(nda18)[grepl(".y",colnames(nda18),fixed=T)]
  dup.1
  dup.2

  #4. check updated rows and columns 
  dim(nda18)  
  
  
###SAVE MERGED DATASET
  #you should edit the filename for your dataset
  write.csv(file=paste0(main.dir,output.dir,"2.01_combined.csv"),nda18, row.names = F, na = "")
  saveRDS(nda18, "nda2.0.1_orig.Rds")

  
###TODO: check difff between gender and sex

##TODO: Fix wrong values to be NA

#####pmq01  
# dt$parent_monitor_mean = apply(dt[,c("parent_monitor_q1_y","parent_monitor_q2_y","parent_monitor_q3_y","parent_monitor_q4_y","parent_monitor_q5_y")],1,function(x) {mean(as.numeric(as.character(x)))})




