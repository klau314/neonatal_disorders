
## ******************************************************************************
##
## Purpose: Identify any data that spans multiple age groups or
##          sexes, and split the data into specific age groups and sexes.
## Input:   Input data filepath and measure (either prevalence or incidence)
## Output:  Input data expanded out into rows for constituent ages and sexes,
##          ready for further adjustment
## Author:  Kathryn Lau
## Last Update: 4/19/20
##
## To run: launch through launch_agesex_split.R
##
## ******************************************************************************

expand_test_data <- function(agg.test){
  #' @description Expands the number of rows in the test/aggregated age group dataset to appropriate number of
  #' age and sex groups
  #' @param agg.test data.table. The test data from the output of divide_data
  #' @return A data.table with additional rows. One row in the old test data will lead to 'n' number of rows in new test data
  #' where 'n' is the number of age*sex groups within the test data interval
  test <- copy(agg.test)
  
  test[sex == "Both", sex_id := 3]
  test[sex == "Male", sex_id := 1]
  test[sex == "Female", sex_id := 2]
  setnames(test, c("sex_id"), c("agg_sex_id"))
  
  # assign a unique split.id for every original data point that needs to be split, and 
  # code if each data point represents two sexes or one
  test[, `:=`(split.id = .I,
              n.sex = ifelse(agg_sex_id==3, 2, 1))]
  
  # subset out birth prevalence (age_start and age_end = 0) because it does not need age expansion,
  # it only needs sex expansion
  birth_prev <- test[age_start == 0 & age_end == 0]
  birth_prev[, `:=` (agg_age_start = age_start, 
                     agg_age_end = age_end, 
                     n.age = 1,
                     need_split = 0,
                     age_group_id = 164)]
  test <- test[age_end != 0]
  
  
  #### EXPAND FOR AGE ####
  test[, orig_age_end := age_end]
  test[age_demographer == 0 & age_end >= 1, age_end := age_end - 0.01]
  setnames(test, c("age_start", "age_end"), c("agg_age_start", "agg_age_end"))
  
  age_map <- fread("/mnch/crosswalks/dismod_splits_materials/age_map_v2.csv")
  age_map <- age_map[age_end != 0]
  
  # split the data table into a list, where each data point to be split is now its own data table 
  test <- split(test, by = "split.id")
  
  # then apply this function to each of those data tables in that list
  test <- lapply(test, function(t){
    
    # Find age_map's order where age_start is closest to but less than or equal to t's agg_age_start  
    order_start <- age_map[age_start == max(age_map[age_start <= t$agg_age_start, age_start]), order]
    
    # Find age_map's order where age_end is closest to but greater than t's agg_age_end  
    if (t$agg_age_end > 124) { t$agg_age_end <- 124 }
    order_end <- age_map[age_end_compute == min(age_map[age_end_compute > t$agg_age_end, age_end_compute]), order]
    t[, agg_age_end := orig_age_end]
    
    t <- cbind(t, age_map[order %in% order_start:order_end, .(age_start, age_end, age_group_id)])
    t[, n.age := length(order_start:order_end)]
    
    t[n.age == 1, `:=` (age_start = agg_age_start, age_end = agg_age_end)]
    
    t[length(order_start:order_end) == 1, need_split := 0]
    t[length(order_start:order_end) > 1, need_split := 1]
    
    return(t)
    
  })
  
  test <- rbindlist(test)
  
  
  #### EXPAND FOR SEX ####
  ## Append birth prevalence back on
  test <- rbind(test, birth_prev, fill = TRUE)
  
  test[, sex_split_id := paste0(split.id, "_", age_start)]
  
  sex_specific <- test[n.sex == 1]
  sex_specific[, sex_id := agg_sex_id]
  test <- test[n.sex == 2]
  test[, need_split := 1]
  
  expanded <- rep(test[,sex_split_id], test[,n.sex]) %>% data.table("sex_split_id" = .)
  test <- merge(expanded, test, by="sex_split_id", all=T)
  
  if (nrow(test[agg_sex_id ==3]) > 0) {
    test <- test[agg_sex_id==3, sex_id := 1:.N, by=sex_split_id]
  }
  
  test$sex_id <- as.double(test$sex_id)
  test[is.na(sex_id), sex_id:=agg_sex_id]
  
  test <- rbind(test, sex_specific, use.names = TRUE, fill = TRUE)
  
  # hard-code sex restrictions for Turner (437 - should only be females) and Kleinfelter (438 - should only be males)
  if (bun_id == 437) {
    test[sex_id == 1, cases := 0]
  }
  if (bun_id == 438) {
    test[sex_id == 2, cases := 0]
  }
  
  return(test)
}
####################################################################################

#Run

rm(list=ls())

args <- commandArgs(trailingOnly = TRUE)
bun_id <- args[1]
measure_name <- args[2]
bv_id <- args[3]
type <- args[4]

print(args)

# can set bundle id and measure name directly to troubleshoot
# bun_id <- 435
# measure_name <- "prevalence"
# bv_id <- 25229

os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
  h <- "H:/"
} else {
  j <- "/home/j/"
  h <- paste0("/homes/", Sys.getenv("USER"),"/")
}

library('openxlsx')
library('tidyr')
library('dplyr')
library('data.table')
library('matrixStats')
library('ggplot2')
install.packages('msm', lib = '/homes/klau314/R_packages')
library('msm', lib.loc = '/homes/klau314/R_packages')

#-----------------------------------------------------------------------------------
### constants
save_dir <- "/crosswalks/agesex_split_data/"
outdir <- paste0(save_dir, bun_id, "_", measure_name, "_",Sys.Date(),'/')
outdir <- gsub('-','_',outdir)

if (file.exists(outdir)){
  outdir <-  paste0(save_dir, bun_id, "_", measure_name, "_",Sys.Date(),'_v2/')
  outdir <- gsub('-','_',outdir)
}

dir.create(outdir)

map <- fread("/crosswalks/all_me_bundle.csv")

me_id <- map[bundle_id == bun_id, me_id]

#-----------------------------------------------------------------------------------
#load bundle data
#-----------------------------------------------------------------------------------
#load your data
bun_data <- data.table(read.xlsx(paste0(j,input_dir,bun_id,'_BundleV56_tobesplit.xlsx')))

bun_data <- bun_data[!is.na(mean)]

#aggregate old norway data into new subnat groups
norway_map <- fread(input = paste0('/homes/mnch/norway_subnats.csv'))
setnames(norway_map, 'nor19_loc_id', 'location_id')
bun_data <- merge(bun_data, norway_map, by = 'location_id', all.x = TRUE)
bun_data[!is.na(nor20_loc_id), `:=`(cases = sum(.SD$cases),
                                    sample_size = sum(.SD$sample_size)),
         by=c('nor20_loc_id', 'year_start', 'sex', 'age_start', 'age_end','nid')]
#replace old norway data with new subnat rows
norway_data <- bun_data[!is.na(nor20_loc_id)]
bun_data <- bun_data[is.na(nor20_loc_id)]
norway_data <- norway_data[!duplicated(norway_data[,c('nor20_loc_id', 'year_start', 'sex', 'age_start', 'age_end', 'nid')]),]
bun_data <- rbind(bun_data, norway_data)
bun_data[!is.na(nor20_loc_id), location_id := nor20_loc_id]
bun_data$nor20_loc_id <- NULL


#-----------------------------------------------------------------------------------
# Expand each row into its constituent age and sex groups. Subset off data 
# that is already age and sex-specific, and therefore does not need to be 
# split ("good_data")
#-----------------------------------------------------------------------------------
expanded <- expand_test_data(agg.test = bun_data)

good_data <- expanded[need_split == 0]
expanded <- fsetdiff(expanded, good_data, all = TRUE)

if (nrow(expanded)==0) { 
  print(paste("Bundle",bun_id,"does not contain any",measure_name,"data that needs to be split"))
  stop(paste("Bundle",bun_id,"does not contain any",measure_name,"data that needs to be split"))
}

#-----------------------------------------------------------------------------------
#merge populations and age group ids onto the expanded dataset
#-----------------------------------------------------------------------------------
expanded[,year_id := floor((as.numeric(year_end)+as.numeric(year_start))/2)]

#label each row with the closest estimation year for matching to model results
#round down
expanded[year_id%%5 < 3, est_year_id := year_id - year_id%%5]
#round up
expanded[year_id%%5 >= 3, est_year_id := year_id - year_id%%5 + 5]

expanded[est_year_id < 1990, est_year_id := 1990]
expanded[year_id == 2018 | year_id == 2019, est_year_id := 2019]
expanded[year_id > 2019, est_year_id := year_id]

expanded <- add_pops(expanded)
print("Loaded populations")
print(paste0('Number of rows in dataset where pop is NA: ',nrow(expanded[is.na(population)])))

# logit transform the original data mean and se
expanded$mean_logit <- log(expanded$mean / (1- expanded$mean))
expanded$standard_error_logit <- sapply(1:nrow(expanded), function(i) {
  mean_i <- as.numeric(expanded[i, "mean"])
  se_i <- as.numeric(expanded[i, "standard_error"])
  deltamethod(~log(x1/(1-x1)), mean_i, se_i^2)
})

write.xlsx(expanded, 
           file = paste0(outdir, bun_id,'_',measure_name,'_summary_expanded.xlsx'),
           sheetName = 'extraction',
           showNA = FALSE)