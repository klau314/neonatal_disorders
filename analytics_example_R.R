## ******************************************************************************
##
## Purpose: Diagnostic plots for total EHB and total kernicterus in hemolytic 
##          modeling
## Author:  Kathryn Lau
## Last Update: 8/11/20
##
## ******************************************************************************

rm(list=ls())

source("/libraries/current/r/get_location_metadata.R"  )
source("/libraries/current/r/get_demographics.R"  )

template <- get_demographics("epi")
template_locs <- template[[1]]
template_years <- template[[4]]

out_dir <- '/neonatal/outputs/2020/neonatal_hemolytic/'
past_dir <- '/neonatal/outputs/hemolytic/'

locs <- get_location_metadata(location_set_id = 35, gbd_round_id = 7,
                              decomp_step = 'iterative')

rh <- fread(paste0(out_dir,"01_rh_disease/rh_disease_ehb_summary_stats.csv"))
rh_19 <- fread(paste0(past_dir,"01_rh_disease/01_D_final_birthprev/rh_disease_ehb_summary_stats.csv"))

g6pd <- fread(paste0(out_dir,"02_g6pd/g6pd_ehb_summary_stats.csv"))
g6pd_19 <- fread(paste0(past_dir,"02_g6pd/g6pd_ehb_summary_stats.csv"))

preterm <- fread(paste0(out_dir,"03_preterm/preterm_ehb_summary_stats.csv"))
preterm19 <- fread(paste0(past_dir,"03_preterm/preterm_ehb_summary_stats.csv"))

other <- fread(paste0(out_dir,"04_other/other_ehb_summary_stats.csv"))
other_19 <- fread(paste0(past_dir,"04_other/other_ehb_summary_stats.csv"))



current <- merge(rh, preterm, by = c('location_id', 'year', 'sex'), suffixes = c('_rh', '_pre'), all.y = TRUE)
current2 <- merge(g6pd, other, by = c('location_id', 'year', 'sex'), suffixes = c('_g', '_o'), all.y = TRUE)
current <- merge(current, current2, by = c('location_id', 'year', 'sex'), all.x = TRUE)
current[is.na(mean_rh), mean_rh := 0]
current[is.na(mean_g), mean_rh := 0]
current[is.na(mean_o), mean_rh := 0]

current[, ehb_sum := mean_rh + mean_pre + mean_g + mean_o]


current_19 <- merge(rh_19, preterm19, by = c('location_id', 'year', 'sex'), suffixes = c('_rh', '_pre'), all.y = TRUE)
current2_19 <- merge(g6pd_19, other_19, by = c('location_id', 'year', 'sex'), suffixes = c('_g', '_o'), all.y = TRUE)
current_19 <- merge(current_19, current2_19, by = c('location_id', 'year', 'sex'), all.x = TRUE)
current_19[is.na(mean_rh), mean_rh := 0]
current_19[is.na(mean_g), mean_rh := 0]
current_19[is.na(mean_o), mean_rh := 0]

current_19[, ehb_sum := mean_rh + mean_pre + mean_g + mean_o]


# Create a comparison plot where each etiology is a facet (Rh, preterm, G6PD, other, total)
# One plot per estimation year and per EHB vs kernicterus

source("/libraries/current/r/get_model_results.R"  )
total_ehb <- get_model_results(team = 'epi', id = 19812, round_id = 7, decomp_step = 'iterative', age_group_id = 164)
total_ehb_19 <- get_model_results(team = 'epi', id = 19812, round_id = 6, decomp_step = 'step4', age_group_id = 164)

rh$component <- 'Rh'
rh <- rh[sex %in% c(1,2)]
g6pd$component <- 'G6PD'
preterm$component <- 'Preterm'
other$component <- 'Other'
total_ehb$component <- 'Total'
setnames(total_ehb, c('sex_id','year_id'), c('sex','year'))
stack <- rbind(rh, g6pd, preterm, other, total_ehb, fill = TRUE)

rh_19$component <- 'Rh'
rh_19 <- rh_19[sex %in% c(1,2)]
g6pd_19$component <- 'G6PD'
preterm19$component <- 'Preterm'
other_19$component <- 'Other'
total_ehb_19$component <- 'Total'
setnames(total_ehb_19, c('sex_id','year_id'), c('sex','year'))
stack_19 <- rbind(rh_19, g6pd_19, preterm19, other_19, total_ehb_19, fill = TRUE)

stack_both <- merge(stack, stack_19, by = c('location_id', 'year', 'sex', 'component'), all.x = TRUE)
stack_both$outcome <- 'EHB'


# pull kernicterus results
rh <- fread(paste0(out_dir,"01_rh_disease/rh_disease_kernicterus_summary_stats.csv"))
rh_19 <- fread(paste0(past_dir,"01_rh_disease/01_D_final_birthprev/rh_disease_kernicterus_summary_stats.csv"))

g6pd <- fread(paste0(out_dir,"02_g6pd/g6pd_kernicterus_summary_stats.csv"))
g6pd_19 <- fread(paste0(past_dir,"02_g6pd/g6pd_kernicterus_summary_stats.csv"))

preterm <- fread(paste0(out_dir,"03_preterm/preterm_kernicterus_summary_stats.csv"))
preterm19 <- fread(paste0(past_dir,"03_preterm/preterm_kernicterus_summary_stats.csv"))

other <- fread(paste0(out_dir,"04_other/other_kernicterus_summary_stats.csv"))
other_19 <- fread(paste0(past_dir,"04_other/other_kernicterus_summary_stats.csv"))

new_kern_preterm <- fread(paste0('/neonatal/outputs/gbd2020/neonatal_hemolytic/05_final_kernicterus/all_locs_means_preterm.csv'))
new_kern_preterm$component <- 'Total Kern.'
setnames(new_kern_preterm, c('sex_id','year_id'), c('sex','year'))

kern_19 <- fread('/neonatal/outputs/hemolytic/05_final_kernicterus/final_kernicterus_summary_stats.csv')
kern_19$component <- 'Total Kern.'

rh$component <- 'Rh'
rh <- rh[sex %in% c(1,2)]
g6pd$component <- 'G6PD'
preterm$component <- 'Preterm'
other$component <- 'Other'
new_kern_preterm$component <- 'Total'
stack <- rbind(rh, g6pd, preterm, other, new_kern_preterm, fill = TRUE)

rh_19$component <- 'Rh'
rh_19 <- rh_19[sex %in% c(1,2)]
g6pd_19$component <- 'G6PD'
preterm19$component <- 'Preterm'
other_19$component <- 'Other'
kern_19$component <- 'Total'
stack_19 <- rbind(rh_19, g6pd_19, preterm19, other_19, kern_19, fill = TRUE)

stack_both_kern <- merge(stack, stack_19, by = c('location_id', 'year', 'sex', 'component'), all.x = TRUE)
stack_both_kern$outcome <- 'Kern'


stack_both_outcomes <- rbind(stack_both, stack_both_kern, fill = TRUE)
template_years <- template_years[template_years < 2020]
stack_both_outcomes <- stack_both_outcomes[year %in% template_years]

stack_both_outcomes <- merge(stack_both_outcomes, locs, by = 'location_id')
stack_both_outcomes <- stack_both_outcomes[level > 2]

#map the new Norway subnats to themselves
norway_map <- fread(input = paste0('/ihme/mnch/norway_subnats.csv'))
stack_both_outcomes[location_id %in% norway_map$gbd20_loc_id, mean.y := mean.x]

stack_both_outcomes <- stack_both_outcomes[location_id %in% template_locs]

stack_both_outcomes[, missing := FALSE]
stack_both_outcomes[is.na(mean.y), missing := TRUE]
stack_both_outcomes[is.na(mean.y), mean.y := 0.00001]

out_dir <-'/neonatal/outputs/2020/neonatal_hemolytic/'
pdf(file = paste0(out_dir, "v20_vs_v19_ehb_and_kern_by_etiology.pdf"),
    width = 12, height = 8)

for (year_i in template_years) {
  gg1 <- ggplot() +
    geom_point(data = stack_both_outcomes[year == year_i & outcome == 'EHB'], 
               aes(x = mean.y, y = mean.x, color = region_name, shape = missing,
                   text = paste0(location_name_short, ' (', location_id,') ', year, ' ', sex, ': (',
                                 round(mean.y,5),', ',round(mean.x,5),')')),
               alpha = 0.5) +
    scale_shape_manual(values=c(16,4)) +
    facet_grid(~ component) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.2) +
    labs(title = paste0('Prevalence of EHB due to Each Etiology, Year: ', year_i),
         x = '2019',
         y = '2020') +
    theme_minimal() + theme(legend.position = 'bottom', legend.text = element_text(size = 8), legend.title = element_text(size = 8))
  print(gg1)
}

for (year_i in template_years) {
  gg2 <- ggplot() +
    geom_point(data = stack_both_outcomes[year == year_i & outcome == 'EHB'], 
               aes(x = mean.y, y = mean.x, color = region_name, shape = missing,
                   text = paste0(location_name_short, ' (', location_id,') ', year, ' ', sex, ': (',
                                 round(mean.y,5),', ',round(mean.x,5),')')),
               alpha = 0.5) +
    scale_shape_manual(values=c(16,4)) +
    facet_grid(~ component) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.2) +
    labs(title = paste0('Prevalence of EHB due to Each Etiology (Log Space), Year: ', year_i),
         x = '2019',
         y = '2020') +
    scale_y_log10() +
    scale_x_log10() +
    theme_minimal()+ theme(legend.position = 'bottom', legend.text = element_text(size = 8), legend.title = element_text(size = 8))
  print(gg2)
}

for (year_i in template_years) {
  gg3 <- ggplot() +
    geom_point(data = stack_both_outcomes[year == year_i & outcome == 'Kern'], 
               aes(x = mean.y, y = mean.x, color = region_name, shape = missing,
                   text = paste0(location_name_short, ' (', location_id,') ', year, ' ', sex, ': (',
                                 round(mean.y,5),', ',round(mean.x,5),')')),
               alpha = 0.5) +
    scale_shape_manual(values=c(16,4))+
    facet_grid(~ component) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.2) +
    labs(title = paste0('Prevalence of Kernicterus due to Each Etiology, Year: ', year_i),
         x = '2019',
         y = '2020') +
    theme_minimal()+ theme(legend.position = 'bottom', legend.text = element_text(size = 8), legend.title = element_text(size = 8))
  print(gg3)
}

for (year_i in template_years) {
  gg4 <- ggplot() +
    geom_point(data = stack_both_outcomes[year == year_i & outcome == 'Kern'], 
               aes(x = mean.y, y = mean.x, color = region_name, shape = missing,
                   text = paste0(location_name_short, ' (', location_id,') ', year, ' ', sex, ': (',
                                 round(mean.y,5),', ',round(mean.x,5),')')),
               alpha = 0.5) +
    scale_shape_manual(values=c(16,4))+
    facet_grid(~ component) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.2) +
    labs(title = paste0('Prevalence of Kernicterus due to Each Etiology (Log Space), Year: ', year_i),
         x = '2019',
         y = '2020') +
    scale_y_log10() +
    scale_x_log10() +
    theme_minimal()+ theme(legend.position = 'bottom', legend.text = element_text(size = 8), legend.title = element_text(size = 8))
  print(gg4)
}

dev.off()

ggplotly(p = gg1, tooltip = 'text') 
ggplotly(p = gg2, tooltip = 'text')
ggplotly(p = gg3, tooltip = 'text') 
ggplotly(p = gg4, tooltip = 'text') 