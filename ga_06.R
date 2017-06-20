
library(rPkg)
packageLoad(c("dplyr","googlesheets"))

gs_ls()
gsheet = gs_title("GA_benchmark")

benchmarks_gs = gsheet %>% gs_read(ws=1,range = "A6:T214")

countNA(benchmarks)

benchmarks = as.data.frame(benchmarks_gs)

head(benchmarks)

x = lm(p_dem_percent ~ Clinton.Percent, data = benchmarks )

summary(x)

plot(x)
par(mfrow = c(2, 2))


benchmarks[c(6,16,29),]

r1_D_votes = sum(benchmarks$PRIMARY.DEM)
r1_T_votes = sum(benchmarks$PRIMARY.VOTES)
.51-r1_D_votes/r1_T_votes


baseline=benchmarks[c(1,3,14,15)]
names(baseline)
baseline$p_dem_pct = (baseline$PRIMARY.DEM/baseline$PRIMARY.VOTES)
baseline$O_baseline = 1
(baseline$PRIMARY.DEM/baseline$PRIMARY.VOTES)-(baseline$p_dem_percent/100)

head(baseline)

#Calculate votes needed for 50.1%
baseline$r2_baseline_1 = baseline$p_dem_pct + .501-r1_D_votes/r1_T_votes
baseline$r2_baseline_d_votes = baseline$r2_baseline_1 * baseline$PRIMARY.VOTES
sum(baseline$r2_baseline_d_votes)/sum(baseline$PRIMARY.VOTES)

names(benchmarks)

#Calculate Expected Margin
#Expected vote in each precinct = Expected D Votes / expected total votes
#expected total votes = total votes * 1.3 (Gets you to 250k)
  #This gets adjusted as results come in
#Expected D votes
  #Dem r1 percent adjusted to 50.5%, adjusted as results come in

sum(baseline$PRIMARY.VOTES)



baseline$r2_e_t_votes = baseline$PRIMARY.VOTES* 1.3
baseline$r2_e_d_votes = baseline$r2_e_t_votes * (baseline$p_dem_pct + .501-r1_D_votes/r1_T_votes)
baseline$r2_e_d_pct = baseline$r2_e_d_votes/baseline$r2_e_t_votes

head(baseline,5)
names(benchmarks)

baseline$r2_actual_d = benchmarks$GE.OSSOFF
baseline$r2_actual_t = benchmarks$GE.VOTES

baseline$r2_actual_d_pct = baseline$r2_actual_d/baseline$r2_actual_t

baseline$r2_dif = baseline$r2_actual_d_pct - baseline$r2_e_d_pct


weighted.mean(baseline$r2_dif[!is.na(baseline$r2_dif)],baseline$r2_e_t_votes[!is.na(baseline$r2_dif)])

baseline$r2_pred_d_pct =
if(is.na(baseline$r2_actual_d_pct)){
  baseline$r2_e_d_pct
} else {
    baseline$r2_actual_d_pct
  }

baseline$r2_pred_d_pct =
  if(!is.na(baseline$r2_actual_d_pct)){
    baseline$r2_actual_d_pct
  } else {
    baseline$r2_e_d_pct
  }


head(baseline,15)
