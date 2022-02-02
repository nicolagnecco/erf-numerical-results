files2run <- c(
  
  "configs/exp_sec_app_sensitivity_tau0.json"
  
)

for (f in files2run) {
  system(paste0("Rscript main/rscript_sims.R -j ", f))
}
