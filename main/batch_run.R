files2run <- c(
    "configs/exp_sec_4_2_hill_vs_erf.json"
)

for (f in files2run) {
    system(paste0("Rscript main/rscript_sims.R -j ", f))
}
