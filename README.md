# Extremal Random Forests – Numerical experiments

The goal of this repository is to reproduce the numerical experiments from <a href="#ref1">[1]</a><a id="ref1-back"></a>.

If you use this software in your work, please cite it using the following metadata.

```bibtex
@misc{gnecco2023extremal,
      title={Extremal Random Forests}, 
      author={Nicola Gnecco and Edossa Merga Terefe and Sebastian Engelke},
      year={2023},
      eprint={2201.12865},
      archivePrefix={arXiv},
      primaryClass={stat.ME}
}
```

## Table of Contents
- [Directory Structure](#directory-structure)
- [Dependencies](#dependencies)
- [Clone Repository](#clone-repository)
- [Instructions](#instructions)
- [Contributors](#contributors)
- [License](#license)
- [References](#references)


## Directory Structure

Below is a detailed description of the folder and file structure in the `erf-numerical-results` repository. 

- **Data/**: Contains the US Census datasets used in the experiments from <a href="#ref2">[2]</a><a id="ref2-back"></a>.
- **R/**: R functions and settings for running numerical experiments (e.g., data analysis, visualization, etc.)
- **configs/**: JSON configuration files for setting up and running numerical experiments.
- **figures/**: Generated figures and graphs from the numerical experiments.
- **main/**: Main scripts for reproducing the numerical experiments from the paper.
- **output/**: Intermediate results from the numerical experiments.


## Dependencies

- R: version (4.0.2)

### Installing R Requirements
1. **Ensure R is installed:**
Make sure you have R installed on your system by running in the terminal:
```bash
R --version
```
If you don't have R installed please [download R](https://cran.rstudio.com/) and install it.

2. **Install the requirements:**
Move to the `main` directory and launch the R script [`dependencies.R`](main/dependencies.R) by running in the terminal:
```bash
cd R-code
Rscript --vanilla main/dependencies.R
```

## Clone Repository

The repository works with `git lfs` \[<https://git-lfs.com/>\]. After
you install `git lfs` on your system, you can clone the GitHub directory
by typing the following in your command line.

``` bash
git lfs clone https://github.com/nicolagnecco/erf-numerical-results.git
```

## Instructions
All the code to reproduce results in the article is located in the `main` folder.
Below we provide a detailed breakdown of each file.

> [!NOTE]  
> As an example consider the third row of the table referring to [sec_4_1-experiments.R](main/sec_4_1-experiments.R). The file produces Figure 3 of the paper. Its configuration file [exp_sec_4_1.json](configs/exp_sec_4_1.json)  includes the parameter to run the simulation, including the number of parallel `R` instance (see `n_workers`). It produces intermediate results, which are saved in the [output/](output/) folder, and it produces figures, which are saved in the [figures/](figures/) folder. For this file, you can plot the results without running the simulation by setting `RUN_SIMULATION <- FALSE` inside the file. Alternatively, you can run the simulation _and_ plot the results by setting `RUN_SIMULATION <- TRUE`.
The estimated runtime is approximately 15 mins.

> [!WARNING]  
> The estimated runtime is based on the number of parallel `R` instances set in the corresponding config file (see the parameter `n_workers` there). Running several `R` instances in parallel (e.g., `n_workers > 20`) on a local computer might not be feasible.


### Main Folder Breakdown

| File Name        | Description                                       | Config              |  Intermediate Results?    | Figures? | Only Plot Results? |Estimated Runtime |
|--------------|--------------|--------------|--------------|--------------|--------------|--------------|
| [sec_3-generative_model.R](main/sec_3-generative_model.R)| Produce Figure 1| | no | yes | no | < 5 mins
| [sec_3-cv_lambda.R](main/sec_3-cv_lambda.R)     |  Produce Figure 2 | [exp_sec_3_lambda.json](configs/exp_sec_3_lambda.json) [exp_sec_3_cv_lambda.json](configs/exp_sec_3_cv_lambda.json)       | yes   | yes |yes  | ~ 30 mins     |
|[sec_4_1-experiments.R](main/sec_4_1-experiments.R) | Produce Figure 3 | [exp_sec_4_1.json](configs/exp_sec_4_1.json) | yes | yes | yes | ~ 15 mins
| [sec_4_1-boxplots.R](main/sec_4_1-boxplots.R) | Produce Figure 4 | [exp_sec_4_1_boxplots.json](configs/exp_sec_4_1_boxplots.json) | yes | yes | yes | ~ 30 mins
|[sec_5-wage_analysis.R](main/sec_5-wage_analysis.R) | Run US Census analysis without plots | | yes | no | no | ~ 15 mins
| [sec_5-wage_plots.R](main/sec_5-wage_plots.R) | Plots results for US Census analysis, including Figures 5, 6, 7, and Figures 13, 14, 15, 16 (appendix) | | no | yes |  yes | < 5 mins
| [sec_5-wage.R](main/sec_5-wage.R) | Runs [sec_5-wage_analysis.R](main/sec_5-wage_analysis.R) and then [sec_5-wage_plots.R](main/sec_5-wage_plots.R) | | yes | yes | yes | ~ 15 mins
| [sec_app-similarity_weights.R](main/sec_app-similarity_weights.R) | Produce Figure 8 (appendix) | |no | yes | yes | < 5 mins
|[sec_3-cv_kappa.R](main/sec_3-cv_kappa.R) | Produce Figure 9 (appendix) | [exp_sec_3_kappa.json](configs/exp_sec_3_kappa.json) [exp_sec_3_cv_kappa.json](configs/exp_sec_3_cv_kappa.json) | yes | yes | yes | ~ 30 mins
|[sec_4_2-boxplots.R](main/sec_4_2-boxplots.R) | Produce Figure 10 (appendix) | [exp_sec_4_2_boxplots.json](configs/exp_sec_4_2_boxplots.json) | yes | yes | yes | ~ 90 mins
|[sec_4_2-hill.R](main/sec_4_2-hill.R) | Produce Figure 11 (appendix) | [exp_sec_4_2_hill_vs_erf.json](configs/exp_sec_4_2_hill_vs_erf.json) | yes | yes | yes | ~ 15 mins
| [sec_4_1_bias_var-experiments.R](main/sec_4_1_bias_var-experiments.R)|  Produce Figure 12  (appendix) | [exp_sec_4_1_bias_var.json](configs/exp_sec_4_1_bias_var.json) | yes | yes | yes | ~ 15 mins




## Contributors

- [Nicola Gnecco](https://ngnecco.com)
- [Edossa Merga Terefe](https://www.linkedin.com/in/edossa-merga-55b60573/)
- [Sebastian Engelke](http://www.sengelke.com/)


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.


## References
<a id="ref1"></a><a href="#ref1-back">[1]</a>: Nicola Gnecco, Edossa Merga Terefe, and Sebastian Engelke. 2022. "Extremal Random Forests." arXiv Preprint [https://arxiv.org/abs/2201.12865].

<a id="ref2"></a><a href="#ref2-back">[2]</a>: Joshua D. Angrist, Victor Chernozhukov, and Iván Fernández-Val. 2009. "Replication data for: Quantile Regression under Misspecification, with an Application to the U.S. Wage Structure." Harvard Dataverse, V1. DOI: [10.7910/DVN/JNEOLQ](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JNEOLQ).


