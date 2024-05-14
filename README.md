


### Repository to my bachelors thesis "using mixed effect modeling to assign individuals to latent classes"
### by Daniel Porawski @ Saarland University


#### ./preliminary/
This folder contains all analysis results of the original data to explore its structure

#### ./data_generation/
This folder contains the scripts to generate or plot the scenarios.

#### eval.R
The script will perform the entire experiment, including generating the data and testing the methods on it. The results, data, and even models will be saved in the `./data/` folder in a subdirectory. Since Bayesian model files take up a lot of disk space, one should be carefull when running the whole experiment and using the "file =" kexword in bmr. 

#### ./data/
Contains the evaluated clustering tasks. The data for the thesis is saved in ./data/part1/ , incuding plots of assignments and raw data for all scenario instances.
Additionally, the evaluation for a preliminary experiment with linear mixed models is found in ./data/gaus/

#### ./results/
Script for statistical analysis of results, plotting and a copy of evaluation result dataframe

