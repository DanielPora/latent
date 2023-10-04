analysis.R 
was used to explore the original data set and understand its structure

eval.R 
will perform the whole experiment, including generating the data and testing the methods on it.
results, data, and even models will be saved in the ./data/ folder in a subdirectory named according to a timestamp ID.
Bayesian model files take a lot of disk space, so they are not uploaded here.

data_gen.R (depreciated)
functions to generate synthetic data with L,R,B,F types seperate
plots for data generation

data_generator.R
used in the main experiment, functions to generate synthetic data with FB and LR type

plots.R
creates plots to use in the thesis

results/bernoulli/results.R 
merges the results for both runs with Benoulli and Gauss families, some plots for results
used to analyze results

