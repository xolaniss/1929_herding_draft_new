# Makefile to run the analysis for the paper and compile the manuscript

## Recursively look for all files in the current directory and its subdirectories
VPATH = $(shell find . -type d)

## List of inputs
INPUT_TARGETS = artifacts_crisis_fundamental.rds \
artifacts_csad_cssd.rds \
artifacts_descriptives.rds \
artifacts_fama_french.rds \
artifacts_fundamental_herding.rds \
artifacts_general_herding.rds \
artifacts_herding_crisis.rds \
artifacts_party_dummy.rds \
artifacts_presidential_ratings.rds \
artifacts_presidential_terms.rds \
artifacts_returns_data.rds \
artifacts_returns_graphs_equal.rds \
artifacts_returns_graphs_weighted_part_1.rds \
artifacts_returns_graphs_weighted_part_2.rds \
artifacts_volatility.rds  

## Generating the manuscript 
1929_herding_draft.pdf: 1929_herding_draft.qmd $(INPUT_TARGETS)
	quarto render $<
	
## Generating rds inputs to manuscript
artifacts_returns_data.rds: 00_returns.R 49_Industry_Portfolios_Daily_equal.csv \
49_Industry_Portfolios_Daily_weighted.csv
	Rscript $<

artifacts_fama_french.rds: 01_fama_french.R F_F_Research_Data_Factors_daily.csv
	Rscript $<

artifacts_csad_cssd.rds: 02_CSAD_CSSD.R artifacts_returns_data.rds
	Rscript $<

artifacts_descriptives.rds: 03_descriptives.R artifacts_csad_cssd.rds
	Rscript $<

artifacts_general_herding.rds: 04_general_herding_analysis.R artifacts_descriptives.rds
	Rscript $<

artifacts_herding_crisis.rds: 05_crisis_herding_analysis.R artifacts_descriptives.rds
	Rscript $<

artifacts_fundamental_herding.rds: 06_fundemental_herding_analysis.R artifacts_general_herding.rds \
artifacts_fama_french.rds
	Rscript $<

artifacts_crisis_fundamental.rds: 07_fundemental_herding_analysis_crisis.R artifacts_fundamental_herding.rds
	Rscript $<

artifacts_presidential_ratings.rds: 08_PAR.R PAR.xlsx \
PEAR.xlsx
	Rscript $<

artifacts_presidential_terms.rds: 09_presidential_terms.R  
	Rscript $<

artifacts_volatility.rds: 10_volatility.R artifacts_fama_french.rds
	Rscript $<

artifacts_party_dummy.rds: 11_dummy_on_dummy.R artifacts_presidential_terms.rds \
artifacts_general_herding.rds \
artifacts_fundamental_herding.rds \
artifacts_volatility.rds
	Rscript $<





