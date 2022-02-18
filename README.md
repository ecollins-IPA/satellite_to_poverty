# Combining satellite imagery and the Poverty Probability Index (PPI) to predict poverty #

This repository contains data and code necessary to replicate the existing analysis of the "Satellite to poverty" project. 

Code was written in R 4.1.2 and Python [TBD]

## Description of folders

- **01 Documents**: This folder contains literature and reports
- **02 Data**: Input and output data stored here
- **03 Data Wrangling**: Contains code for creating PPI databases and satellite imagery databases
- **04 Analysis**: Scripts for incorporating satellite imagery into the standard PPI code.
- **05 Tables**:
- **06 Figures**: Relevant grahs for accuracy analysis
- **07 [TBD]**:
- **08 Results**: Results of the PPI model ("standard" and "standard + satellite" versions)

## Instructions for processing satellite imagery data 

* *nightlights_api* is a simple set of script that computes mean nightlight radiance for a given set of coordinate points. Please check the folder readme for instructions

* *predicting-poverty-replication* is a replication and some refactoring of some of the scripts used in [this](https://github.com/jmather625/predicting-poverty-replication) repo, which is the code used in Jeat et al.(2016). Its the same code but written in a slightly nicer way.


## Instructions for processing PPI data

* *01_Ethiopia_VarSelection* restricts LSMS datasets to the variables that are suitable as PPI candidate indicators.

* *02_Ethiopia_Poverty* replicates national and international poverty lines, based on consumption indicators and definitions by the World Bank.

* *03_Ethiopia_VarManipulation* manipulates the candidate questions and converts them into binary usable indicators. This file does NOT include luminosity scores.

* *03_Ethiopia_VarManipulation_Lum* manipulates the candidate questions and converts them into binary usable indicators. This file does include luminosity scores.
