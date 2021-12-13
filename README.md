
<!-- README.md is generated from README.Rmd. -->

# <img src="figures/mapi.PNG" align="right" height="185.5"/> Morphological Analysis of (Phyto)Plankton Imagery

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![R
4.1.1](https://img.shields.io/badge/R-4.1.1-red.svg)](https://www.r-project.org/)
[![Bash](https://img.shields.io/badge/Bash-grey.svg)](https://www.gnu.org/software/bash/)

<!-- badges: end -->

This project contains the analysis and graphs used in the article *Size, width, shape regularity, and chains: time series analysis of phytoplankton morphology from imagery in Narragansett Bay* by Sonnet et al. (2022, in revision).

## Data

**Environmental data** compiled from the weekly samples and the YSI sonde of the MERL team at the GSO Pier (<https://web.uri.edu/gso/research/marine-ecosystems-research-laboratory/datasets/>), and the solar radiation from the Kingston station of the USCRN (<https://www1.ncdc.noaa.gov/pub/data/uscrn/products/subhourly01/>) are stored in the file *NBay_hourly.csv*.

The full dataset of the coordinates of the 110,741,171 images in the **morphological space** is too big to be uploaded but a 1-month subset is given as example, *pca_coord_1month.csv.*

The summary of **morphological coordinates** per hour (median, mean, 1st quartile, 3rd quartile) for each PC component is in the file *pca_coord_summary.csv*.

The raw **morphological features** averaged per sampled are in *raw_morpho.csv*.

Weekly counts of general **phytoplankton** categories are stored in *gsoIFCB_date+classnames+counts_weekly.csv*.

Note that the **INLA** code uses a modified version of a function from the `brinla` package (<https://github.com/julianfaraway/brinla>).

## Scripts

| Script          | Function                                      | Outputs                                                                                                                                                                                                 |
|-----------------|-----------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `PCA.Rmd`       | Look at the output of the PCA object          | **Figure S3** (screeplot) and **Figure 5** (variable contributions to principal components)                                                                                                             |
| `TS_morpho.Rmd` | Time series of morphological coordinates      | **Figure 6** (time series of hourly coordinates), **Figure 7** (distribution of hourly coordinates), **Figure S4** (time series of main contributors) and **Figure S5** (zooplankton Dec 2018-Mar 2019) |
| `RDA.Rmd`       | Redundancy Analysis                           | **Figure 8** (weekly environmental influence)                                                                                                                                                           |
| `INLA.Rmd`      | Bayesian Dynamic Linear Regression using INLA | **Figure 9** (hourly dynamic regressor's influence).                                                                                                                                                    |
| `Phyto.Rmd`     | Phytoplankton time series                     | **Figure 10** (percentage per week)                                                                                                                                                                     |
