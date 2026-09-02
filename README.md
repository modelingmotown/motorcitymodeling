# Automated Valuation Model Development for the City of Detroit

This repository contains code and supporting materials for building, validating, and comparing automated valuation models (AVMs) for residential property assessment in Detroit, Michigan.

The project has two closely related goals:

1. **Reproduce and validate existing SPSS-based linear regression models in R**
2. **Evaluate expanded AVM approaches**, including engineered geospatial features, nonlinear regression, and machine-learning methods

The workflow emphasizes reproducibility, interpretable model comparison, and assessment-quality diagnostics routinely used in mass appraisal.

---

## Project Overview

The analysis compares **five model specifications across seven city council districts**, with paired SPSS and R implementations to evaluate whether model results can be reproduced consistently across software environments. Concurrently, **city-wide models** are being tested.

The project also explores broader AVM development workflows, including:

- data cleaning and feature engineering
- linear regression
- backward variable selection
- nonlinear modeling with generalized additive models (GAMs)
- machine-learning approaches
- geospatial accessibility features
- time-adjustment variables
- model-fit diagnostics
- assessment ratio studies
- SPSS-vs-R coefficient concordance
- district-level model comparison and visualization

---

## Repository Goals

### 1. Reproduce SPSS Models in R

A major objective is to determine whether existing SPSS regression workflows can be replicated in R with comparable:

- coefficients
- fitted values
- R
- R²
- adjusted R²
- standard error of estimate (SEE)
- F statistics
- residual degrees of freedom
- ratio-study measures

### 2. Evaluate AVM Performance

Model performance is assessed using both conventional predictive metrics and mass-appraisal diagnostics.

Common model-fit measures include:

- R²
- adjusted R²
- SEE
- RMSE
- MAE
- F statistic

Ratio-study measures include:

- median ratio
- mean ratio
- weighted mean ratio
- coefficient of dispersion (COD)
- price-related differential (PRD)
- price-related bias (PRB)
- percent of predictions within ±10%, ±20%, and ±50% of observed sale price

### 3. Expand Feature Engineering

The project explores structural, temporal, categorical, and spatial predictors such as:

- building and land characteristics
- condition and quality indicators
- age / year-built categories
- garage and amenity features
- Economic Condition Factor (ECF) areas
- time-adjustment variables
- linear distance to amenities
- road-network distance
- estimated driving time
- proximity to parks, hospitals, schools, transit, grocery stores, emergency services, airports, highways, and other location features

---

## Modeling Approaches

### Linear Regression

Traditional multiple linear regression provides the primary benchmark and supports direct comparison with existing SPSS models.

```r
model <- lm(
  ln_price ~ ln_landratio +
    ln_base_bldgsize_ratio +
    condition +
    quality +
    central_air +
    fireplace,
  data = sales
)
```

Backward-selection functions are used where appropriate to approximate existing SPSS model-development procedures while allowing selected predictors to be protected from removal.

### Generalized Additive Models

Generalized additive models are used to investigate nonlinear relationships that may not be captured adequately by a strictly linear specification.

```r
library(mgcv)

gam_model <- gam(
  ln_price ~
    s(building_area, bs = "cs") +
    s(age, bs = "cs") +
    s(distance_to_downtown, bs = "cs") +
    condition +
    quality,
  data = sales,
  method = "REML"
)
```

### Machine Learning

Tree-based methods such as XGBoost and LightGBM can be evaluated alongside traditional regression models.

These methods may improve predictive accuracy by automatically capturing:

- nonlinear relationships
- interactions
- thresholds
- heterogeneous effects

Because predictive accuracy alone is not sufficient for assessment modeling, machine-learning models are also evaluated using ratio-study and equity metrics.

---

## Geospatial Features

Location is an important component of residential property value.

The project includes workflows for calculating:

### Straight-Line Distance

```r
distance <- sf::st_distance(
  parcel_points,
  destination_points
)
```

### Road-Network Distance and Travel Time

A local GraphHopper routing server can be used to calculate network-based accessibility measures without relying on external routing API limits.

Examples include distance or travel time to:

- downtown / CBD
- hospitals
- parks
- schools and universities
- grocery stores
- police stations
- fire stations
- bus and QLINE stops
- airports
- highways
- major Detroit landmarks and amenities

---

## SPSS vs R Validation

For each paired model, the project compares SPSS and R output directly.

### Metric Comparison

Results can be summarized using tables or heatmaps showing whether R is:

- stronger than SPSS
- approximately equivalent
- weaker than SPSS

Metric direction is handled explicitly. For example:

- higher R² is better
- lower RMSE / SEE / MAE is better
- lower COD is generally better
- median ratio and PRD are evaluated relative to a target near 1.00
- PRB is evaluated relative to a target near 0.00

### Coefficient Concordance

A coefficient-concordance plot compares:

- **x-axis:** SPSS unstandardized coefficient
- **y-axis:** R coefficient

A 1:1 reference line indicates perfect agreement.

```r
ggplot(coef_compare, aes(x = spss_coef, y = r_coef)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_point(alpha = 0.6) +
  coord_equal()
```

Because intercepts and other large coefficients can compress smaller coefficients visually, concordance plots may also be split into separate coefficient ranges.

---

## Assessment Ratio Analysis

Assessment-quality diagnostics are calculated from predicted-to-observed ratios.

For a parcel i:

```text
Ratio_i = Predicted_i / Observed_i
```

The project calculates several commonly used mass-appraisal statistics.

### Median Ratio

Measures overall appraisal level.

### COD

Measures the average absolute deviation of individual ratios from the median ratio.

### PRD

Compares the mean ratio to the weighted mean ratio.

### PRB

Measures systematic price-related bias in assessment ratios.

### Within-Band Metrics

The project also reports the share of observations for which:

```r
abs(ratio - 1) <= 0.10
abs(ratio - 1) <= 0.20
abs(ratio - 1) <= 0.50
```

These are useful supplemental measures of prediction concentration, although they should not be interpreted as substitutes for formal ratio-study metrics.

---

## Reproducibility

This project is developed primarily in R.

Recommended setup:

```r
install.packages("renv")
renv::restore()
```

Using an RStudio Project (`.Rproj`) together with the `here` package is recommended so that project-relative paths remain reproducible across systems.

```r
library(here)

sales <- readr::read_csv(
  here("data", "processed", "sales.csv")
)
```

Avoid hard-coded machine-specific paths such as:

```r
"C:/Users/name/Documents/project/data/sales.csv"
```

---

## Key R Packages

Packages used across the project may include:

```text
tidyverse
sf
mgcv
gratia
xgboost
lightgbm
tidymodels
kableExtra
here
renv
```

Additional packages may be required by individual scripts.

---

## GraphHopper

Some spatial features rely on a locally hosted GraphHopper routing server.

A typical workflow is:

1. Download an OpenStreetMap `.osm.pbf` extract
2. Configure a GraphHopper routing profile
3. Import the road network
4. Start the local GraphHopper server
5. Send routing requests from R
6. Store calculated network distances / travel times as AVM predictors

Large GraphHopper graph files and OpenStreetMap extracts should generally **not** be committed to the repository.

---

## Data Availability

This repository is intended to make the **modeling workflow and analytical code** publicly reproducible where possible.

Property-level sales, assessment, parcel, or other administrative datasets used during development may contain restricted, licensed, or otherwise non-public information and therefore may not be distributed with the repository.

---

## Interpretation

This repository is intended for research, reproducibility, and methodological comparison.

AVM performance should not be judged using a single statistic. A model with stronger predictive fit can still exhibit undesirable:

- dispersion
- price-related bias
- geographic bias
- instability
- overfitting

For that reason, model evaluation combines traditional predictive statistics with ratio-study diagnostics and direct examination of model behavior.

---

## Current Project Status

The project is under active development.

Current work includes:

- validating SPSS-to-R model reproduction
- comparing five model specifications across seven districts
- expanding geospatial predictors
- evaluating nonlinear and machine-learning alternatives
- comparing model fit and ratio-study performance
- developing reproducible visualization and reporting functions

---

## Contributions

Issues, suggestions, and methodological discussion are welcome.

For contributions involving model comparisons, please include enough information to reproduce the result, including:

- R version
- package versions
- model specification
- preprocessing steps
- relevant data assumptions

---

## License

GNU General Public License v3.0

---

## Acknowledgments

This project makes use of open-source R packages and, where applicable, OpenStreetMap-derived geographic data and GraphHopper routing tools.

The residential AVM development team is part of the GIS/Data Analysis section of the Office of the Assessor, within the Office of the Chief Financial Officer of the City of Detroit. Please contact Elliot Dean (Elliot.Dean@detroitmi.gov) with any questions or comments.

---

### Disclaimer

This repository is a research and development project. Results should not be interpreted as official property assessments, valuations, or policy determinations unless separately reviewed and adopted by the appropriate authority.
