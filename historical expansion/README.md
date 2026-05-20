# Historical Expansion Analysis

Analysis of historical electrification expansion in Rwanda using the Rwanda Energy Access and Quality Improvement Project (R-EAQIP) data.

## Overview

This folder contains scripts analyzing the impacts of historical electrification expansion in Rwanda, focusing on the EARP (Electricity Access and Rural Project) infrastructure program. The analysis evaluates outcomes across multiple indicators including electrification rates, economic productivity, nighttime luminosity, and employment patterns.

## Datasets Used

### Primary Data Sources

- **Spatial Data**: Village, district, and cell boundaries (shapefiles) from Rwanda administrative divisions
- **EARP Infrastructure**: Planned and existing Medium Voltage (MV) lines and Low Voltage (LV) distribution data
- **Electrification Status**: Historical electrification records by village and year
- **Utility Data**: Electricity consumption and usage patterns
- **Nightlight Data**: NOAA and VIIRS satellite nighttime luminosity measurements
- **Business Census**: Establishment and employment data (2011, 2014, 2017, 2020)
- **Sectoral Data**: Industry classification (ISIC codes) and productive use sectors

## Script Organization

### Data Preparation (0.x Scripts)

- **0. Main dataset.R**: Main data pipeline. Loads spatial shapefiles (villages, districts, cells), reads EARP infrastructure layers, processes electrification status, and prepares the core merged dataset for analysis.

- **0.1 Unified FE Dataset.R**: Creates unified fixed effects dataset for econometric analysis. Prepares data for regression models with appropriate structure.

- **0.2 Run new FE regressions.R**: Executes fixed effects regressions on the unified dataset. Runs core econometric specifications.

- **0.5 descriptives and plots.R**: Generates descriptive statistics and visualization plots of key variables and trends.

### Identification & Validity (1.x Scripts)

- **1. first stage.R**: First stage analysis validating infrastructure as treatment variable. Tests whether EARP placement predicts electrification outcomes. Implements identification tests using logit models with cell fixed effects.

### Treatment Variable Construction (2.x Scripts)

- **2.1 EARP.R**: Constructs EARP treatment indicator and controls. Defines treated vs. control villages based on EARP MV/LV presence.

- **2.2 EARP new FE.R**: EARP analysis using new fixed effects specifications with improved model structure.

- **2.3 EARP balance table.R**: Tests balance of pre-treatment covariates between treated and control groups. Validates randomness of EARP placement.

- **2.3 EARP predict.R**: Predicts EARP probability using cell characteristics. Creates predicted treatment variable for robustness checks.

### Electrification Analysis (3.x & 4.x Scripts)

- **3.1 elec12-14.R**: Analyzes electrification outcomes 2012-2014. Event study of electrification around EARP implementation using 2012-2014 period baseline.

- **3.2 elec12-14 new FE.R**: 2012-2014 electrification analysis with updated fixed effects methodology.

- **3.3 elec12-14 balance table.R**: Balance tests for 2012-2014 electrification sample.

- **4.1 elec15-17.R**: Analyzes electrification outcomes 2015-2017. Later follow-up period examining longer-term electrification patterns.

- **4.2 elec15-17 new FE.R**: 2015-2017 electrification analysis with new FE specifications.

- **4.3 elec15-17 balance table.R**: Balance tests for 2015-2017 sample.

- **4.4 elec12_17_es.R**: Event study analysis covering full 2012-2017 period examining dynamic effects of electrification over time.

### Economic Impact Analysis (5.x Scripts)

- **5. productive user regressions.R**: Analyzes impacts on productive use and business activity. Estimates effects on establishment counts, employment, and economic productivity by sector.

### Nightlight Analysis (6.x Scripts)

- **6.1 nightlight analysis.R**: Analyzes NOAA nighttime luminosity data. Examines light intensity as proxy for electrification and economic activity.

- **6.2 nightlight analysis VIIRS.R**: Analysis using VIIRS (higher resolution) satellite nightlight data for robustness checks.

- **6.3 nightlight win5.R**: Nightlight analysis with windowed smoothing (5-pixel window) to reduce noise.

- **6.4 nightlight pixel.R**: Pixel-level nightlight analysis for spatial granularity.

- **6.Usage.R**: Links nightlight intensity to electricity consumption patterns.

### Heterogeneous Effects (7.x, 8.x, 9.x Scripts)

- **7. Event study plot.R**: Generates event study plots visualizing dynamic treatment effects over time.

- **8. above below median.R**: Examines heterogeneous effects by median baseline characteristics (above vs. below median analysis).

- **8.1 above below mean(3.manufacture).R**: Heterogeneous effects by manufacturing sector baseline activity.

- **8.2 above below mean(7.wholesale).R**: Heterogeneous effects by wholesale/trade sector baseline activity.

- **8.3 above below mean(9.accomodation).R**: Heterogeneous effects by accommodation/service sector baseline activity.

- **8.4 above below mean(19.other).R**: Heterogeneous effects by other sectors.

- **9. working place.R**: Analyzes effects on workplace employment and labor market outcomes.

### Specialized Analyses

- **Decomposition.R**: Decomposes total treatment effects into components (e.g., direct EARP vs. network spillovers).

- **EARP new FE archive.R**: Archived version of updated EARP analysis.

- **earp_lv_mv.R**: Separates analysis of Low Voltage vs. Medium Voltage EARP components.

- **elec12-14 new FE archive.R**: Archived 2012-2014 analysis version.

- **elec15-17 new FE archive.R**: Archived 2015-2017 analysis version.

- **ISIC.R**: Industry analysis by ISIC (International Standard Industrial Classification) codes.

- **historical expansion.R**: Core historical expansion analysis pipeline.

- **new electrification.R**: Analysis of newly electrified areas.

- **Nightlight.R**: Master nightlight analysis script.

- **public private.R**: Comparative analysis of public vs. private electrification impacts.

- **Usage analysis.R**: Electricity consumption and usage pattern analysis.

- **balance table plot.R**: Visualization of balance test results.

- **10.data preparation claude.R**: Data preparation script (automated generation note).

- **mdb file.R**: Access and processing of database files.

- **save to mega.R**: Output file management and archiving.

- **junk code.R**: Experimental/testing code (not part of main pipeline).

## Execution Workflow

Recommended execution order:

1. **Data Preparation**: Run scripts 0.x in sequence to prepare merged dataset
2. **Identification**: Run script 1.x to validate treatment variable
3. **Treatment Construction**: Run scripts 2.x to define EARP treatment
4. **Main Analysis**: Run electrification scripts 3.x and 4.x 
5. **Economic Impacts**: Run script 5.x for business outcomes
6. **Robustness Checks**: Run nightlight scripts 6.x
7. **Heterogeneity**: Run scripts 7.x, 8.x, 9.x for subgroup analysis
8. **Specialized**: Run remaining analysis scripts as needed

## Key Variables

- **Treatment**: EARP electrification (MV/LV lines)
- **Outcomes**: 
  - Electrification status (yes/no)
  - Nighttime luminosity (NOAA/VIIRS)
  - Business establishments count
  - Employment levels
  - Electricity consumption
  - Sectoral productivity indicators
- **Units**: Village-level analysis with year dimension

## Notes

- Analysis uses fixed effects methodology with cell and year fixed effects
- Multiple robustness checks via balance tables and predicted treatment variables
- Comprehensive heterogeneous effects analysis by baseline characteristics and sectors
- Satellite data used to validate survey-based electrification measures