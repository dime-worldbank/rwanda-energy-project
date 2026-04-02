================================================================================
README: Nightlight Pixel-Level DID Analysis (6.4 nightlight pixel.R)
Author: Xiaoming Zhang
Last Updated: March 2026
================================================================================

OVERVIEW
--------
This script estimates the causal effect of rural electrification (2015-2017) 
on nighttime light intensity in Rwanda using a Difference-in-Differences (DID) 
design at the pixel level (~500m VIIRS resolution).


DATA INPUTS
-----------
1. expansion_claude.xlsx / expansion_join.xlsx
   - Village-level electrification status and infrastructure indicators
   - Key variables:
     * village_id, sector_id, cell_id, District
     * electrified_year: year of electrification (9999 = never electrified)
     * status: treatment classification (elec15_17, never_elec, etc.)
     * Infrastructure: cell_office, health_center, primary_school, 
       secondary_school, sector_district_office, industry, market, imidugudu

2. gb_viirs_corrected_monthly_start_201401_avg_rad.tif
   - VIIRS corrected monthly nightlight data starting January 2014
   - Each band = one month of radiance data
   - Processed to pixel-level observations via spatial join to villages

3. Village.shp
   - Rwanda village boundary shapefile
   - Used to spatially assign pixels to villages via st_within()


DID DESIGN
----------
Treatment Groups:
  - Treatment: Villages electrified 2015-2017 (elec15_17 = 1)
  - Control: Villages never electrified (never_elec, elec15_17 = 0)

Sample Restrictions:
  - Excludes districts: Ngororero, Nyabihu, Nyamasheke, Rubavu
  - Uses January observations only (_01 suffix) for annual comparisons

Time Periods:
  - Original specification: 2014 (baseline), 2017, 2020
  - Extended specification: 2014-2019 (6 annual observations)
  - Base year: 2014 (coefficients show differential change relative to 2014)

Unit of Analysis:
  - Pixel-level (~500m VIIRS resolution)
  - Pixels spatially joined to villages via st_within()
  - Panel balanced using tidyr::complete()


OUTCOME VARIABLES
-----------------
| Variable     | Description                                      |
|--------------|--------------------------------------------------|
| ntl_0.6      | NTL capped at 0.6 (detects low-level lighting)   |
| ntl_3        | NTL capped at 3                                  |
| ntl_15       | NTL capped at 15                                 |
| ntl_15_drop  | NTL with values >= 15 set to NA (dropped)        |

All outcomes also estimated in log(1+x) transformation.

Rationale for capping:
  - Rural electrification produces low-intensity lighting
  - Capping reduces influence of urban/industrial sources
  - ntl_0.6 most sensitive to household-level electrification


REGRESSION SPECIFICATION
------------------------
Model:
  Y_it = α + Σ_t(β_t × Year_t × elec15_17) + γ_i + δ_ct + ε_it

Where:
  - Y_it: Nightlight intensity for pixel i in year t
  - Year_t × elec15_17: Treatment × post-period interactions
  - γ_i: Village fixed effects
  - δ_ct: Cell × Year fixed effects
  - ε_it: Error term, clustered at sector level

Fixed Effects Specifications:
  1. Baseline (columns 1, 3): Village FE + Cell × Year FE
  2. Full (columns 2, 4): Baseline + Infrastructure × Year FE
     - Includes: cell_office, health_center, primary_school, 
       secondary_school, sector_district_office, industry, 
       market, imidugudu (all interacted with year)

Clustering:
  - Standard errors clustered at sector level


PANEL BALANCING
---------------
The panel is balanced using tidyr::complete() to ensure each pixel 
appears in all years:
  - Missing NTL values filled with 0
  - Time-invariant characteristics filled via fill(.direction = "downup")
  - Verification: count(pixel_id) should show equal n for all pixels


KEY CODE SECTIONS
-----------------
Lines 583-650:  Data construction for 2014-2019 panel
Lines 651-670:  Panel balancing with complete()
Lines 680-760:  did_regression() function for 2014-2019


OUTPUT FILES
------------
LaTeX regression tables saved to: outputs/regressions/
  - elec15_17_ntl_0.6_did_pixel_2014_2019.tex
  - elec15_17_ntl_3_did_pixel_2014_2019.tex
  - elec15_17_ntl_15_did_pixel_2014_2019.tex
  - elec15_17_ntl_15_drop_did_pixel_2014_2019.tex


INTERPRETATION
--------------
Coefficients on (Year_t × elec15_17) represent the differential change 
in NTL for treated vs. control villages relative to the 2014 baseline.

Pre-trends check:
  - 2015 and 2016 coefficients should be close to zero and insignificant
  - These years are pre-treatment or early treatment period

Treatment effects:
  - 2017-2019 coefficients capture post-electrification effects
  - Significant positive coefficients indicate electrification 
    increased nightlight intensity

Example interpretation (from results):
  - elec15_17 × 2019 = 0.0064** (ntl_0.6, cell FE)
  - Treated villages show 0.0064 higher NTL in 2019 vs 2014, 
    relative to never-electrified villages


DEPENDENCIES
------------
R Packages:
  - lfe: Fixed effects regression with clustered SEs (felm)
  - stargazer: LaTeX table output
  - sf: Spatial operations (st_join, st_within)
  - raster: Reading VIIRS TIF files
  - tidyverse: Data manipulation (dplyr, tidyr, stringr)
  - readxl/writexl: Excel I/O


NOTES
-----
1. The script contains two DID specifications:
   - Original (2014, 2017, 2020): Lines ~450-570
   - Extended (2014-2019): Lines ~583-760

2. Infrastructure × Year FE control for differential trends in villages 
   near schools, health centers, markets, etc.

3. Negative raw NTL values are set to 0 (sensor noise correction)

================================================================================
