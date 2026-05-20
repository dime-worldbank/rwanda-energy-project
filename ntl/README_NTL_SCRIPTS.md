# NTL (Nighttime Lights) Analysis Scripts - README

## Project Overview
This folder contains R scripts for processing and analyzing **Nighttime Lights (NTL) data** from satellite imagery to measure electrification patterns and economic activity in Rwanda. These scripts extract, visualize, and link nightlight data to household survey locations and infrastructure networks.

---

## Dataset Summary

### Primary Satellite Data Sources

#### 1. **VIIRS Nighttime Lights Data**
- **Source**: NASA VIIRS (Visible Infrared Imaging Radiometer Suite)
- **File**: `gb_viirs_corrected_monthly_start_201401_avg_rad.tif`
- **Coverage**: January 2014 - February 2023 (114 months)
- **Format**: Monthly raster layers (GeoTIFF)
- **Resolution**: Pixel-level nighttime radiance values
- **Content**: Monthly average radiance measurements of nighttime light emissions
- **Location**: `Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/data/`

#### 2. **LRCC-DVNL (Harmonized Nightlights) Data**
- **Source**: Long-Range Corrected NOAA (LRCC) harmonized nightlights
- **File**: `LACC_2022.tif`
- **Coverage**: 2010-2022 (yearly)
- **Format**: Annual raster GeoTIFF files
- **Location**: `Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/data/LRCC-DVNL data/rwa_cropped/`

### Spatial Reference Data

1. **Rwanda Administrative Boundaries**
   - Village shapefile: `rwa_villages/Village.shp` - Contains ~14,000 villages with Village_ID
   - District shapefile: `rwa_district/District.shp` - 30 districts
   - Cell shapefile: `rwa_cell/Cell.shp` - Smallest administrative unit
   - Country boundary: `rwa_boundary/RWA_adm0.shp`

2. **Electricity Infrastructure (2011 & 2022)**
   - HV Lines: `Existing_HVLine.shp` - High voltage transmission lines
   - MV Lines: `Existing_MVLine.shp` - Medium voltage distribution lines
   - Transformers: `Existing_Transformer.shp` - Transformer locations

3. **Electrification Status Data**
   - File: `expansion_join.xlsx`
   - Content: Village-level electrification year and status categories
   - Location: `Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs/`

### Household Survey Data Integration
- RCT baseline household and village data from HFC surveys
- Location: `Rwanda Energy/EAQIP/datawork/RCT_data/baseline/data/data/`

---

## Script Descriptions

### 1. **LRCC-DVNL.R** - Historical Nightlights Processing (2010-2022)
**Purpose**: Process long-range corrected harmonized nightlights data and aggregate to village level

**Key Functions**:
- Loads LRCC annual nightlight rasters (2010-2022)
- Crops and masks rasters to Rwanda district boundaries
- Aggregates pixel-level data to village level using spatial joins
- Creates animated GIF showing nightlight evolution 2010-2022
- Generates village-level nightlight summary statistics

**Data Processing Steps**:
1. **Load rasters**: Read 13 annual LRCC TIF files (2010-2022)
2. **Spatial transformation**: Reproject to village shapefile CRS
3. **Masking**: Crop to Rwanda boundary, mask to village extents
4. **Aggregation**: Sum nightlight values per pixel within each village
5. **Visualization**: Create individual maps and animated progression

**Key Variable Creation**:
- `ntl_2010` through `ntl_2022` - Annual village-level nightlight sums
- Value transformation: value_adj = value^(1/4) (fourth-root transformation for visualization)

**Outputs**:
- `village_ntl(2010-2022).xlsx` - Village-level annual nightlight summaries
- PNG files: Single-year nightlight maps (one per year)
- `rwanda_nightlights_2010_2022.gif` - Animated timeline of nightlight evolution
- Individual district visualizations

---

### 2. **viirs.R** - VIIRS Monthly-to-Yearly Processing (2014-2023)
**Purpose**: Extract and aggregate VIIRS monthly nightlight data to monthly and yearly village-level summaries

**Key Functions**:
- Loads 114-layer monthly VIIRS raster (Jan 2014 - Feb 2023)
- Extracts mean nightlight values per village using `exact_extract()`
- Aggregates monthly data to annual summaries
- Creates electrification status maps by district
- Visualizes nightlight patterns overlaid with electrification status
- Compares nightlights with village electrification timing

**Data Processing Steps**:
1. **Layer extraction**: Loop through 114 monthly VIIRS layers
2. **Village aggregation**: Extract mean radiance per village using exactextractr
3. **Monthly dataframe**: Create 114-column table (one per month)
4. **Yearly aggregation**: Calculate mean of 12 monthly values per year
5. **Visualization**: Create maps showing electrification status transitions

**Variables Created**:
- Monthly columns: `2014-01` to `2023-02` (114 columns)
- Yearly columns: `2014` to `2023` (10 columns)
- `value_adj` = (value)^(1/4) for visualization (capped at 5)
- Electrification status categories:
  - `always_elec` - Electrified by 2011
  - `elec12_14` - Electrified 2012-2014
  - `elec15_17` - Electrified 2015-2017
  - `elec18_20` - Electrified 2018-2020
  - `never_elec` - Not electrified through 2022

**Outputs**:
- `viirs_2014-2023(monthly&yearly).xlsx` - Monthly + yearly village data
- `viirs_2014-2023(yearly).xlsx` - Yearly only (10 columns)
- District-level electrification status maps (PNG):
  - `Karongi_Electrification_Status.png`
  - `Rutsiro_Electrification_Status.png`
  - `Rulindo_Electrification_Status.png`
  - `Rusizi_Electrification_Status.png`
- Nightlight 2014 distribution maps by district

---

### 3. **viirs_win_pixel.R** - Pixel-Level Winsorized VIIRS (2014-2023)
**Purpose**: Extract VIIRS data with pixel-level winsorization (cap at 5) before aggregating to villages

**Key Differences from viirs.R**:
- Applies winsorization **at pixel level** (before aggregation)
- Values < 0 → 0
- Values > 5 → 5
- Extracts mean **after** winsorization (more conservative estimate)

**Data Processing Steps**:
1. **Load monthly VIIRS layer**
2. **Pixel-level winsorization**: Cap values at [0, 5] range
3. **Village aggregation**: Extract mean of winsorized pixels
4. **Yearly aggregation**: Mean of 12 monthly winsorized means

**Why Winsorize?**
- Prevents extreme outlier pixels from inflating village-level estimates
- Produces more stable, comparable time-series estimates
- Better for statistical inference and modeling

**Outputs**:
- `viirs_2014-2023(monthly&yearly)_5.xlsx` - Monthly + yearly (winsorized)
- `viirs_2014-2023(yearly)_5.xlsx` - Yearly only (winsorized)

**Key Variables**:
- Same structure as viirs.R but with pixel values capped at 5 before aggregation

---

### 4. **ntl_presentation.R** - Infrastructure Overlay Visualization
**Purpose**: Create presentation-quality maps overlaying nightlights with electrical infrastructure networks

**Key Functions**:
- Loads 2014 VIIRS nightlight data
- Overlays HV/MV electrical lines and transformer locations
- Tests multiple radiance thresholds for visualization
- Creates comparison maps at different intensity thresholds
- Generates high-resolution outputs for presentations/reports

**Visualization Strategy**:
- Tests thresholds: 0.01, 0.02, 0.03, 0.05, 0.1, 0.3, 0.5, 1, 2, 7
- Binary classification per threshold (radiance ≥ threshold = 1, else = 0)
- Three-layer overlay:
  1. Nightlight raster (black/yellow gradient)
  2. HV lines (red) - High voltage transmission
  3. MV lines (blue) + Transformers (green) - Distribution network

**Outputs**:
- Multiple PNG files (one per threshold)
- Filename format: `electrification_nightlight_threshold_[value].png`
- High DPI (300) for publication quality

**Use Cases**:
- Stakeholder presentations
- Academic publications
- Policy briefings
- Demonstrating relationship between infrastructure and nighttime economic activity

---

## Workflow & Data Flow

```
VIIRS Raster Data (114 months)
  ↓
[viirs.R & viirs_win_pixel.R]
  ├→ Extract mean radiance per village
  ├→ Monthly table (114 columns)
  └→ Yearly aggregation
       ↓
   [Village-level summaries]
   ├→ viirs_2014-2023(yearly).xlsx
   └→ viirs_2014-2023(yearly)_5.xlsx (winsorized)

LRCC-DVNL Rasters (13 years: 2010-2022)
  ↓
[LRCC-DVNL.R]
  ├→ Aggregate to village level
  ├→ Sum pixel values per village
  └→ Create time series visualization
       ↓
   [Animated evolution]
   ├→ village_ntl(2010-2022).xlsx
   └→ rwanda_nightlights_2010_2022.gif

Village + Infrastructure Data
  ↓
[ntl_presentation.R]
  ├→ Overlay nightlights with electrical networks
  └→ Multi-threshold visualization
       ↓
   [Publication-ready maps]
   └→ Electrification_Status_*.png
```

---

## Key Datasets Output

| Output File | Source Script | Format | Content | Years |
|---|---|---|---|---|
| `village_ntl(2010-2022).xlsx` | LRCC-DVNL.R | Excel | Village-level harmonized nightlights | 2010-2022 |
| `viirs_2014-2023(yearly).xlsx` | viirs.R | Excel | Village-level VIIRS yearly means | 2014-2023 |
| `viirs_2014-2023(yearly)_5.xlsx` | viirs_win_pixel.R | Excel | Village-level VIIRS (winsorized) | 2014-2023 |
| `rwanda_nightlights_2010_2022.gif` | LRCC-DVNL.R | GIF | Animated nightlight evolution | 2010-2022 |
| `*_Electrification_Status.png` | viirs.R | PNG | District maps with elec. status | 2014-2023 |
| `*_Nightlight_2014.png` | viirs.R | PNG | District nightlight distribution | 2014 |

---

## Technical Notes

### Software & Libraries
- **Language**: R (version 4.x+)
- **Key packages**:
  - `raster` - Raster data manipulation
  - `sf` - Spatial feature (vector) operations
  - `exactextractr` - Fast spatial aggregation
  - `dplyr` - Data wrangling
  - `ggplot2` - Visualization
  - `gganimate` + `gifski` - Animated visualizations
  - `janitor` - Data cleaning (clean_names)
  - `writexl` - Export to Excel
  - `magick` - Image/GIF manipulation

### Data Transformations
1. **Fourth-root transformation**: value_adj = value^(1/4)
   - Applied for visualization to compress extreme values
   - Improves visual contrast in maps

2. **Winsorization**: Cap values at specified thresholds (typically 5)
   - Prevents outlier pixels from skewing village aggregates
   - Two approaches:
     - Pixel-level: Applied before aggregation (viirs_win_pixel.R)
     - No winsorization: Raw sum (LRCC-DVNL.R uses sum)

3. **Missing value handling**: 
   - `na.rm = TRUE` in all aggregations
   - Handles no-data pixels and water areas

### Spatial Reference System
- **CRS**: Mix of WGS84 (EPSG:4326) and raster CRS
- **Transformation**: Scripts automatically reproject all layers to common CRS

---

## Running the Scripts

### Execution Steps
1. **First run LRCC-DVNL.R** (2010-2022 historical baseline)
2. **Then run viirs.R** (2014-2023 detailed monthly analysis)
3. **Optionally run viirs_win_pixel.R** (alternative winsorized version)
4. **Finally run ntl_presentation.R** (create presentation visualizations)

### System Requirements
- **Memory**: Recommend 16GB+ RAM for raster operations
- **Storage**: ~2GB for input data, ~500MB for outputs
- **Time**: 
  - LRCC-DVNL.R: ~5-10 minutes
  - viirs.R: ~10-15 minutes
  - viirs_win_pixel.R: ~10-15 minutes
  - ntl_presentation.R: ~5 minutes

### Customization
- **Change Dropbox path**: Modify `DROPBOX` variable if data location differs
- **Adjust districts**: Filter `district_list` in viirs.R for specific regions
- **Modify thresholds**: Edit `thresholds` vector in ntl_presentation.R

---

## Key Findings & Uses

### What NTL Data Shows
- **Economic activity**: Nighttime light emissions correlate with electricity access and economic development
- **Electrification timing**: Changes in light intensity can indicate when villages received grid electricity
- **Infrastructure planning**: Overlay with power lines shows coverage gaps and expansion priorities

### Analysis Applications
1. **Baseline measurement**: Pre-treatment nightlight levels for RCT
2. **Impact evaluation**: Compare village light before/after electrification projects
3. **Development tracking**: Monitor rural electrification progress over time
4. **Policy assessment**: Evaluate effectiveness of energy access initiatives

### Integration with RCT
- Link village-level nightlights to household survey data
- Use pre-treatment nightlights as baseline covariate
- Measure spillover effects on neighboring villages
- Compare treatment group NTL trends vs. control

---

## Notes & Limitations

- VIIRS data starts January 2014; earlier analysis uses LRCC harmonized data (2010-2013)
- Some pixels may be affected by cloud cover in monthly data
- Winsorization at pixel level vs. village level affects estimates
- Infrastructure data (2011, 2022) may not align perfectly with nightlight years
- Transformer locations may be incomplete or outdated
- Border pixels may be assigned to wrong village in spatial joins

---

## File Size & Storage
- **Input rasters**: ~500MB each
- **Output Excel files**: ~20-50MB each
- **PNG maps**: ~2-5MB each (at 400 DPI)
- **GIF animation**: ~50-100MB

---

## Contact & Questions
For questions about NTL data processing or scripts, refer to:
- Data location: Rwanda Energy/EAQIP/datawork/Historical data/Nightlight/
- RCT data: Rwanda Energy/EAQIP/datawork/RCT_data/