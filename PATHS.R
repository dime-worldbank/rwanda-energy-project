#----------------------------------------------------------------------#
#### Master Paths Setup for Rwanda Energy Project	####
#----------------------------------------------------------------------#

# Housekeeping Rules
# 0. Setup personal path to dropbox and Github
# 1. Per project, it follows folders setup of data, script, and output 
# Data and output folder is usually hosted on Dropbox while scripts are hosted on Github
# 2. General naming convention is TYPE_PROJECT_MODULE 
## TYPE (DATA, SCRIPT, OUTPUT)
## PROJECT ()
## MODULE (Baseline, Midline etc)

#--------------------------------------
# 0. Dropbox, Github and OneDrive paths
#--------------------------------------

# Set paths per user. Add your username and folder path here
# To find your username, open your command line shell and type
# `Sys.getenv("USERNAME")` on Windows or `Sys.getenv("USER")` on MacOS

# Yeji work
if (Sys.getenv("USERNAME") == "wb636130"){
  DROPBOX <- file.path("C:/Users/wb636130/Dropbox/Rwanda Energy")
  GITHUB <- file.path("C:/Users/wb636130/OneDrive - WBG/Documents/Github/rwanda-energy-project")
  ONEDRIVE <- file.path("C:/Users/wb636130/OneDrive - WBG/Rwanda_Energy_Project")
}

DROPBOX_EAQIP <- file.path(DROPBOX, "EAQIP")
DROPBOX_ASCENT <- file.path(DROPBOX, "ASCENT")

# WBG OneDrive: files shared with the team and the survey firm
ONEDRIVE_HFC_ML <- file.path(ONEDRIVE, "EAQIP_survey/HFC_26")   # midline HFC dashboard

#---------------------------------------
# EAQIP survey_cto
# PROJECT Abbreviation: 
#---------------------------------------

#### Data Folder ####

DATA_RCT <- file.path(DROPBOX_EAQIP, "datawork/RCT_data/baseline/data/data")
DATA_SCOPE <- file.path(DROPBOX_EAQIP, "datawork/RCT_data/baseline/data/Updated scope villages& households")
DATA_SCREENING <- file.path(DROPBOX_EAQIP, "datawork/Screening/data")

DATA_ANALYSIS <- file.path(DROPBOX_EAQIP, "datawork/baseline analysis/data")

DATA_CTO <- file.path(DROPBOX_EAQIP, "datawork/HFC/data")
DATA_CTO_BL <- file.path(DATA_CTO, "baseline-second-round")
DATA_CTO_ML <- file.path(DATA_CTO, "midline")

DATA_ML <- file.path(DROPBOX_EAQIP, "questionnaires/02-midline")
DATA_ML_ELIGIBILITY <- file.path(DATA_ML, "01-SHS-eligibility")
DATA_ML_SCOPE <- file.path(DROPBOX_EAQIP, "questionnaires/02-midline/00-village-scope")
DATA_ML_PRELOAD <- file.path(DROPBOX_EAQIP, "questionnaires/02-midline/02-CTO-preload")


#### Script Folder ####
SCRIPT_CTO <- file.path(GITHUB, "RCT_data" )
SCRIPT_CTO_BL <- file.path(SCRIPT_CTO, "HFC", "scripts")
SCRIPT_CTO_ML <- file.path(SCRIPT_CTO, "midline", "scripts")

#### Outputs Folder ####
OUTPUT_HFC <- file.path(DROPBOX_EAQIP, "datawork/HFC/output")
OUTPUT_HFC_ML <- file.path(DROPBOX_EAQIP, "datawork/HFC_midline/output")
OUTPUT_ANALYSIS <- file.path(DROPBOX_EAQIP, "datawork/baseline analysis/output") #previously titled output_path in Xiaoming's code

#---------------------------------------
# Historical Expansion
# --------------------------------------

historical_data_path <- file.path(DROPBOX_EAQIP, "datawork/Historical data")

data_path <- file.path(DROPBOX_EAQIP, "datawork/Historical Expansion/data")

# July-30-2026, Commented out the output path to avoid overwriting existing files
# output_path <- file.path(
#   dropbox,
#   "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
# )

output_path <- file.path(DROPBOX_EAQIP, "datawork/Historical Expansion/outputs_replication")

#---------------------------------------
# Intervention Analysis 
# --------------------------------------

# Kazera's tracker of Readyboard Implementation by EDCL 
READYBOARD_K <- file.path(DROPBOX_EAQIP, "intervention/Readyboard Implementation/Implementation tracker/Readyboard tracker by Village")

