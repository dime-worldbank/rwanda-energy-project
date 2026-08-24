#----------------------------------------------------------------------#
#### Master Paths Setup for Rwanda Energy Project	####
#----------------------------------------------------------------------#

# 0. Setup personal path to dropbox and Github
# 1. Per project, it follows folders setup of data, script, and output 
# Data and output folder is usually hosted on Dropbox while scripts are hosted on Github
# 2. General naming convention is TYPE_PROJECT_MODULE 
## TYPE (DATA, SCRIPT, OUTPUT)
## PROJECT ()
## MODULE (Baseline, Midline etc )

#--------------------------------------
# 0. Dropbox and Github paths 
#--------------------------------------

# Set paths per user. Add your username and folder path here
# To find your username, open your command line shell and type
# `Sys.getenv("USERNAME")` on Windows or `Sys.getenv("USER")` on MacOS

# Yeji work
if (Sys.getenv("USERNAME") == "wb636130"){
  DROPBOX <- file.path("C:/Users/wb636130/Dropbox/Rwanda Energy")
  GITHUB <- file.path("C:/Users/wb636130/OneDrive - WBG/Documents/Github/rwanda-energy-project")
}

#---------------------------------------
# EAQIP survey_cto
# PROJECT Abbreviation: 
#---------------------------------------

#### Data Folder ####
DATA_CTO <- file.path(DROPBOX, "EAQIP", "datawork", "HFC", "data")
DATA_ANALYSIS <- file.path(DROPBOX, "EAQIP", "datawork", "baseline analysis", "data")
DATA_CTO_BL <- file.path(DATA_CTO, "baseline-second-round")
DATA_CTO_ML <- file.path(DATA_CTO, "midline")

#### Script Folder ####
SCRIPT_CTO <- file.path(GITHUB, "RCT_data" )

SCRIPT_CTO_BL <- file.path(SCRIPT_CTO, "HFC", "scripts")
#SCRIPT_CTO_BL_ANALYSIS <- file.path()
SCRIPT_CTO_ML <- file.path(SCRIPT_CTO, "midline", "scripts")

#### Outputs Folder ####
#OUTPUT_SURVEY <- file.path(DROPBOX, "" )

#---------------------------------------
# Historical Expansion
# --------------------------------------

historical_data_path <- file.path(DROPBOX, "/EAQIP/datawork/Historical data")

data_path <- file.path( DROPBOX, "/EAQIP/datawork/Historical Expansion/data")

# July-30-2026, Commented out the output path to avoid overwriting existing files
# output_path <- file.path(
#   dropbox,
#   "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
# )

output_path <- file.path(DROPBOX, "/EAQIP/datawork/Historical Expansion/outputs_replication")