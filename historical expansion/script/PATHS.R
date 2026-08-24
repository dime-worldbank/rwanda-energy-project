#--------------------------------------#
#### Dropbox and GitHub paths	####
#
# Set paths per user. Add your username and folder path here
#
# To find your username, open your command line shell and type
# `Sys.getenv("USERNAME")` on Windows or `Sys.getenv("USER")` on MacOS
#--------------------------------------#

# Xiaoming Work
if (Sys.getenv("USERNAME") == "wb614406"){
    dropbox <- 'C:/Users/wb614406/Dropbox' 
}

# Yeji work
if (Sys.getenv("USERNAME") == "wb636130"){
  dropbox <- file.path("C:/Users/wb636130/Dropbox")
}

historical_data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical data"
)

data_path <- file.path(
  dropbox,
  "Rwanda Energy/EAQIP/datawork/Historical Expansion/data"
)

# July-30-2026, Commented out the output path to avoid overwriting existing files
# output_path <- file.path(
#   dropbox,
#   "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs"
# )

output_path <- file.path(
   dropbox,
   "Rwanda Energy/EAQIP/datawork/Historical Expansion/outputs_replication"
 )