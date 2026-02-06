##########################
#Author: Xiaoming Zhang
#Date: March 26th 2025
#Purpose: Google earth enginen install
################################


## install`reticulate`, only one time
if (!("reticulate" %in% installed.packages()[,1])) {
  print("Installing package `reticulate`...")
  install.packages("reticulate")
} else { 
  print("Package `reticulate` already installed") }


library(reticulate)
Sys.which("python")   # system default
Sys.which("python3") 
use_python("C:\Users\wb614406\AppData\Local\Microsoft\WindowsApps\python.exe")

library(rgee)
rgee::ee_install()


