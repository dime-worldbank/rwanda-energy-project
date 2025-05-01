use "C:\Users\wb614406\Dropbox\Rwanda Energy\EAQIP\datawork\Historical Expansion\data\expansion_ec.dta", clear

ssc install ietoolkit

iebaltab num_establishment total_employee, grpvar("earp_2011") stats(desc(sd) pair(diff) ) savetex("C:\Users\wb614406\Dropbox\Rwanda Energy\EAQIP\datawork\Historical Expansion\outputs\bt_earp.tex") replace

iebaltab num_establishment total_employee, grpvar("earp_2011") fixedeffect(cell_id) stats(desc(sd) pair(se)) savetex("C:\Users\wb614406\Dropbox\Rwanda Energy\EAQIP\datawork\Historical Expansion\bt_earp_cell.tex") replace

iebaltab num_establishment total_employee, grpvar("earp_2011") fixedeffect(sector_id) stats(desc(sd) pair(se)) savetex("C:\Users\wb614406\Dropbox\Rwanda Energy\EAQIP\datawork\Historical Expansion\bt_earp_sector.tex") replace





iebaltab num_establishment total_employee, grpvar("earp_2011") stats(desc(sd) pair(se)) fixedeffect(cell_id) savetex("C:\Users\wb614406\Dropbox\Rwanda Energy\EAQIP\datawork\Historical Expansion\bt_earp_cell.tex") replace