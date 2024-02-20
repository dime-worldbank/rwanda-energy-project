* For establishment 2017
* Read file
import delimited "your_file_path_here", clear

* Generate Village_ID
gen Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 + ID5

* Aggregate on the village level
*The question numbers are from document "q-rwa-nisr-ec-2017-eng.docx" which is the establishment census itself in 2017. Please refer back if unclear

#Please refer back to this questionnaire if anything is unclear
collapse (count) num_establishments (sum) total_worker = Q17_3 ///
                                    (sum) within_market = (Q2 == 1) ///
                                    (sum) outside_market = (Q2 == 2) ///
                                    (sum) industrial_zone = (Q2 == 3) ///
                                    (sum) ICPCs = (Q2 == 4) ///
                                    (sum) status_working = (Q3 == 1) ///
                                    (sum) status_temp_closed = (Q3 == 2) ///
                                    (sum) status_perm_closed = (Q3 == 3) ///
                                    (sum) private_sector = (Q7 == 1) ///
                                    (sum) cooperative_sector = (Q7 == 2) ///
                                    (sum) public_sector = (Q7 == 3) ///
                                    (sum) mixed_sector = (Q7 == 4) ///
                                    (sum) ngo_rwa = (Q7 == 5) ///
                                    (sum) ngo_intl = (Q7 == 6) ///
                                    (sum) head_office = (Q16 == 1) ///
                                    (sum) single_unit_establishment = (Q16 == 2) ///
                                    (sum) branch = (Q16 == 3) ///
                                    (sum) sub_branch = (Q16 == 4) ///
                                    (sum) registered_sector = (Q22_1 == 1) ///
                                    (sum) registered_district = (Q22_2 == 1), by(Village_ID)

* Save the results
export delimited "village2017.csv", replace


* For establishment 2020

* Read file
import delimited "your_file_path_here", clear

* Generate Village_ID
gen Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 + ID5

* Aggregate on the village level
*The question numbers are from document "Establishment Census 2020 Questionnaire in English.pdf" which is the establishment census itself in 2017. Refer back if unclear

collapse (count) num_establishments (sum) total_worker = Q17_3 ///
                                    (sum) within_market = (Q2 == 1) ///
                                    (sum) outside_market = (Q2 == 2) ///
                                    (sum) industrial_zone = (Q2 == 3) ///
                                    (sum) ICPCs = (Q2 == 4) ///
                                    (sum) status_working = (Q3_1 == 1) ///
                                    (sum) status_temp_closed = (Q3_1 == 2) ///
                                    (sum) status_perm_closed = (Q3_1 == 3) ///
                                    (sum) private_sector = (Q7 == 1) ///
                                    (sum) cooperative_sector = (Q7 == 2) ///
                                    (sum) public_sector = (Q7 == 3) ///
                                    (sum) mixed_sector = (Q7 == 4) ///
                                    (sum) ngo_rwa = (Q7 == 5) ///
                                    (sum) ngo_intl = (Q7 == 6) ///
                                    (sum) head_office = (Q16 == 1) ///
                                    (sum) single_unit_establishment = (Q16 == 2) ///
                                    (sum) branch = (Q16 == 3) ///
                                    (sum) sub_branch = (Q16 == 4) ///
                                    (sum) registered_sector = (Q22_1 == 1) ///
                                    (sum) registered_district = (Q22_2 == 1), by(Village_ID)

* Save the results
export delimited "village2020.csv", replace

* For establishment 2014
* Read file
import delimited "your_file_path_here", clear

* Generate Village_ID
gen Village_ID = ID1*10000000 + ID2*1000000 + ID3*10000 + ID4*100 + ID5

* Aggregate on the village level
*The question numbers are from document "q-rwa-nisr-ec-2014-eng.docx", refer back if unclear

collapse (count) num_establishments (sum) total_worker = Q20 ///
                                    (sum) within_market = (Q2 == 1) ///
                                    (sum) outside_market = (Q2 == 2) ///
                                    (sum) industrial_zone = (Q2 == 3) ///
                                    (sum) status_working = (Q3 == 1) ///
                                    (sum) status_temp_closed = (Q3 == 2) ///
                                    (sum) status_perm_closed = (Q3 == 3) ///
                                    (sum) private_sector = (Q8 == 1) ///
                                    (sum) public_sector = (Q8 == 3) ///
                                    (sum) mixed_sector = (Q8 == 2) ///
                                    (sum) ngo_rwa = (Q8 == 4) ///
                                    (sum) ngo_intl = (Q8 == 5) ///
                                    (sum) head_office = (Q18 == 1) ///
                                    (sum) single_unit_establishment = (Q18 == 2) ///
                                    (sum) branch = (Q18 == 3) ///
                                    (sum) sub_branch = (Q18 == 4) ///
                                    (sum) registered_sector = (Q27_1 == 1) ///
                                    (sum) registered_district = (Q27_2 == 1), by(Village_ID)

* Save the results
export delimited "village2014.csv", replace