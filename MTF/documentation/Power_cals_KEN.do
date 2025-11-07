***********************************************************************************************
*Power Calculations for Kenya GEWE 
*Written by: Tanay Balantrapu
*Date: March 07, 2022
*Notes: The estimates for control were obtained from Kenya Integrated Household Budget Survey.
*The LASSO estimates were from El Salvador power calculations. 
***********************************************************************************************

set more off
clear 
set matsize 4000


***Household consumption  - from the ES power calculations***


global sigma2 0.46



local rho 0.05

local z 2.80 /*total z from: alpha=0.05, and beta=0.8*/
local m 20
local N 1849
local P 0.66


*Calc MDE from inputs above
di "MDE: " `= $sigma2 * `z' * sqrt((1+`rho'*(`m'-1))/(`N'*`P'*(1-`P')))'


*Expected Effect Size from Assumed Take-up %, Transfer Size relative to monthly cons, and Marginal Propensity to Consume
local trans 264/ 776  /*put transfer size relative to mean of total consumption*/
local takeup 0.70
local mpc 0.67

di "Expected Effect Size: " `= `trans'*`takeup'*`mpc''


***Women preferred consumption***
**** Choose parameters ****

global sigma 1.39

local rho 0.05

local z 2.80 /*total z from: alpha=0.05, and beta=0.8*/
local m 30
local N 2250 /*30*75 villages */
local P 0.5
*Implies total program cost 2250*75/2 = 84375

*Calc MDE from inputs above
di "MDE: " `= $sigma * `z' * sqrt((1+`rho'*(`m'-1))/(`N'*`P'*(1-`P')))'

*Expected Effect Size from Assumed Take-up %, Transfer Size relative to monthly cons, and Marginal Propensity to Consume
local trans 75 / 120 /*put transfer relative to mean of women's income*/
local takeup 0.5
local mpc 0.67

di "Expected Effect Size: " `= `trans'*`takeup'*`mpc''


*If increase transfer and reduce number per village
local m 20
local N 1500
local P 0.5
*Implies total program cost 1875*90/2 = 84375


*Calc MDE from inputs above
di "MDE Fewer Villages: " `= $sigma * `z' * sqrt((1+`rho'*(`m'-1))/(`N'*`P'*(1-`P')))'

*Expected Effect Size from Assumed Take-up %, Transfer Size relative to monthly cons, and Marginal Propensity to Consume
local trans 112 / 120 /*put transfer relative to mean of women's income*/
local takeup 0.5
local mpc 0.67

di "Expected Effect Size Bigger Transfer: " `= `trans'*`takeup'*`mpc''
