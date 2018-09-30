** ROW data
import delimited data/row/bbindex.csv, encoding(ISO-8859-1) clear
save data/row/bbindex.dta, replace

** Turnout
forvalues i = 1996(2)2006{
  clear
  import delimited data/electoral/house/`i'_4_0_2.csv, varnames(1) rowrange(3) encoding(ISO-8859-1)
  g year = `i'
  destring fips, generate(fips_int)
  destring totalvote, generate(totalvote_int)
  destring democrat, generate(democrat_int)
  destring republican, generate(republican_int)

  save data/electoral/house/house_`i'.dta, replace
}

use data/electoral/house/house_1996.dta, clear
forvalues i = 1998(2)2006{
  append using data/electoral/house/house_`i'.dta
}

keep fips_int totalvote_int year
rename fips_int fips
rename totalvote_int totalvote
rename democrat_int democrat
rename republican_int republican

save data/turnout.dta, replace

forvalues i = 1996(2)2006{
  erase data/electoral/house/house_`i'.dta
}


** County controls
use data/county_controls/___county_controls.dta, clear
keep if year>=1994 & year<=2006

save data/county_controls/county_controls_tmp.dta, replace

** Importing and merging data
use data/circulation/__Stromberg_ABC_data.dta, clear

rename member np
rename countyid fips

merge m:1 fips year using data/county_controls/county_controls_tmp.dta
merge m:m staten using data/row/bbindex.dta, nogenerate
merge m:1 fips year using data/isp/isps.dta, nogenerate
merge m:1 fips year using data/turnout.dta, nogenerate

keep if _merge == 3

* Keep only the right years, drop Alaska and Hawaii, drop missing observations
keep year np newspaper stateab county daily staten state countyn fips        ///
pct_college_2000 income_2000 age_2000 unemployment_2000 pop_ voting_pop_     ///
share_white share_hisp share_black total deployment supply demand num_ISPs   ///
num_ISPs_ipo totalvote

keep if num_ISPs_ipo!=.

/* keep year member newspaper countyn staten countyid daily state voting_pop_ */
keep if daily != .
keep if year >= 1994 & year <= 2006
keep if state!=2 & state != 15
bysort np: egen nr_state = nvals(state)
keep if nr_state<=7 & nr_state>1
replace staten = lower(staten)
replace countyn = lower(countyn)

g circ = (2*daily/voting_pop_)*100
g turnout = totalvote/voting_pop_*100

save broadband_newspaper.dta, replace

erase data/county_controls/county_controls_tmp.dta
