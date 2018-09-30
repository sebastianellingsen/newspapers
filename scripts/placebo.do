/* This data estimates differences in trends between counties with different row
index within newpaper markets. */

** Placebo exercises
use broadband_newspaper.dta, clear

foreach x of varlist circ num_ISPs_ipo pop_ voting_pop_ share_white ///
                      share_hisp share_black{

  use broadband_newspaper.dta, clear

  * Placebo regression
  qui areg `x' c.total##ib2001.year, a(np) cluster(np)
  regsave

  * Cleaning data
  qui keep if substr((var),-6 ,.) == ".total"
  forvalues i = 1996(1)2006{
    qui replace var = "`i'" if var == "`i'.year#c.total"
  }

  qui replace var = "2001" if var == "2001b.year#co.total"
  qui destring var, generate(year)
  qui drop var

  * Exporting data
  qui save data/placebo_`x'.dta, replace

}
