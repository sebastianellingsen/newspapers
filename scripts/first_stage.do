use broadband_newspaper.dta, clear

keep if year==2000|year==2006
g post = (year>2001)


  estimates clear
  * Baseline model
  qui areg circ c.total##i.post i.np#i.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local statefe "No"
  qui estadd local fs_test="-"
  qui estadd ysumm

  qui areg circ c.total##i.post share_white share_hisp share_black pop_ i.np#i.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "Yes"
  qui estadd local statefe "No"
  qui estadd local fs_test="-"
  qui estadd ysumm

  qui areg circ c.total##i.post share_white share_hisp share_black pop_ i.np#i.year c.year#unemployment_2000, a(np) cluster(np)
  qui eststo
  qui estadd local controls "Yes"
  qui estadd local statefe "Yes"
  qui estadd local fs_test="-"
  qui estadd ysumm

  * Baseline model
  qui areg num_ISPs_ipo c.total##i.post i.np#i.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local statefe "No"
  qui test  1.post#c.total
  qui estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg num_ISPs_ipo c.total##i.post share_white share_hisp share_black pop_ i.np#i.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "Yes"
  qui estadd local statefe "No"
  qui test  1.post#c.total
  qui estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg num_ISPs_ipo c.total##i.post i.np#i.year share_white share_hisp share_black pop_ c.year#unemployment_2000, a(np) cluster(np)
  qui eststo
  qui estadd local controls "Yes"
  qui estadd local statefe "Yes"
  qui test  1.post#c.total
  qui estadd scalar fs_test=r(F)
  qui estadd ysumm

  esttab using tables/first_stage.tex, replace  r2 se b(%9.3f) se(%9.3f) ///
         stats(controls statefe r2_a fs_test,     ///
         labels("Demographic controls" "Market $\times$ year FE" "Adj. R-Square" "F-value (instr.)"))      ///
         varlabels(1.post#c.total "RoW $\times$ Post" total "RoW" 1.post "Post")  ///
         keep(1.post#c.total total 1.post) style(tex) mlabels(none) collabels(none)  ///
         nolines prehead("\begin{tabular}{lcccccc}" "\hline") ///
         posthead(\hline) prefoot("") postfoot("\hline" "\end{tabular}") ///
         nostar nolegend mgroups("Circulation" "ISPs", pattern(1 0 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))






foreach y of varlist num_ISPs_ipo{

  estimates clear
  * Baseline model
  qui areg `y' c.total##i.post, a(np) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local countyfe "No"
  qui estadd local edutrend "No"
  qui estadd local incometrend "No"
  qui estadd local poptrend "No"
  qui test  1.post#c.total
  estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg `y' c.total##i.post share_white share_hisp share_black pop_, a(np) cluster(np)
  qui eststo
  qui estadd local controls "Yes"
  qui estadd local countyfe "No"
  qui estadd local edutrend "No"
  qui estadd local incometrend "No"
  qui estadd local poptrend "No"
  qui test  1.post#c.total
  estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg `y' c.total##i.post i.np, a(fips) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local countyfe "Yes"
  qui estadd local edutrend "No"
  qui estadd local incometrend "No"
  qui estadd local poptrend "No"
  qui test  1.post#c.total
  estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg `y' c.total##i.post c.pct_college_2000#c.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local countyfe "No"
  qui estadd local edutrend "Yes"
  qui estadd local incometrend "No"
  qui estadd local poptrend "No"
  qui test  1.post#c.total
  estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg `y' c.total##i.post c.income_2000#c.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local countyfe "No"
  qui estadd local edutrend "No"
  qui estadd local incometrend "Yes"
  qui estadd local poptrend "No"
  qui test  1.post#c.total
  estadd scalar fs_test=r(F)
  qui estadd ysumm

  qui areg `y' c.total##i.post c.age_2000#c.year, a(np) cluster(np)
  qui eststo
  qui estadd local controls "No"
  qui estadd local countyfe "No"
  qui estadd local edutrend "No"
  qui estadd local incometrend "Yes"
  qui estadd local poptrend "No"
  qui test  1.post#c.total
  estadd scalar fs_test=r(F)
  qui estadd ysumm

  esttab using tables/first_stage_appendix.tex, replace  r2 se b(%9.3f) se(%9.3f) ///
         stats(controls countyfe edutrend incometrend poptrend r2_a fs_test,     ///
         labels("Demographic controls" "County FE" "Year $\times$ college 2000" "Year $\times$ income 2000"        ///
        "Year $\times$ age 2000" "Adj. R-Square" "F-value (instr.)"))      ///
         varlabels(1.post#c.total "RoW $\times$ Post")  ///
         keep(1.post#c.total) style(tex) mlabels(none) collabels(none)  ///
         note("All specifications include year and municipality fixed effects." ///
         "Standard errors clustered at the municipality level.")  ///
         nolines prehead("\begin{tabular}{lcccccc}" "\hline") ///
         posthead(\hline) prefoot("") postfoot("\hline" "\end{tabular}") ///
         nostar nolegend

}
