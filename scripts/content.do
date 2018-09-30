
** State mentions
use data/data_newspaper_content/state_mentions/statementions.dta, clear
g staten = lower(ABC_state)
drop if year>2006
save data/data_newspaper_content/state_mentions/statementions_tmp.dta, replace

** Merging with main dataset
use broadband_newspaper.dta, clear
merge m:1 np staten year using data/data_newspaper_content/state_mentions/statementions_tmp.dta
keep if year==2000|year==2006
g post = (year>2001)
erase data/data_newspaper_content/state_mentions/statementions_tmp.dta
replace share_statementions = share_statementions*100


** Output
estimates clear
qui areg share_statementions c.total##i.post, a(np) cluster(np)
qui eststo
qui estadd local controls "No"
qui estadd local statefe "No"
qui estadd local countyyear "No"
qui estadd ysumm

qui areg share_statementions c.total##i.post share_white share_hisp share_black pop_, a(np) cluster(np)
qui eststo
qui estadd local controls "Yes"
qui estadd local statefe "No"
qui estadd local countyyear "No"
qui estadd ysumm

qui areg share_statementions c.total##i.post share_white share_hisp share_black pop_ i.state, a(np) cluster(np)
qui eststo
qui estadd local controls "Yes"
qui estadd local statefe "Yes"
qui estadd local countyyear "No"
qui estadd ysumm

qui areg share_statementions c.total##i.post share_white share_hisp share_black pop_ i.np#i.year, a(np) cluster(np)
qui eststo
qui estadd local controls "Yes"
qui estadd local statefe "No"
qui estadd local countyyear "Yes"
qui estadd ysumm

esttab using tables/results_content.tex, replace  r2 se b(%9.3f) se(%9.3f) ///
       keep(1.post#c.total total 1.post) stats(controls statefe countyyear r2_a,     ///
       labels("Demographic controls" "State FE" "Market $\times$ year FE" "Adj. R-Square"))          ///
       varlabels(1.post#c.total "RoW $\times$ Post" total "RoW" 1.post "Post")  ///
       style(tex) mlabels(none) collabels(none)  ///
       nolines prehead("\begin{tabular}{lcccc}" "\hline") ///
       posthead(\hline) prefoot("") postfoot("\hline" "\end{tabular}") ///
       nostar nolegend
