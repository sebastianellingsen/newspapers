
use broadband_newspaper.dta, clear

keep if year==2000|year==2006
g post = (year>2001)

    estimates clear
    * Baseline model
    qui ivreg2 circ i.np i.post c.total (num_ISPs_ipo = 1.post#c.total), cluster(np) partial(i.np)
    qui eststo
    qui estadd local controls "No"
    qui estadd local countyfe "No"
    qui estadd local edutrend "No"
    qui estadd local incometrend "No"
    qui estadd ysumm

    qui ivreg2 circ i.np i.post c.total share_white share_hisp share_black pop_ (num_ISPs_ipo = 1.post#c.total), cluster(np) partial(i.np)
    qui eststo
    qui estadd local controls "Yes"
    qui estadd local countyfe "No"
    qui estadd local edutrend "No"
    qui estadd local incometrend "No"
    qui estadd ysumm

    qui ivreg2 circ i.np i.post c.total share_white share_hisp share_black pop_ i.np i.state (num_ISPs_ipo = 1.post#c.total), cluster(np) partial(i.np)
    qui eststo
    qui estadd local controls "No"
    qui estadd local countyfe "Yes"
    qui estadd local edutrend "No"
    qui estadd local incometrend "No"
    qui estadd ysumm

    qui ivreg2 circ i.np i.post c.total c.pct_college_2000#c.year (num_ISPs_ipo = 1.post#c.total), cluster(np) partial(i.np)
    qui eststo
    qui estadd local controls "No"
    qui estadd local countyfe "No"
    qui estadd local edutrend "Yes"
    qui estadd local incometrend "No"
    qui estadd ysumm

    qui ivreg2 circ i.np i.post c.total c.income_2000#c.year (num_ISPs_ipo = 1.post#c.total), cluster(np) partial(i.np)
    qui eststo
    qui estadd local controls "No"
    qui estadd local countyfe "No"
    qui estadd local edutrend "No"
    qui estadd local incometrend "Yes"
    qui estadd ysumm

    esttab using tables/results.tex, replace  r2 se b(%9.3f) se(%9.3f) ///
           keep(num_ISPs_ipo) stats(controls countyfe edutrend incometrend r2_a,     ///
           labels("Demographic controls" "State FE" "Year $\times$ share college 2000" "Year $\times$ Av. income 2000" "Adj. R-Square"))          ///
           varlabels(num_ISPs_ipo "ISP")  ///
           style(tex) mlabels(none) collabels(none)  ///
           nolines prehead("\begin{tabular}{lcccccc}" "\hline") ///
           posthead(\hline) prefoot("") postfoot("\hline" "\end{tabular}") ///
           nostar nolegend
