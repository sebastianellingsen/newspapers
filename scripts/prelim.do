/* keep if year==2000|year==2006
g post = (year>2001)
areg circ i.post##c.total, a(np) cluster(np) */
/* collapse (mean) circ num_ISPs num_ISPs_ipo total, by(post fips np) */











areg turnout c.total##i.year i.state, a(np) cluster(np)

estimates store interactions
coefplot interactions, keep(*.year#c.total) yline(0, lc(black))    ///
              legend(off) graphregion(color(white)) vertical   ///
              graphregion(fcolor(white)) fcolor(none) lcolor(black)          ///
              legend(off) yla(, nogrid) ciopts(lc(black)) mfc(black)         ///
              mcolor(black)


keep if year==2000|year==2006
g turnout = totalvote/voting_pop_
areg turnout c.total##i.post i.np#i.year, a(np) cluster(np)





              g treat = (total>35.75  )
              preserve
              collapse (mean) circ num_ISPs num_ISPs_ipo, by(treat year)

              twoway(connected num_ISPs_ipo year if treat == 1)(connected num_ISPs_ipo year if treat == 0)

              restore
