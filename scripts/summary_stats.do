
** Summary statistics

use broadband_newspaper.dta, clear
eststo clear

* Municipality year - level data
estpost sum circ total num_ISPs_ipo pop_ voting_pop_ share_white share_hisp  ///
             share_black pct_college_2000 income_2000 unemployment_2000      ///
             age_2000

est store a

esttab a using tables/summary_stats_county.tex, replace cells("mean(fmt(2)) sd min max count(fmt(0))") ///
          nodepvar noobs mlabels(none) postfoot("\hline" "\end{tabular}") ///
          prehead("\begin{tabular}{lccccc}" "\hline" " & Mean & Std. & Min. & Max. & N \\" ) ///
          collabels(none) nonumber varlabels(circ "Circulation" num_ISPs_ipo "ISPs"  ///
          pop_ "Population" voting_pop_ "Voting poulation" total "RoW" ///
          share_white "Share white "share_hisp "Share hispanic"  ///
          share_black "Share balck" pct_college_2000 "Share college in 2000" ///
          income_2000 "Av. income in 2000"  unemployment_2000 "Unemployment in 2000"       ///
          age_2000 "Av. age in 2000")


collapse circ total num_ISPs_ipo pop_ voting_pop_ share_white share_hisp     ///
             share_black pct_college_2000 income_2000 unemployment_2000      ///
             age_2000, by(np year)

estpost sum circ total num_ISPs_ipo pop_ voting_pop_ share_white share_hisp  ///
                          share_black pct_college_2000 income_2000 unemployment_2000      ///
                          age_2000

est store b

esttab b using tables/summary_stats_np.tex, replace cells("mean(fmt(2)) sd min max count(fmt(0))") ///
         nodepvar noobs mlabels(none) postfoot("\hline" "\end{tabular}") ///
         prehead("\begin{tabular}{lccccc}" "\hline" " & Mean & Std. & Min. & Max. & N \\" ) ///
         collabels(none) nonumber varlabels(circ "Circulation" num_ISPs_ipo "ISPs"  ///
         pop_ "Population" voting_pop_ "Voting poulation" total "RoW" ///
         share_white "Share white "share_hisp "Share hispanic"  ///
         share_black "Share black" pct_college_2000 "Share college in 2000" ///
         income_2000 "Av. income in 2000"  unemployment_2000 "Unemployment in 2000"       ///
         age_2000 "Av. age in 2000")
