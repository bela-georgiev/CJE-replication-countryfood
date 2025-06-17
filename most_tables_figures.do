*Locally install packages
global adobase "$basepath"
capture mkdir "$adobase"
sysdir set PERSONAL "$adobase/ado/personal"
sysdir set PLUS "$adobase/ado/plus"
sysdir set SITE "$adobase/ado/site"

set seed 123

use nwtcommunity, clear

merge m:1 community using remoteness
drop _merge

merge 1:1 year community using nwtprice_clean
drop _merge

*Need to create treatment indicator for all years, including those not in the NWT community survey
bysort community: egen basenneligible=min(nneligible)
replace nneligible=basenneligible
bysort community: egen basernfb_sub_treat=min(rnfb_sub_treat)
replace rnfb_sub_treat=basernfb_sub_treat

*Create outcome variables for consumption and hunting
gen obtain_ns=hh-(obtain_yes+obtain_no)
gen obtain_least=obtain_yes-(obtain_90+obtain_75+obtain_50+obtain_25+obtain_10)
summ obtain_ns obtain_least, detail
gen obtain_ns_share=obtain_ns/hh
gen obtain_least_share=obtain_least/hh
summ obtain_ns_share obtain_least_share, detail

gen obtain_share=obtain_yes/hh
gen obtain_share90=obtain_90/hh
gen obtain_share75=(obtain_90+obtain_75)/hh
gen obtain_share50=(obtain_90+obtain_75+obtain_50)/hh
gen obtain_share25=(obtain_90+obtain_75+obtain_50+obtain_25)/hh
gen obtain_share10=(obtain_90+obtain_75+obtain_50+obtain_25+obtain_10)/hh
summ obtain_share*

gen obtain_miss=obtain_yes
gen miss_counter=0
foreach j in 90 75 50 25 10 least{
replace obtain_miss=obtain_miss-obtain_`j' if obtain_`j'~=.
replace miss_counter=miss_counter+1 if obtain_`j'==.
}
gen obtain_impute=obtain_miss/miss_counter

foreach j in 90 75 50 25 10 least{
gen iobtain_`j'=obtain_`j'
replace iobtain_`j'=obtain_impute if obtain_`j'==.
}

summ obtain_* iobtain_*

gen iobtain_share90=iobtain_90/hh
gen iobtain_share75=(iobtain_90+iobtain_75)/hh
gen iobtain_share50=(iobtain_90+iobtain_75+iobtain_50)/hh
gen iobtain_share25=(iobtain_90+iobtain_75+iobtain_50+iobtain_25)/hh
gen iobtain_share10=(iobtain_90+iobtain_75+iobtain_50+iobtain_25+iobtain_10)/hh

summ iobtain_share* obtain_share

cap: drop meatshare
gen meatshare=(0.95*iobtain_90)
replace meatshare=meatshare+(0.875*iobtain_75)
replace meatshare=meatshare+(0.625*iobtain_50)
replace meatshare=meatshare+(0.375*iobtain_25)
replace meatshare=meatshare+(0.175*iobtain_10)
replace meatshare=meatshare+(0.05*iobtain_least)
replace meatshare=meatshare/hh
summ meatshare, detail

gen hunt_share=hunt_yes/pop15plus
gen hunt_sharefreq=hunt_freq/pop15plus
gen hunt_shareocc=(hunt_freq+hunt_occ)/pop15plus
gen hunt_shareday=(hunt_freq+hunt_occ+hunt_day)/pop15plus
gen hunt_sharerare=(hunt_freq+hunt_occ+hunt_day+hunt_rare)/pop15plus
summ hunt_share*

gen hunt_miss=hunt_yes
gen hmiss_counter=0
foreach j in freq occ day rare{
replace hunt_miss=hunt_miss-hunt_`j' if hunt_`j'~=.
replace hmiss_counter=hmiss_counter+1 if hunt_`j'==.
}

gen hunt_impute=hunt_miss/hmiss_counter

foreach j in freq occ day rare{
gen ihunt_`j'=hunt_`j'
replace ihunt_`j'=hunt_impute if hunt_`j'==.
}

summ hunt_* ihunt_*
gen ihunt_sharefreq=ihunt_freq/pop15plus
gen ihunt_shareocc=(ihunt_freq+ihunt_occ)/pop15plus
gen ihunt_shareday=(ihunt_freq+ihunt_occ+ihunt_day)/pop15plus
gen ihunt_sharerare=(ihunt_freq+ihunt_occ+ihunt_day+ihunt_rare)/pop15plus
summ ihunt_share* hunt_share

*An arbitrary hunting frequency aggregation
gen hunt_agg=(0.75*ihunt_freq) + (0.25*ihunt_occ) + (0.125*ihunt_day) + (0.05*(hunt_yes-ihunt_freq-ihunt_occ-ihunt_day))
gen hunt_agg_share=hunt_agg/pop15plus

foreach j of varlist obtain_share-obtain_share10 iobtain_share90-iobtain_share10{
gen ln`j'=ln(`j')
}

gen lnmeatshare=ln(meatshare)
gen lnshare=ln(share)

*Create treatment indicators
cap: drop treat treat1 treat2
gen treat=0
replace treat=1 if year>2010 & nneligible==1
replace treat=1 if year>2016 & rnfb_sub_treat==1

gen treat1=0
replace treat1=1 if year>2010 & nneligible==1

gen treat2=0
replace treat2=1 if year>2016 & rnfb_sub_treat==1

*Logged income and household count
gen lninc=ln(hh_median_income)
gen lnhh=ln(hh)

*Create region groups and yellowknife area and small community dummies
gen yarea=0
replace yarea=1 if community=="Yellowknife" | community=="Detah" | community=="Ndilǫ"
egen rgroup=group(region)


gen small=0
replace small=1 if community=="Kakisa"
replace small=1 if community=="Jean Marie River"


***************************************
********* Consumption outcomes ********
*************************************** 

*************************************** 
************** Table 2 ****************
*************************************** 

************** Panel A ****************
local yvar lnshare
local restrict 
local weight [weight=hh]
local controls lnhh
local xvar treat1 treat2 

areg `yvar' `xvar'  `controls' i.year `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append
tab treat1 treat2 if e(sample) & year==2018

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append
tab treat1 treat2 if e(sample) & year==2018
tab community if e(sample)==1 & year==2018 & treat1==1

areg `yvar' `xvar'  `controls' i.year##i.rgroup lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append
tab community if e(sample)==1 & year==2018 & treat1==1
*Drops Colville Lake and Sachs Harbor
tab community if e(sample)==1 & year==2018 & treat2==1
*Drops Wekweti
boottest lninc=0

local restrict
areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append
tab community if e(sample)==1 & year==2018 & treat1==1
tab community if e(sample)==1 & year==2018 & treat2==1
*No communities dropped, only years

************** Panel B ****************
cap: gen lnhh=ln(hh)
local yvar lnobtain_share
local restrict 
local weight [weight=hh]
local controls lnhh
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

tab community if e(sample)==1

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append
tab community if e(sample)==1
tab community if e(sample)==1 & year==2018 & treat1==1
tab community if e(sample)==1 & year==2018 & treat2==1
*Drops Detah, Yellowknife, Kakisa and Jean Marie River

areg `yvar' `xvar'  `controls' i.year##i.rgroup lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append
tab community if e(sample)==1
tab community if e(sample)==1 & year==2018 & treat1==1
tab community if e(sample)==1 & year==2018 & treat2==1

local restrict
areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

************** Panel C ****************
local yvar lnmeatshare
local restrict 
local weight [weight=hh]
local controls lnhh
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup   `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg `yvar' `xvar'  `controls'  i.year##i.rgroup  lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

local restrict
areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

***************************************
********* Appendix Table 1 ************
***************************************

*More consumption frequencies
foreach k in 90 75 50 25 10{
local yvar lniobtain_share`k'
local restrict 
local weight [weight=hh]
local controls lnhh
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup   `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using constable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg `yvar' `xvar'  `controls'  i.year##i.rgroup  lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

local restrict
areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using constable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append
}

*Estimates not in the paper, controlling for interaction of initial Indigenous share with year
corr abshare huntshare trapshare if year==2018

gen babshare=abshare if year==2008
bysort community: egen minabshare=min(babshare)
 
local yvar lnshare
local restrict 
local weight [weight=hh]
local controls lnhh
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year##i.rgroup c.minabshare##i.year  `restrict' `weight', absorb(community) vce(cluster community)

***************************************
********** Hunting outcomes ***********
*************************************** 

foreach j of varlist hunt_share-hunt_sharerare ihunt_sharefreq-ihunt_sharerare hunt_agg_share{
cap: gen ln`j'=ln(`j')
}
cap: gen lnpop15plus=ln(pop15plus)

***************************************
************** Table 3 ****************
***************************************

************** Panel A ****************
local yvar lnihunt_shareocc
local restrict 
local weight [weight=pop15plus]
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict 
areg `yvar' `xvar'  `controls' i.year##i.rgroup lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttable, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

************** Panel B ****************
local yvar lnhunt_share
local restrict 
local weight [weight=pop15plus]
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict 
areg `yvar' `xvar'  `controls' i.year##i.rgroup lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttableb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

************** Panel C ****************
local yvar lnhunt_agg_share
local restrict 
local weight [weight=pop15plus]
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict 
areg `yvar' `xvar'  `controls' i.year##i.rgroup lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttablec, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

***************************************
********* Appendix Table 2 ************
***************************************

*Non-imputed occasional, high frequency, principal component
pca ihunt_sharefreq ihunt_shareocc ihunt_shareday ihunt_sharerare hunt_share
predict hunt_agg2
corr hunt_agg2 hunt_agg_share

foreach k in hunt_agg2 lnhunt_shareocc lnihunt_sharefreq lnihunt_shareday lnihunt_sharerare{
local yvar	`k'
local restrict 
local weight [weight=pop15plus]
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict if yarea==0 & small==0
areg `yvar' `xvar'  `controls' i.year##i.rgroup  `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using hunttable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

local restrict 
areg `yvar' `xvar'  `controls' i.year##i.rgroup lninc `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest lninc=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append

areg `yvar' `xvar'  `controls' i.year##i.rgroup abshare `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest abshare=0, boottype(wild) weighttype(webb) 
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
outreg2 using hunttable`k', tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb')	nocons append
  
} 

*Estimates not in the paper, controlling for interaction of initial Indigenous share with year
local yvar lnihunt_shareocc
local restrict 
local weight [weight=pop15plus]
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year##i.rgroup c.minabshare##i.year `restrict' `weight', absorb(community) vce(cluster community)

***************************************
************** Table 4 ****************
***************************************

*Trapping, and hunting outcomes separated by Indigenous identity
gen lntrapshare=ln(trapshare)

local yvar lntrapshare
local restrict 
local weight [weight=pop15plus]
local controls lnpop15plus
local xvar treat1 treat2 

************* Column  1 **************
areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using other, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace    

gen lnindhuntshare=ln(indhuntshare)
gen lnnonindhuntshare=ln(nonindhuntshare)
gen lnindhuntshare2=asinh(indhuntshare)
gen lnnonindhuntshare2=asinh(nonindhuntshare)
bysort year: summ indhuntshare nonindhuntshare

gen lnindpop15plus=ln(indpop15plus)

************* Column  2 **************
local restrict if year>=2008
local weight [weight=indpop15plus]
local yvar lnindhuntshare2
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using other, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

************* Column  3 **************
local restrict if year>=2008
local weight [weight=nonindpop15plus]
local yvar lnnonindhuntshare2
local controls lnpop15plus
local xvar treat1 treat2 
areg `yvar' `xvar'  `controls' i.year##i.rgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using other, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a),clusters,e(N_clust)) addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

***************************************
************** Figures ****************
***************************************

set scheme s1mono

***************************************
************** Figure 3 ***************
***************************************

foreach t in 1998 2003 2008 2013 2018{
cap: gen dum`t'_=0
replace dum`t'=1 if year==`t'
}
foreach t in 1998 2003 2008 2013 2018{
gen dum`t'_fm=dum`t'_*nneligible
}
foreach t in 1998 2003 2008 2013 2018{
gen dum`t'_nn=dum`t'_*rnfb_sub_treat
}

local yvar2 lnihunt_shareocc
local xvar lnpop15plus
cap: drop coef* upci* loci*

local restrict2 
local dum community
local vars dum1998_nn dum2003_nn dum2008_nn dum2018_nn  dum1998_fm dum2003_fm dum2013_fm dum2018_fm i.year##i.rgroup 
areg `yvar2' `xvar' `vars' `restrict2' [weight=pop15plus], absorb(`dum') vce(cluster community)

gen coeffm=.
gen upcifm=.
gen locifm=.
gen coefnn=.
gen upcinn=.
gen locinn=.

local ci 1.96

forvalues j=1998(5)2018{
cap: boottest dum`j'_fm=0, boottype(wild) weighttype(webb) 	
cap: replace coeffm=_b[dum`j'_fm] if year==`j'
cap: replace upcifm=r(CI)[1,2] if year==`j'
cap: replace locifm=r(CI)[1,1] if year==`j'
cap: boottest dum`j'_nn=0, boottype(wild) weighttype(webb) 	
cap: replace coefnn=_b[dum`j'_nn] if year==`j'
cap: replace upcinn=r(CI)[1,2] if year==`j'
cap: replace locinn=r(CI)[1,1] if year==`j'

}

replace coeffm=0 if year==2008
replace coefnn=0 if year==2013

twoway (scatter coeffm year `restrict', yline(0) ytitle("Coefficient") title("Hunt occasionally/frequently: Food Mail-NNC") lcolor(black) legend(off) xline(2011)) (rcap upcifm locifm year)
graph save fmnnc`yvar2'.gph, replace
twoway (scatter coefnn year `restrict', yline(0) ytitle("Coefficient") title("Hunt occasionally/frequently: 2016 NNC reform") lcolor(black) legend(off) xline(2016)) (rcap upcinn locinn year)
graph save nnc2016`yvar2'.gph, replace

local yvar1 lnshare
local xvar lnhh
cap: drop coef* upci* loci*

local restrict2 
local dum community  
local vars dum1998_nn dum2003_nn dum2008_nn dum2018_nn  dum1998_fm dum2003_fm dum2013_fm dum2018_fm i.year##i.rgroup 
areg `yvar1' `xvar' `vars' `restrict2' [weight=hh], absorb(`dum') vce(cluster community)

gen coeffm=.
gen upcifm=.
gen locifm=.
gen coefnn=.
gen upcinn=.
gen locinn=.

local ci 1.96

forvalues j=1998(5)2018{
cap: boottest dum`j'_fm=0, boottype(wild) weighttype(webb) 	
cap: replace coeffm=_b[dum`j'_fm] if year==`j'
cap: replace upcifm=r(CI)[1,2] if year==`j'
cap: replace locifm=r(CI)[1,1] if year==`j'
cap: boottest dum`j'_nn=0, boottype(wild) weighttype(webb) 	
cap: replace coefnn=_b[dum`j'_nn] if year==`j'
cap: replace upcinn=r(CI)[1,2] if year==`j'
cap: replace locinn=r(CI)[1,1] if year==`j'
}

replace coeffm=0 if year==2008
replace coefnn=0 if year==2013

twoway (scatter coeffm year `restrict', yline(0) ytitle("Coefficient") title("Hunt >75% meat: Food Mail-NNC") lcolor(black) legend(off) xline(2011)) (rcap upcifm locifm year)
graph save fmnnc`yvar1'.gph, replace
twoway (scatter coefnn year `restrict', yline(0) ytitle("Coefficient") title("Hunt >75% meat: 2016 NNC reform") lcolor(black) legend(off) xline(2016)) (rcap upcinn locinn year)
graph save nnc2016`yvar1'.gph, replace
*Combined graph with just four panels ("main outcomes")

graph combine fmnnc`yvar1'.gph nnc2016`yvar1'.gph fmnnc`yvar2'.gph nnc2016`yvar2'.gph, altshrink ycommon xcommon

graph export figure3.pdf, replace

***************************************
********* Appendix Figure 1 ***********
***************************************

local yvar2 lnhunt_agg_share
local xvar lnpop15plus
cap: drop coef* upci* loci*

local restrict2 
local dum community
local vars dum1998_nn dum2003_nn dum2008_nn dum2018_nn  dum1998_fm dum2003_fm dum2013_fm dum2018_fm i.year##i.rgroup 
areg `yvar2' `xvar' `vars' `restrict2' [weight=pop15plus], absorb(`dum') vce(cluster community)

gen coeffm=.
gen upcifm=.
gen locifm=.
gen coefnn=.
gen upcinn=.
gen locinn=.

local ci 1.96

forvalues j=1998(5)2018{
cap: boottest dum`j'_fm=0, boottype(wild) weighttype(webb) 	
cap: replace coeffm=_b[dum`j'_fm] if year==`j'
cap: replace upcifm=r(CI)[1,2] if year==`j'
cap: replace locifm=r(CI)[1,1] if year==`j'
cap: boottest dum`j'_nn=0, boottype(wild) weighttype(webb) 	
cap: replace coefnn=_b[dum`j'_nn] if year==`j'
cap: replace upcinn=r(CI)[1,2] if year==`j'
cap: replace locinn=r(CI)[1,1] if year==`j'

}

replace coeffm=0 if year==2008
replace coefnn=0 if year==2013

twoway (scatter coeffm year `restrict', yline(0) ytitle("Coefficient") title("Hunt agg. share: Food Mail-NNC") lcolor(black) legend(off) xline(2011)) (rcap upcifm locifm year)
graph save fmnnc`yvar2'.gph, replace
twoway (scatter coefnn year `restrict', yline(0) ytitle("Coefficient") title("Hunt agg. share: 2016 NNC reform") lcolor(black) legend(off) xline(2016)) (rcap upcinn locinn year)
graph save nnc2016`yvar2'.gph, replace

local yvar1 lnmeatshare
local xvar lnhh
cap: drop coef* upci* loci*

local restrict2 
local dum community  
local vars dum1998_nn dum2003_nn dum2008_nn dum2018_nn  dum1998_fm dum2003_fm dum2013_fm dum2018_fm i.year##i.rgroup 
areg `yvar1' `xvar' `vars' `restrict2' [weight=hh], absorb(`dum') vce(cluster community)

gen coeffm=.
gen upcifm=.
gen locifm=.
gen coefnn=.
gen upcinn=.
gen locinn=.

local ci 1.96

forvalues j=1998(5)2018{
cap: boottest dum`j'_fm=0, boottype(wild) weighttype(webb) 	
cap: replace coeffm=_b[dum`j'_fm] if year==`j'
cap: replace upcifm=r(CI)[1,2] if year==`j'
cap: replace locifm=r(CI)[1,1] if year==`j'
cap: boottest dum`j'_nn=0, boottype(wild) weighttype(webb) 	
cap: replace coefnn=_b[dum`j'_nn] if year==`j'
cap: replace upcinn=r(CI)[1,2] if year==`j'
cap: replace locinn=r(CI)[1,1] if year==`j'
}

replace coeffm=. if year==1998
replace coefnn=. if year==1998
replace coeffm=0 if year==2008
replace coefnn=0 if year==2013

twoway (scatter coeffm year `restrict', yline(0) ytitle("Coefficient") title("Meat agg. share: Food Mail-NNC") lcolor(black) legend(off) xline(2011)) (rcap upcifm locifm year)
graph save fmnnc`yvar1'.gph, replace
twoway (scatter coefnn year `restrict', yline(0) ytitle("Coefficient") title("Meat agg. share: 2016 NNC reform") lcolor(black) legend(off) xline(2016)) (rcap upcinn locinn year)
graph save nnc2016`yvar1'.gph, replace
*Combined graph with just four panels ("main outcomes")

graph combine fmnnc`yvar1'.gph nnc2016`yvar1'.gph fmnnc`yvar2'.gph nnc2016`yvar2'.gph, altshrink ycommon xcommon

graph export pretrendsa.pdf, replace

***************************************
********* Appendix Figure 2 ***********
***************************************

local yvar2 lnhunt_share
local xvar lnpop15plus
cap: drop coef* upci* loci*

local restrict2 
local dum community
local vars dum1998_nn dum2003_nn dum2008_nn dum2018_nn  dum1998_fm dum2003_fm dum2013_fm dum2018_fm i.year##i.rgroup 
areg `yvar2' `xvar' `vars' `restrict2' [weight=pop15plus], absorb(`dum') vce(cluster community)

gen coeffm=.
gen upcifm=.
gen locifm=.
gen coefnn=.
gen upcinn=.
gen locinn=.

local ci 1.96

forvalues j=1998(5)2018{
cap: boottest dum`j'_fm=0, boottype(wild) weighttype(webb) 	
cap: replace coeffm=_b[dum`j'_fm] if year==`j'
cap: replace upcifm=r(CI)[1,2] if year==`j'
cap: replace locifm=r(CI)[1,1] if year==`j'
cap: boottest dum`j'_nn=0, boottype(wild) weighttype(webb) 	
cap: replace coefnn=_b[dum`j'_nn] if year==`j'
cap: replace upcinn=r(CI)[1,2] if year==`j'
cap: replace locinn=r(CI)[1,1] if year==`j'
	
}

replace coeffm=0 if year==2008
replace coefnn=0 if year==2013

twoway (scatter coeffm year `restrict', yline(0) ytitle("Coefficient") title("Hunt ever: Food Mail-NNC") lcolor(black) legend(off) xline(2011)) (rcap upcifm locifm year)
graph save fmnnc`yvar2'.gph, replace
twoway (scatter coefnn year `restrict', yline(0) ytitle("Coefficient") title("Hunt ever: 2016 NNC reform") lcolor(black) legend(off) xline(2016)) (rcap upcinn locinn year)
graph save nnc2016`yvar2'.gph, replace

local yvar1 lnobtain_share
local xvar lnhh
cap: drop coef* upci* loci*

local restrict2 
local dum community  
local vars dum1998_nn dum2003_nn dum2008_nn dum2018_nn  dum1998_fm dum2003_fm dum2013_fm dum2018_fm i.year##i.rgroup 
areg `yvar1' `xvar' `vars' `restrict2' [weight=hh], absorb(`dum') vce(cluster community)

gen coeffm=.
gen upcifm=.
gen locifm=.
gen coefnn=.
gen upcinn=.
gen locinn=.

local ci 1.96

forvalues j=1998(5)2018{
cap: boottest dum`j'_fm=0, boottype(wild) weighttype(webb) 	
cap: replace coeffm=_b[dum`j'_fm] if year==`j'
cap: replace upcifm=r(CI)[1,2] if year==`j'
cap: replace locifm=r(CI)[1,1] if year==`j'
cap: boottest dum`j'_nn=0, boottype(wild) weighttype(webb) 	
cap: replace coefnn=_b[dum`j'_nn] if year==`j'
cap: replace upcinn=r(CI)[1,2] if year==`j'
cap: replace locinn=r(CI)[1,1] if year==`j'
}

replace coeffm=. if year==1998
replace coefnn=. if year==1998
replace coeffm=0 if year==2008
replace coefnn=0 if year==2013

twoway (scatter coeffm year `restrict', yline(0) ytitle("Coefficient") title("Any meat: Food Mail-NNC") lcolor(black) legend(off) xline(2011)) (rcap upcifm locifm year)
graph save fmnnc`yvar1'.gph, replace
twoway (scatter coefnn year `restrict', yline(0) ytitle("Coefficient") title("Any meat: 2016 NNC reform") lcolor(black) legend(off) xline(2016)) (rcap upcinn locinn year)
graph save nnc2016`yvar1'.gph, replace
*Combined graph with just four panels ("main outcomes")

graph combine fmnnc`yvar1'.gph nnc2016`yvar1'.gph fmnnc`yvar2'.gph nnc2016`yvar2'.gph, altshrink ycommon xcommon

graph export pretrendsb.pdf, replace

*For Figure 1 and 2, generate summary stats for each of the three treatment groups
gen grouping=1 if nneligible==0 & rnfb_sub_treat==0
replace grouping=2 if nneligible==1
replace grouping=3 if rnfb_sub_treat==1

cap: drop gmean*
bysort grouping year: egen gmeanmeat75share=mean(share)
bysort grouping year: egen gmeanhuntoccshare=mean(ihunt_shareocc)

bysort grouping year: egen gmeanmeataggshare=mean(meatshare)
bysort grouping year: egen gmeanhuntaggshare=mean(hunt_agg_share)

bysort grouping year: egen gmeanmeatever=mean(obtain_share)
bysort grouping year: egen gmeanhuntever=mean(hunt_share)

***************************************
************** Figure 1 ***************
***************************************

local j gmeanmeat75share
twoway (line `j' year if grouping==1, title("A. >75% meat from harvest") ytitle("Mean community share") legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) sort(year)) (line `j' year if grouping==2, lpattern(dash) sort(year)) (line `j' year if grouping==3, lcolor(black) lpattern(dot) sort(year))
graph save panela, replace

local j gmeanhuntoccshare
twoway (line `j' year if grouping==1, title("B. Harvest frequently/occasionally") ytitle("Mean community share") legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) sort(year)) (line `j' year if grouping==2, lpattern(dash) sort(year)) (line `j' year if grouping==3, lcolor(black) lpattern(dot) sort(year))
graph save panelb, replace

local j gmeanmeatever
twoway (line `j' year if grouping==1, title("C. Any meat from harvest") ytitle("Mean community share") legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) sort(year)) (line `j' year if grouping==2, lpattern(dash) sort(year)) (line `j' year if grouping==3, lcolor(black) lpattern(dot) sort(year))
graph save panelc, replace

local j gmeanhuntever
twoway (line `j' year if grouping==1, title("D. Any harvest") ytitle("Mean community share") legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) sort(year)) (line `j' year if grouping==2, lpattern(dash) sort(year)) (line `j' year if grouping==3, lcolor(black) lpattern(dot) sort(year))
graph save paneld, replace

graph combine panela.gph panelb.gph panelc.gph paneld.gph, xcommon ycommon altshrink
graph export figure1.pdf, replace

***************************************
************** Figure 2 ***************
***************************************

replace yesrate=yesrate/100

*Cross-section scattter/summary stats
twoway (scatter share ihunt_shareocc if year==2018 & grouping==1, mlabel(community) mlabposition(12) legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) ytitle("Share >75% harvest mean") xtitle("Share harvest freq./occ.")) (scatter share ihunt_shareocc if year==2018 & grouping==2, mlabposition(12) mlabel(community)) (scatter share ihunt_shareocc if year==2018 & grouping==3, mlabposition(12) mlabel(community))
graph save huntshare.gph, replace

twoway (scatter share abshare if year==2018 & grouping==1, mlabel(community) mlabposition(12) legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) ytitle("Share >75% harvest mean") xtitle("Share Indigenous")) (scatter share abshare if year==2018 & grouping==2, mlabel(community) mlabposition(12) ) (scatter share abshare if year==2018 & grouping==3, mlabel(community) mlabposition(12) )
graph save abshare.gph, replace

twoway (scatter share yesrate if year==2018 & grouping==1, mlabel(community) mlabposition(12) legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) ytitle("Share >75% harvest mean") xtitle("Share Food Insecure")) (scatter share yesrate if year==2018 & grouping==2, mlabel(community) mlabposition(12) ) (scatter share yesrate if year==2018 & grouping==3, mlabel(community) mlabposition(12) )
graph save yesshare.gph, replace

twoway (scatter share hh_median_income if year==2018 & grouping==1, mlabel(community) mlabposition(12) legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) ytitle("Share >75% harvest mean") xtitle("Med.hh.income")) (scatter share hh_median_income if year==2018 & grouping==2, mlabel(community) mlabposition(12) ) (scatter share hh_median_income if year==2018 & grouping==3, mlabel(community) mlabposition(12) )
graph save incshare.gph, replace

twoway (scatter share remote if year==2018 & grouping==1, mlabel(community) mlabposition(12) legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) ytitle("Share >75% harvest mean") xtitle("Remoteness index")) (scatter share remote if year==2018 & grouping==2, mlabel(community) mlabposition(12) ) (scatter share remote if year==2018 & grouping==3, mlabel(community) mlabposition(12) )
graph save remote.gph, replace

cap: drop nwtprice2019 nwtprice2018
gen nwtprice2019=nwtprice if year==2019
bysort community: egen nwtprice2018=min(nwtprice2019)
replace nwtprice=nwtprice2018 if year==2018

twoway (scatter share nwtprice if year==2013 & grouping==1, mlabel(community) mlabposition(12) legend(order(1 "No subsidy" 2 "FM-NNC" 3 "NNC2016")) ytitle("Share >75% harvest mean") xtitle("Food price (Yellowknife=100)")) (scatter share nwtprice if year==2013 & grouping==2, mlabel(community) mlabposition(12) ) (scatter share nwtprice if year==2018 & grouping==3, mlabel(community) mlabposition(12) )
graph save nwtprice.gph, replace

graph combine huntshare.gph abshare.gph yesshare.gph incshare.gph remote.gph nwtprice.gph, altshrink
graph export figure2.pdf, replace

***************************************
********* Summary Statistics **********
***************************************

***************************************
************** Table 1 ****************
***************************************

*Top panel
*We manually copy pasted these to make Latex table
foreach j in hh pop15plus share obtain_share meatshare ihunt_shareocc hunt_share indhuntshare nonindhuntshare hunt_agg_share abshare hh_median_income trapshare remote yesrate nwtprice{
cap bysort grouping year: egen gmean`j'=mean(`j')
}

by grouping year: gen gcounter=_n
browse grouping year gmeanhh-gmeanyesrate if gcounter==1 & (year==1998 | year==2003 | year==2008 | year==2013 | year==2018)

*Bottom panel (prices)
sort grouping year
browse grouping year gmeannwtprice if gcounter==1 & (year==2000 | year==2004 | year==2010 | year==2015 | year==2019)

***************************************
******* Price using NWT index *********
***************************************

***************************************
********** Table 5 Panel A ************
***************************************

bysort community: egen brgroup=min(rgroup)

gen t2019=0
replace t2019=1 if year==2019 & (treat1==1 | treat2==1)

*Note that Tuktoyaktuk loses subsidy eligibility in 2019 and does not benefit from 2019 subsidy increase, but Kenny et al. show that there was no short-term price effect in 2018 as freight cost decline seems to have offset loss of subsidies. So consider Tuktoyaktuk as contiuing as not losing previous subsidy in 2019, but not gaining new subsidy
replace t2019=0 if community=="Tuktoyaktuk"
replace treat1=1 if community=="Tuktoyaktuk" & year==2019

*Prices
local xvar treat1 treat2 t2019

*Restrict to non-imputed years
local restrict if year~=1998 & year~=2003 & year~=2008 & year~=2013 & year~=2018 & year>2003 & year~=2012

local weight
areg nwtprice `xvar' i.year `restrict' `weight', absorb(community) vce(cluster community)

boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest t2019=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
boottest t2019+treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2c: display %4.3f `j2'
local fmtj1c: display %4.3f `j1'
local fmtjc: display  %4.3f `r(p)'
outreg2 using prices, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb',citreat4,"(`fmtj1c', `fmtj2c')",ptreat4,`fmtjc')	nocons replace

cap: gen lnnwtprice=ln(nwtprice)
areg lnnwtprice `xvar' i.year `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest t2019=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
boottest t2019+treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2c: display %4.3f `j2'
local fmtj1c: display %4.3f `j1'
local fmtjc: display  %4.3f `r(p)'
outreg2 using prices, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb',citreat4,"(`fmtj1c', `fmtj2c')",ptreat4,`fmtjc')	nocons append

areg nwtprice `xvar' i.year##i.brgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest t2019=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
boottest t2019+treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2c: display %4.3f `j2'
local fmtj1c: display %4.3f `j1'
local fmtjc: display  %4.3f `r(p)'
outreg2 using prices, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb',citreat4,"(`fmtj1c', `fmtj2c')",ptreat4,`fmtjc')	nocons append

areg lnnwtprice `xvar' i.year##i.brgroup `restrict' `weight', absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
boottest t2019=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2b: display %4.3f `j2'
local fmtj1b: display %4.3f `j1'
local fmtjb: display  %4.3f `r(p)'
boottest t2019+treat2=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2c: display %4.3f `j2'
local fmtj1c: display %4.3f `j1'
local fmtjc: display  %4.3f `r(p)'
outreg2 using prices, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja',citreat3,"(`fmtj1b', `fmtj2b')",ptreat3,`fmtjb',citreat4,"(`fmtj1c', `fmtj2c')",ptreat4,`fmtjc')	nocons append
