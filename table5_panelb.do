***************************************
************ RNFB prices **************
***************************************

***************************************
********** Table 5 Panel B ************
***************************************

*Locally install packages
global adobase "$basepath"
capture mkdir "$adobase"
sysdir set PERSONAL "$adobase/ado/personal"
sysdir set PLUS "$adobase/ado/plus"
sysdir set SITE "$adobase/ado/site"

set seed 123

*Older RNFB data from archived Food Mail websites
use nn_data, clear
*Newer RNFB data from current websites
append using rnfb_clean_merge

replace fmnnc=0 if fmnnc==.
replace cflag=0 if cflag==.
bysort community: egen maxtreat1=max(fmnnc)
bysort community: egen maxtreat2=max(cflag)

tab community if maxtreat1==1
tab community if maxtreat2==1

*Fixing one error - Stony Rapids was listed in 2016 press release as newly eligible but never used subsidy
replace maxtreat2=0 if community=="Stony Rapids"

gen mon=1 if month=="mar"
replace mon=2 if month=="jun"
replace mon=3 if month=="sep"
replace mon=4 if month=="dec"

egen period=group(year mon)

*April 2011 is when NNC subsidies kick in
gen treat1=0
replace treat1=1 if maxtreat1==1 & period>7 & period~=.

gen treat2=0
replace treat2=1 if maxtreat2==1 & t>23 & t~=.

egen z=group(province)
bysort community: egen minz=min(z)

cap: gen lnrnfb=ln(rnfb)
areg rnfb treat1 treat2 i.period if year<2019, absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2a=r(CI)[1,2]
local j1a=r(CI)[1,1]
local fmtj2a: display %4.3f `j2a'
local fmtj1a: display %4.3f `j1a'
local fmtja: display %4.3f `r(p)'
outreg2 using pricesb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat1,"(`fmtj1a', `fmtj2a')",ptreat1,`fmtja')	nocons replace

areg lnrnfb treat1 treat2 i.period if year<2019, absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2a=r(CI)[1,2]
local j1a=r(CI)[1,1]
local fmtj2a: display %4.3f `j2a'
local fmtj1a: display %4.3f `j1a'
local fmtja: display %4.3f `r(p)'
outreg2 using pricesb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat1,"(`fmtj1a', `fmtj2a')",ptreat1,`fmtja')	nocons append

areg rnfb treat1 treat2 i.period##i.z if year<2019, absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2a=r(CI)[1,2]
local j1a=r(CI)[1,1]
local fmtj2a: display %4.3f `j2a'
local fmtj1a: display %4.3f `j1a'
local fmtja: display %4.3f `r(p)'
outreg2 using pricesb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat1,"(`fmtj1a', `fmtj2a')",ptreat1,`fmtja')	nocons append

areg lnrnfb treat1 treat2 i.period##i.z if year<2019, absorb(community) vce(cluster community)
boottest treat1=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2: display %4.3f `j2'
local fmtj1: display %4.3f `j1'
local fmtj: display %4.3f `r(p)'
boottest treat2=0, boottype(wild) weighttype(webb)
local j2a=r(CI)[1,2]
local j1a=r(CI)[1,1]
local fmtj2a: display %4.3f `j2a'
local fmtj1a: display %4.3f `j1a'
local fmtja: display %4.3f `r(p)'
outreg2 using pricesb, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a))addtext(citreat1,"(`fmtj1', `fmtj2')",ptreat1,`fmtj',citreat1,"(`fmtj1a', `fmtj2a')",ptreat1,`fmtja')	nocons append
