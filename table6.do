***************************************
************** Table 6 ****************
***************************************

*Locally install packages
global adobase "$basepath"
capture mkdir "$adobase"
sysdir set PERSONAL "$adobase/ado/personal"
sysdir set PLUS "$adobase/ado/plus"
sysdir set SITE "$adobase/ado/site"

set seed 123

use quanty_clean_cat, clear
drop if year>2018
*Coding the communities treated in Oct. 2016
gen treat=0
replace treat=1 if community=="Ogoki/Marten Falls"
replace treat=1 if community=="Webequie"
replace treat=1 if community=="Lansdowne House/Neskantaga"
replace treat=1 if community=="Eabamet Lake (Fort Hope)"
replace treat=1 if community=="Summer Beaver"
replace treat=1 if community=="Wawakapewin"
replace treat=1 if community=="North Spirit Lake"
replace treat=1 if community=="Cat Lake"
replace treat=1 if community=="Poplar Hill"
replace treat=1 if community=="Deer Lake"
replace treat=1 if community=="Favourable Lake (Sandy Lake)"
replace treat=1 if community=="Keewaywin"
replace treat=1 if community=="Sachigo Lak"
replace treat=1 if community=="Kasabonika"
replace treat=1 if community=="Angling Lake/Wapekeka"
replace treat=1 if community=="Wunnummin Lake"
replace treat=1 if community=="Kingfisher Lake"
replace treat=1 if community=="Weagamow Lake"
replace treat=1 if community=="Pikangikum"
replace treat=1 if community=="Tadoule Lake"
replace treat=1 if community=="Brochet"
replace treat=1 if community=="York Landing"
replace treat=1 if community=="Shamattawa"
replace treat=1 if community=="Berens River"
replace treat=1 if community=="Uranium City"
replace treat=1 if community=="Wollaston Lake"
replace treat=1 if community=="Fond-du-Lac"
replace treat=1 if community=="Wekweètì"
replace treat=1 if community=="Whatì"
replace treat=1 if community=="Łutselk'e"
replace treat=1 if community=="Gamètì"
replace treat=1 if community=="Port-Menier"
replace treat=1 if community=="Blanc-Sablon"
replace treat=1 if community=="Fort Chipewyan"

gen treat_post=0
replace treat_post=1 if year>2016 & treat==1
replace treat_post=0.25 if year==2016 & treat==1

*Stats about importance of subsidy
gen sharemeatv=totval_meat/totval
gen sharemeatplusv=totval_meatplus/totval

*summ totval totval_meat totval_meatplus sharemeatv sharemeatplusv if (treat_post==1) & year>2016 & totval~=.
*tab community if  (rnfb_sub_treat==1) & year<2016 & share~=. & totval~=.
*summ totval totval_meat totval_meatplus sharemeatv sharemeatplusv hh if (rnfb_sub_treat==1) & year==2013
*tab community if (rnfb_sub_treat==1) & year==2013

*summ totval totval_meat totval_meatplus sharemeatv sharemeatplusv if (nneligible==1 | rnfb_sub_treat==1) & year==2018
gen sharemeatw=totweight_meat/totweight
gen sharemeatplusw=totweight_meatplus/totweight
*summ totweight totweight_meat totweight_meatplus sharemeatw sharemeatplusw  if (nneligible==1 | rnfb_sub_treat==1) & year==2018
*17-23% of the subsidy quant
*16-22 in our sample

**Income mechanism***
*gen lnval=ln(totval)
*bysort community: egen base_hh=min(hh)
replace totval=0 if totval==.
*gen valhh=totval/base_hh
*gen valmeathh=totval_meat/base_hh
*gen weighthh=totweight/base_hh
*gen weightmeathh=totweight_meat/base_hh

*summ valhh valmeathh weighthh weightmeathh if weighthh>400 & nneligible==1 & (year==2017 | year==2018), detail
*summ valhh valmeathh weighthh weightmeathh if weighthh>400 & rnfb_sub_treat==1 & (year==2017 | year==2018), detail
*17%

*Need to merge in population
*Rename:
replace community="Wha Ti" if community=="Whatì"
replace community="Lutsel K’e" if community=="Łutselk'e"
replace community="Wekweti (Snare Lake)" if community=="Wekweètì"
replace community="Ulukhaktok (Holman)" if community=="Ulukhaktok"
replace community="Gametì (Rae Lakes)" if community=="Gamètì"
replace community="Deline" if community=="Délı̨nę"
merge m:1 community using community_profiles_clean, keepusing(population)
keep if _merge==3
drop _merge

*NWT only sample
gen nwt=0
replace nwt=1 if community=="Aklavik"
replace nwt=1 if community=="Colville Lake"
replace nwt=1 if community=="Deline"
replace nwt=1 if community=="Fort Good Hope"
replace nwt=1 if community=="Norman Wells"
replace nwt=1 if community=="Paulatuk"
replace nwt=1 if community=="Sachs Harbour"
replace nwt=1 if community=="Tuktoyaktuk"
replace nwt=1 if community=="Tulita"
replace nwt=1 if community=="Ulukhaktok (Holman)"
replace nwt=1 if community=="Gametì (Rae Lakes)"
replace nwt=1 if community=="Wekweti (Snare Lake)"
replace nwt=1 if community=="Wha Ti"
replace nwt=1 if community=="Lutsel K’e"

gen totweight_pc=totweight/population
gen totweight_meat_pc=totweight_meat/population

gen totval_pc=totval/population
gen totval_meat_pc=totval_meat/population

cap: gen lntotweight_pc=ln(totweight_pc)
cap: gen lntotweight_meat_pc=ln(totweight_meat_pc)

************** Panel A ****************
*Approx. 2 KG per person per week
local restrict if year>2012 & totweight_pc>100 & nwt==1
local xvar treat_post

areg totval_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)

boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg totval_meat_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg totweight_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg totweight_meat_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg lntotweight_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg lntotweight_meat_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

************** Panel B ****************
*Approx. 2 KG per person per week
local restrict if year>2012 & totweight_pc>100 
local xvar treat_post

areg totval_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight2, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons replace

areg totval_meat_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight2, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg totweight_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight2, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg totweight_meat_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight2, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg lntotweight_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight2, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append

areg lntotweight_meat_pc `xvar' i.year `restrict', absorb(community) vce(cluster community)
boottest treat_post=0, boottype(wild) weighttype(webb)
local j2=r(CI)[1,2]
local j1=r(CI)[1,1]
local fmtj2a: display %4.3f `j2'
local fmtj1a: display %4.3f `j1'
local fmtja: display  %4.3f `r(p)'
outreg2 using incweight2, tex label ctitle() bdec(3) rdec(3) alpha(0.01,0.05,0.10) addstat(Adj R-squared,e(r2_a)) addtext(citreat2,"(`fmtj1a', `fmtj2a')",ptreat2,`fmtja')	nocons append
