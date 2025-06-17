*Clean the NNC quantity data that we received by generating relevant categories, changing names to match, and constructing annual aggregates***
use "PATH\quant_community.dta", clear
gen meat=0
replace meat=1 if category=="Meat, poultry and fish (fresh and frozen)"
replace meat=1 if category==" Meat, poultry and fish (fresh and frozen) "
gen meatplus=0
replace meatplus=1 if meat==1
replace meatplus=1 if category=="Nuts, seeds, peanut butter and other nut butters, tofu and other meat alternatives"
replace meatplus=1 if category==" Nuts, seeds, peanut butter and other nut butters, tofu and other meat alternatives "
replace meatplus=1 if category=="Eggs and egg substitutes"
replace meatplus=1 if category==" Eggs and egg substitutes "

replace community="Sambaa K’e" if community=="Sambaa K'e (Trout Lake)"
replace community="Gamètì" if community=="Gametì (Rae Lakes)"
replace community="Délı̨nę" if community=="Deline"
replace community="Ulukhaktok" if community=="Ulukhaktok (Holman)"
replace community="Wekweètì" if community=="Wekweti (Snare Lake)"
replace community="Łutselk'e" if community=="Lutsel K’e"
replace community="Whatì" if community=="Wha Ti"

bysort community year: egen totweight=total(weight)
bysort community year: egen totval=total(subsidy)
bysort community year: egen totweight_meat=total(weight*meat)
bysort community year: egen totval_meat=total(subsidy*meat)
bysort community year: egen totweight_meatplus=total(weight*meatplus)
bysort community year: egen totval_meatplus=total(subsidy*meatplus)

duplicates drop community year, force

*Inflate for 2021 since we only have first three quarters
foreach j of varlist totweight-totval_meatplus{
replace `j'=`j'*(4/3) if year==2021	
}
keep community year totweight* totval*
save "PATH\quanty_clean_cat", replace

*Clean NWT Community price data, change some names to be compatible interpolate given different reporting years [in the end we didn't use this interpolated data]
clear
*Input from spreadsheet "Historic Food Price Indices"

*Destring
foreach j of varlist y2019-y1997{
destring(`j'), force replace	
}
summ y*
*Create the interpolated years

replace y2019=y2015 if y2019==.
replace y1997=y2000 if y1997==.
summ y*
cap: drop y1998
gen y1998=((2/3)*y1997)+((1/3)*y2000)
gen y2003=((2/3)*y2004)+((1/3)*y2001)
gen y2008=((2/3)*y2010)+((1/3)*y2004)
gen y2013=((2/3)*y2012)+((1/3)*y2015)
*This one is trickiest, since there were some NN subsidy increases in 2019
*Gen y2018=y2019

expand 12
bysort community: gen counter=_n
gen year=.
replace year=1997 if counter==1
replace year=1998 if counter==2
replace year=2000 if counter==3
replace year=2001 if counter==4
replace year=2003 if counter==5
replace year=2004 if counter==6
replace year=2008 if counter==7
replace year=2010 if counter==8
replace year=2012 if counter==9
replace year=2013 if counter==10
replace year=2015 if counter==11
replace year=2019 if counter==12

gen nwtprice=.
foreach j in 1997 1998 2000 2001 2003 2004 2008 2010 2012 2013 2015 2019{
replace nwtprice=y`j' if year==`j'
}

keep community year nwtprice

*Fix some community names for the merge
replace community="Délı̨nę" if community=="Délį̀ne"
replace community="Fort Smith" if community=="Forth Smith"
replace community="Hay River Dene Reserve" if community=="Hay River Reserve"
replace community="Sambaa K’e" if community=="Sambaa Ke"

*Note that these all shop in Yellowknife
replace nwtprice=100 if community=="Detah" | community=="Yellowknife" | community=="Ndilǫ"

bysort year: gen bimp=nwtprice if community=="Fort Providence"
bysort year: egen baseimp=min(bimp)
replace nwtprice=baseimp if community=="Kakisa"
drop bimp baseimp

bysort year: gen bimp=nwtprice if community=="Fort Simpson"
bysort year: egen baseimp=min(bimp)
replace nwtprice=baseimp if community=="Jean Marie River"
drop bimp baseimp

save "\PATH\nwtprice_clean", replace
