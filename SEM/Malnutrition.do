*import the data
use "/Users/mdsayeefalam/Documents/Data/DHS Data/DHS7/IAKR74FL.dta", clear

*keep the required variables
*keep v025 h11 v005 m14 v008 b3 b9 caseid hw70 hw71 hw72 v445 v133 v190 v201 m15 v130 m19 m2a m2b m2g m2h m2i m2j m2k v116 v113 v467d v467e m4 v409 v410 v412c v413 v411 v411a v412a v414a v414e v414f v414g v414i v414j v414k v414l v414m v414n v414o v414p v414s v414t v414v m39a

*generate weight
gen wgt = v005/1000000

*generate age of child in months and keep only children <2 years (ie <24 months) and last born
gen aoc = v008 - b3
keep if aoc < 24 & b9 == 0
keep if _n == 1 | caseid != caseid[_n-1]

*recode to match with nfhs4 table 10.something
recode aoc (0 1 = 0 "<2")(2 3 = 1 "2-3")(4 5 = 2 "4-5")(6/8 = 3 "6-8")(9/11 = 4 "9-11")(12/17 = 5 "12-17")(18/23 = 6 "18-23"),gen(r_aoc)
ta r_aoc [iw = wgt]				
*sample matching done

*recoding variables
recode hw70(-600/-200=1 "Yes")(-200/600=0 "No")(else=.),gen(Stunting)
recode hw71(-600/-200=1 "Yes")(-200/600=0 "No")(else=.),gen(Underweight)
recode hw72(-600/-200=1 "Yes")(-200/600=0 "No")(else=.),gen(Wasting)

gen CNS=.
replace CNS=0 if Stunting==0 & Underweight==0 & Wasting==0
replace CNS=1 if Stunting==1 | Underweight==1 | Wasting==1

recode v445 (1200/1849=1 "Low")(1850/2499=2 "Normal")(2500/6000=3 "High")(9998 = .), gen (BMI)

recode v133 (0/5=0 "Non literate")(6/8=1 "6-8")(9/10=2 "9-10")(11/12=3 "11-12")(13/22=4 "More than 12"), gen(edu)

recode v190 (1/2=1 "Poor")(3=2 "Middle incomed")(4/5=3 "Rich"), gen (wi)

recode v201 (1=1 "Single child")(2=2 "2 children")(3=3 "3 children")(4=4 "4 children")(5=5 "5 children")(6/15=6 "more than 5 children"), gen(ceb)

recode m15 (11/13=1 "Home")(21/96=2 "Institutional"), gen(pod)

recode v130 (1=1 "Hindu")(2=2 "Muslim")(3=3 "Christian")(4/96=4 "Others"),gen(rel)

recode m19 (500/2495=1 "Low")(2500/4000=2 "Normal")(4001/9000=3 "High")(9996 9998 = .),gen(b_wgt)

replace h11 = 0 if h11 == 8 

recode m14 (0 = 0 "No")(1/30 = 1 "Yes")(98 = .), gen(pnv)

recode v116 (11=1)(12=2)(13=3)(22=4)(14=5)(15=6)(23=7)(31=8)(41=9)(44=10)(21=11),gen (TF)
recode TF (1/4=2 "Basic")(5/10 96=1 "Poor")(11=3 "Advance")(97 = .),gen (toil_faci)

recode v113 (32=1)(42=2)(51=3)(12=4)(61=5)(11=6)(21=7)(31=8)(41=9)(43=10)(62=11)(96=12)(13=13)(71=14)(72=15),gen(DWS)
recode DWS (1/5=1 "Poor")(6/12=2 "Adequate")(13/15=3 "Advance")(97 = .),gen(dri_wat_sou)

gen medhelp_prob=.
replace medhelp_prob=1 if v467d==1 & v467e==1 
replace medhelp_prob=2 if v467d==1 & v467e==0 | v467e==2
replace medhelp_prob=3 if v467d==0 | v467d==2 & v467e==1
replace medhelp_prob=4 if v467d==0 | v467d==2 & v467e==0 | v467e==2

recode medhelp_prob (1=1 "Both distance and transport are problems")(2=2 "Only distance problem")(3=3 "Only transport problem")(4=4 "No problem"), gen(DT_prob)
ta DT_prob [iw=wgt], m 

*recoding for exclusive bf
recode m4 (min/94 98 . = 0 "Not Curr BF")(95 = 1 "Still BF"), gen(recodedbf)
recode v409 (1=1 "Yes")(0 8 . = 0 "No"),gen(rv409)
recode v410 (1=1 "Yes")(0 8 . = 0 "No"),gen(rv410)
recode v412c (1=1 "Yes")(0 8 . = 0 "No"),gen(rv412c)
recode v413 (1=1 "Yes")(0 8 . = 0 "No"),gen(rv413)
recode v411 (1=1 "Yes")(0 8 . = 0 "No"),gen(rv411)
recode v411a (1=1 "Yes")(0 8 . = 0 "No"),gen(rv411a)
recode v412a (1=1 "Yes")(0 8 . = 0 "No"),gen(rv412a)
recode v414a (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414a)
recode v414e (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414e)
recode v414f (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414f)
recode v414g (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414g)
recode v414i (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414i)
recode v414j (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414j)
recode v414k (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414k)
recode v414l (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414l)
recode v414m (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414m)
recode v414n (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414n)
recode v414o (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414o)
recode v414p (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414p)
recode v414s (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414s)
recode v414t (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414t)
recode v414v (1=1 "Yes")(0 8 . = 0 "No"),gen(rv414v)

gen ebp1 = .
replace ebp1 = 1 if recodedbf == 1 & (rv409 == 0) & (rv410 == 0 | rv412c == 0 | rv413 == 0) & (rv411 == 0 | rv411a == 0) & (rv412a == 0 | rv414a == 0 | rv414e == 0 | rv414f == 0 | rv414g == 0 | rv414i == 0 | rv414j == 0 | rv414k == 0 | rv414l == 0 | rv414m == 0 | rv414n == 0 | rv414o == 0 | rv414p == 0 | rv414s == 0 | rv414t == 0 | rv414v == 0 | m39a == 0)
replace ebp1 = 0 if ebp1 == .

*Exclusive BFP <24 months
ta ebp1 [iw = wgt]

preserve
keep if aoc<6
*check exclusive bfp <6 months
ta ebp1 [iw = wgt]
restore

*predominant bf
*generate pdmnt_brstfd=0
*replace pdmnt_brstfd=1 if m4==95 & v409==1 | v410==1 | v412c==1 | v413==1 

*complimentary food
*gen comp_fd=0
*replace comp_fd=1 if m4==95 & v411==1 | v411a==1 | v412a==1 | v414a==1 | v414e==1 | v414f==1 | v414g==1 | v414i==1 | v414j==1 | v414k==1 | v414l==1 | v414m==1 | v414n==1 | v414o==1 | v414p==1 | v414s==1 | v414t==1 | v414v==1 | m39a==1

*good fit sem
sem (h11 -> Wasting, ) (h11 -> Underweight, ) (h11 -> Stunting, ) (b_wgt -> Wasting, ) (b_wgt -> Underweight, ) (b_wgt -> Stunting, ) (ebp1 -> Wasting, ) (ebp1 -> Underweight, ) (ebp1 -> Stunting, ) (ceb -> Wasting, ) (ceb -> Underweight, ) (ceb -> Stunting, ) (BMI -> Wasting, ) (BMI  -> Underweight, ) (BMI -> Stunting, ) (BMI -> b_wgt, ) (b4 -> Wasting, ) (b4 -> Underweight, ) (b4 -> Stunting, ) (b4 -> b_wgt, ) (b4 -> ebp1, ) (b4 -> ceb, ) (v012 -> Wasting, ) (v012 -> Underweight, ) (v012 -> Stunting, ) (v012 -> b_wgt, ) (v012 -> ceb, ) (wi -> Wasting, ) (wi -> Underweight, ) (wi -> Stunting, ) (wi -> ceb, ) (wi -> pnv, ) (edu -> Wasting, ) (edu -> Underweight, ) (edu -> Stunting, ) (edu -> b_wgt, ) (edu -> ebp1, ) (edu -> ceb, ) (edu -> pnv, ) (m10  -> Wasting, ) (m10 -> Underweight, ) (m10 -> Stunting, ) (m10 -> b_wgt, ) (m10 -> ceb, ) (pnv - > Wasting, ) (pnv -> Underweight, ) (pnv -> Stunting, ) (pnv -> ebp1, ), method(mlmv) standardized cov( e.Wasting*e.Underweight e.Wasting*e.Stunting e.Underweight*e.Stunting) nocapslatent
*goodness of fit 
estat gof, stats(all)
estat teffects, compact standardized

*other
estat residuals, standardized
estat mindices
estat eqgof
