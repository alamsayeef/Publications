*Converting in -1 0 1 format
gen iq11 = .
replace iq11 = -1 if q11 == 2
replace iq11 = -0 if q11 == 3
replace iq11 = 1 if q11 == 1
gen iq12 = .
replace iq12 = -1 if q12 == 1
replace iq12 = 0 if q12 == 3
replace iq12 = 1 if q12 == 2
gen iq13 = .
replace iq13 = -1 if q13 == 1
replace iq13 = 0 if q13 == 3
replace iq13 = 1 if  q13 == 2
gen iq14 = .
replace iq14 = -1 if q14 == 1
replace iq14 = 0 if  q14 == 3
replace iq14 = 1 if  q14 == 2
gen iq15 = .
replace iq15 = -1 if q15 == 1
replace iq15 = 0 if  q15 == 3
replace iq15 = 1 if  q15 == 2
gen iq16 = .
replace iq16 = -1 if q16 == 2
replace iq16 = 0 if  q16 == 3
replace iq16 = 1 if  q16 == 1
gen iq17 = .
replace iq17 = -1 if q17 == 1
replace iq17 = 0 if  q17 == 3
replace iq17 = 1 if  q17 == 2

*Creating Index
gen idx = (2*iq11) + (2*iq12) + iq13 + (2.5*iq14) + (2.5*iq15) + iq16 + (3*iq17) 

*Checking Reliability
alpha  iq11 iq12 iq13 iq14  iq15 iq16 iq17


by q1, sort : summarize idx
by q2, sort : summarize idx
by q3, sort : summarize idx
by q4, sort : summarize idx


*sayeef
gen iq11 = .
replace iq11 = -1 if feel == "Feel bad"
replace iq11 = 0 if feel == "No feelings"
replace iq11 = 1 if feel == "Feel good"
gen iq12 = .
replace iq12 = -1 if neg_eff == "Yes"
replace iq12 = 0 if neg_eff == "Sometimes"
replace iq12 = 1 if neg_eff == "No"
gen iq13 = .
replace iq13 = -1 if quit == "Yes"
replace iq13 = 0 if quit == "Sometimes"
replace iq13 = 1 if  quit == "No"
gen iq14 = .
replace iq14 = -1 if productive == "Yes"
replace iq14 = 0 if  productive == "Maybe"
replace iq14 = 1 if  productive == "No"
gen iq15 = .
replace iq15 = -1 if delay_work == "Yes"
replace iq15 = 0 if  delay_work == "Sometimes"
replace iq15 = 1 if  delay_work == "No"
gen iq16 = .
replace iq16 = -1 if conn_disconn == "Disconnected"
replace iq16 = 0 if  conn_disconn == "No change in relations"
replace iq16 = 1 if  conn_disconn == "Connected"
gen iq17 = .
replace iq17 = -1 if sleep_dep == "Yes"
replace iq17 = 0 if  sleep_dep == "Sometimes"
replace iq17 = 1 if  sleep_dep == "No"

*Creating Index
gen idx = (2*iq11) + (2*iq12) + iq13 + (2.5*iq14) + (2.5*iq15) + iq16 + (3*iq17) 

*Checking Reliability
alpha  iq11 iq12 iq13 iq14  iq15 iq16 iq17


by gender, sort : summarize idx
by age, sort : summarize idx
by proffesion, sort : summarize idx
by marr, sort : summarize idx
