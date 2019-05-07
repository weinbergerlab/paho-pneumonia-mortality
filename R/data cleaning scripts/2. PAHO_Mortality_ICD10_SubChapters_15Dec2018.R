################################################################################
# PAHO Mortality Data                                                          #
#                                                                              #
#        DATE: December 2018                                                   #
#    CODED BY: Kayoko Shioda (kayoko.shioda@yale.edu)                          #
#     ADVISOR: Dan Weinberger (daniel.weinberger@yale.edu)                     #
################################################################################

#------------------------------------------------------------------------------#
# DESCRIPTION
#------------------------------------------------------------------------------#

# 1. Create ICD-10 chapters and subchapters in the PAHO moratlity data
# 2. Create monthdate and quarterdate

#------------------------------------------------------------------------------#
# SET UP
#------------------------------------------------------------------------------#

rm(list = ls(all = TRUE))
setwd("~/Desktop/PAHO_Mortality_New/Data")
d <- read.csv("~/Desktop/PAHO_Mortality_New/Data/PAHO_10countries_IndividualLevel_15Dec2018.csv", as.is = T)


#------------------------------------------------------------------------------#
# ICD10 CODE
#------------------------------------------------------------------------------#

# Remove decimals and astariscs from ICD10 code
d$dx1 <- gsub("[.]", "", d$dx1)
d$dx2 <- gsub("[.]", "", d$dx2)
d$dx3 <- gsub("[.]", "", d$dx3)
d$dx4 <- gsub("[.]", "", d$dx4)
d$dx5 <- gsub("[.]", "", d$dx5)
d$dx6 <- gsub("[.]", "", d$dx6)
d$dx1 <- gsub("[*]", "", d$dx1)
d$dx2 <- gsub("[*]", "", d$dx2)
d$dx3 <- gsub("[*]", "", d$dx3)
d$dx4 <- gsub("[*]", "", d$dx4)
d$dx5 <- gsub("[*]", "", d$dx5)
d$dx6 <- gsub("[*]", "", d$dx6)
#head(d[d$country=="br",])

# Lower to upper case
d$dx1 <- toupper(d$dx1)
d$dx2 <- toupper(d$dx2)
d$dx3 <- toupper(d$dx3)
d$dx4 <- toupper(d$dx4)
d$dx5 <- toupper(d$dx5)
d$dx6 <- toupper(d$dx6)

# J chapter
table(d$dx1[substr(d$dx1, 1, 1)=="J"])
d$J09_J18_prim <- ifelse(substr(d$dx1, 1, 3) %in% c("J09","J10","J11","J12","J13","J14","J15","J16","J17","J18"), 1, 0)
d$J12_J18_prim <- ifelse(substr(d$dx1, 1, 3) %in% c("J12","J13","J14","J15","J16","J17","J18"), 1, 0)
d$J00_J99_prim <- ifelse(substr(d$dx1, 1, 1)=="J", 1, 0)
d$J13_prim <- ifelse(substr(d$dx1, 1, 3)=="J13", 1, 0)

# J12_J18_any: pneumococcal pneumonia or pneumonia with pneumococcus anywhere in Dxn
d$J12_J18_any <- ifelse(apply(d[,c("dx1","dx2","dx3","dx4","dx5","dx6")], 1,
                              function(x) sum(substr(x, 1, 3) %in% c("J12","J13","J14","J15","J16","J17","J18"))) > 0, 1, 0)

# J13_any: pneumococcal pneumonia or pneumonia with pneumococcus anywhere in Dxn
d$J13_any <- ifelse(apply(d[,c("dx1","dx2","dx3","dx4","dx5","dx6")], 1,
                          function(x) sum(substr(x, 1, 3) == "J13")) > 0, 1, 0)

# possible_pneumo_code: any pneumo code anywhere
d$possible_pneumo_code <- ifelse(d$J12_J18_any==1, 1, 0)
d$possible_pneumo_code <- ifelse(apply(d[,c("dx1","dx2","dx3","dx4","dx5","dx6")], 1,
                                       function(x) sum(substr(x, 1, 3) %in% c("A40","A49","B953","R652","H10","H65",
                                                                              "H66","G00","G01","G02","G03","G04"))) > 0, 
                                 1, d$possible_pneumo_code)
table(d$J12_J18_any)
table(d$possible_pneumo_code)

# acm_noj_prim
d$acm_noj_prim <- ifelse(c(substr(d$dx1, 1, 1)=="J" | d$possible_pneumo_code==1), 0, 1)
table(d$acm_noj_prim)

# acm_noj_nodiarr_prim <------------------- A00_A09 should also be excluded. Replace A08 with A00_A09.
d$acm_noj_nodiarr_prim <- ifelse(c(substr(d$dx1, 1, 1)=="J" | d$possible_pneumo_code==1), 0, 1)
d$acm_noj_nodiarr_prim <- ifelse(substr(d$dx1, 1, 3) =="A08", 0, d$acm_noj_nodiarr_prim)
table(noj = d$acm_noj_prim, noj_nodiarr = d$acm_noj_nodiarr_prim)
length(which(substr(d$dx1, 1, 3) =="A08"))

# Other chapters

### A & B ###

d$A00_B99_prim <- ifelse(c(substr(d$dx1, 1, 1) %in% c("A","B")), 1, 0)
d$A00_B99_prim <- ifelse(c(substr(d$dx1, 1, 2) =="A0" | d$possible_pneumo_code==1), 0, d$A00_B99_prim) # Exclude A00-A09
table(d$A00_B99_prim, d$possible_pneumo_code) # Good.

### A ###

table(d$dx1[substr(d$dx1, 1, 2)=="A0"])
d$A00_A09_prim <- ifelse(c(substr(d$dx1, 1, 2) =="A0"), 1, 0)
d$A00_A09_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A00_A09_prim) 
table(d$A00_A09_prim[substr(d$dx1, 1, 2)=="A0"], d$possible_pneumo_code[substr(d$dx1, 1, 2)=="A0"])

table(d$dx1[substr(d$dx1, 1, 2)=="A1"])
d$A15_A19_prim <- ifelse(c(substr(d$dx1, 1, 2) =="A1"), 1, 0)
d$A15_A19_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A15_A19_prim) 
table(d$dx1[substr(d$dx1, 1, 2)=="A1"], d$possible_pneumo_code[substr(d$dx1, 1, 2)=="A1"])
table(d$A15_A19_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2)=="A2"])
d$A20_A28_prim <- ifelse(c(substr(d$dx1, 1, 2) =="A2"), 1, 0)
d$A20_A28_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A20_A28_prim) 
table(d$dx1[substr(d$dx1, 1, 2)=="A2"], d$possible_pneumo_code[substr(d$dx1, 1, 2)=="A2"])
table(d$A20_A28_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2) %in% c("A3","A4")])
d$A30_49_prim <- ifelse(c(substr(d$dx1, 1, 2)  %in% c("A3","A4")), 1, 0)
d$A30_49_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A30_49_prim) 
table(d$A30_49_prim, d$possible_pneumo_code)

d$A41_prim <- ifelse(substr(d$dx1, 1, 3)=="A41", 1, 0)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("A50","A51","A52","A53","A54","A55","A56","A57","A58","A59",
                                       "A60","A61","A62","A63","A64")])
d$A50_A64_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("A50","A51","A52","A53","A54","A55","A56","A57","A58","A59",
                                                       "A60","A61","A62","A63","A64")), 1, 0)
d$A50_A64_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A50_A64_prim) 
table(d$A50_A64_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("A65","A66","A67","A68","A69")])
d$A65_A69_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("A65","A66","A67","A68","A69")), 1, 0)
d$A65_A69_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A65_A69_prim) 
table(d$A65_A69_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("A70","A71","A72","A73","A74")])
d$A70_A74_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("A70","A71","A72","A73","A74")), 1, 0)
d$A70_A74_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A70_A74_prim) 
table(d$A70_A74_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("A75","A76","A77","A78","A79")])
d$A75_A79_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("A75","A76","A77","A78","A79")), 1, 0)
d$A75_A79_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A75_A79_prim) 
table(d$A75_A79_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2)=="A8"])
d$A80_A89_prim <- ifelse(c(substr(d$dx1, 1, 2) =="A8"), 1, 0)
d$A80_A89_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A80_A89_prim) 
table(d$A80_A89_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2)=="A9"])
d$A09_A99_prim <- ifelse(c(substr(d$dx1, 1, 2) =="A9"), 1, 0)
d$A09_A99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$A09_A99_prim) 
table(d$A09_A99_prim, d$possible_pneumo_code)


### B ###

table(d$dx1[substr(d$dx1, 1, 2)=="B0"])
d$B00_B09_prim <- ifelse(c(substr(d$dx1, 1, 2) =="B0"), 1, 0)
d$B00_B09_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B00_B09_prim) 
table(d$B00_B09_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2)=="B1"])
d$B15_B19_prim <- ifelse(c(substr(d$dx1, 1, 2) =="B1"), 1, 0)
d$B15_B19_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B15_B19_prim) 
table(d$B15_B19_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B20","B21","B22","B23","B24")])
d$B20_B24_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("B20","B21","B22","B23","B24")), 1, 0)
d$B20_B24_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B20_B24_prim) 
table(d$B20_B24_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B25","B26","B27","B28","B29",
                                       "B30","B31","B32","B33","B34")])
d$B25_B34_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("B25","B26","B27","B28","B29",
                                                       "B30","B31","B32","B33","B34")), 1, 0)
d$B25_B34_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B25_B34_prim) 
table(d$B25_B34_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B35","B36","B37","B38","B39") | substr(d$dx1, 1, 2)=="B4"])
d$B35_B49_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("B35","B36","B37","B38","B39") | 
                             substr(d$dx1, 1, 2)=="B4"), 1, 0)
d$B35_B49_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B35_B49_prim) 
table(d$B35_B49_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B60","B61","B62","B63","B64") | substr(d$dx1, 1, 2)=="B5"])
d$B50_B64_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("B60","B61","B62","B63","B64") | substr(d$dx1, 1, 2)=="B5"), 1, 0)
d$B50_B64_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B50_B64_prim) 
table(d$B50_B64_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B65","B66","B67","B68","B69") | 
              substr(d$dx1, 1, 2)=="B7" |
              substr(d$dx1, 1, 3) %in% c("B80","B81","B82","B83")])
d$B65_B83_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("B65","B66","B67","B68","B69") | 
                             substr(d$dx1, 1, 2)=="B7" |
                             substr(d$dx1, 1, 3) %in% c("B80","B81","B82","B83")), 1, 0)
d$B65_B83_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B65_B83_prim) 
table(d$B65_B83_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B85","B86","B87","B88","B89")])
d$B85_B89_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("B85","B86","B87","B88","B89")), 1, 0)
d$B85_B89_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B85_B89_prim) 
table(d$B85_B89_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B90","B91","B92","B93","B94")])
d$B90_B94_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("B90","B91","B92","B93","B94")), 1, 0)
d$B90_B94_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B90_B94_prim) 
table(d$B90_B94_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("B95","B96","B97","B98")])
d$B95_B98_prim <- ifelse(c(substr(d$dx1, 1, 3)  %in% c("B95","B96","B97","B98")), 1, 0)
d$B95_B98_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B95_B98_prim) 
table(d$B95_B98_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3)=="B99"])
d$B99_B99_prim <- ifelse(c(substr(d$dx1, 1, 3) =="B99"), 1, 0)
d$B99_B99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$B99_B99_prim) 
table(d$B99_B99_prim, d$possible_pneumo_code)

### C & D ###

d$C00_D48_prim <- ifelse(c(substr(d$dx1, 1, 1) %in% c("C","D")), 1, 0)
d$C00_D48_prim <- ifelse(d$possible_pneumo_code==1, 0, d$C00_D48_prim) 

### C ###

table(d$dx1[substr(d$dx1, 1, 1)=="C"])
d$C00_C97_prim <- ifelse(c(substr(d$dx1, 1, 1) =="C"), 1, 0)
d$C00_C97_prim <- ifelse(d$possible_pneumo_code==1, 0, d$C00_C97_prim) 
table(d$C00_C97_prim, d$possible_pneumo_code)

### D ###

table(d$dx1[substr(d$dx1, 1, 2)=="D0"])
d$D00_D09_prim <- ifelse(c(substr(d$dx1, 1, 2) =="D0"), 1, 0)
d$D00_D09_prim <- ifelse(d$possible_pneumo_code==1, 0, d$D00_D09_prim) 
table(d$D00_D09_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2)=="D1" |
        substr(d$dx1, 1, 2)=="D2" |
        substr(d$dx1, 1, 3) %in% c("D30","D31","D32","D33","D34","D35","D36")])
d$D10_D36_prim <- ifelse(c(substr(d$dx1, 1, 2)=="D1" |
                             substr(d$dx1, 1, 2)=="D2" |
                             substr(d$dx1, 1, 3) %in% c("D30","D31","D32","D33","D34","D35","D36")), 1, 0)
d$D10_D36_prim <- ifelse(d$possible_pneumo_code==1, 0, d$D10_D36_prim) 
table(d$D10_D36_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 2)=="D4" |
              substr(d$dx1, 1, 3) %in% c("D37","D38","D39")])
d$D37_D48_prim <- ifelse(c(substr(d$dx1, 1, 2)=="D4" |
                             substr(d$dx1, 1, 3) %in% c("D37","D38","D39")), 1, 0)
d$D37_D48_prim <- ifelse(d$possible_pneumo_code==1, 0, d$D37_D48_prim) 
table(d$D37_D48_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("D50","D51","D52","D53")])
d$D50_D53_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("D50","D51","D52","D53")), 1, 0)
d$D50_D53_prim <- ifelse(d$possible_pneumo_code==1, 0, d$D50_D53_prim) 
table(d$D50_D53_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("D55","D56","D57","D58","D59")])
d$D55_D59_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("D55","D56","D57","D58","D59")), 1, 0)
d$D55_D59_prim <- ifelse(d$possible_pneumo_code==1, 0, d$D55_D59_prim) 
table(d$D55_D59_prim, d$possible_pneumo_code)

alph <- "D"
start <- 60
end <- 64
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$D60_D64_prim, d$possible_pneumo_code)

alph <- "D"
start <- 65
end <- 69
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$D65_D69_prim, d$possible_pneumo_code)

alph <- "D"
start <- 70
end <- 77
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$D70_D77_prim, d$possible_pneumo_code)

alph <- "D"
start <- 80
end <- 89
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$D80_D89_prim, d$possible_pneumo_code)

### E ###

d$E00_E90_prim <- ifelse(c(substr(d$dx1, 1, 1) =="E"), 1, 0)
d$E00_E90_prim <- ifelse(d$possible_pneumo_code==1, 0, d$E00_E90_prim)

alph <- "E"
start <- 0
end <- 7
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$E00_E07_prim, d$possible_pneumo_code)

start <- 10
end <- 14
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E10_E14_prim, d$possible_pneumo_code)

start <- 15
end <- 16
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E15_E16_prim, d$possible_pneumo_code)

start <- 20
end <- 35
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E20_E35_prim, d$possible_pneumo_code)

start <- 40
end <- 46
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E40_E46_prim, d$possible_pneumo_code)

start <- 50
end <- 64
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E50_E64_prim, d$possible_pneumo_code)

start <- 65
end <- 68
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E65_E68_prim, d$possible_pneumo_code)

start <- 70
end <- 90
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$E70_E90_prim, d$possible_pneumo_code)

### F ###

table(d$dx1[substr(d$dx1, 1, 1) =="F"])
d$F00_F99_prim <- ifelse(substr(d$dx1, 1, 1) =="F", 1, 0)
d$F00_F99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$F00_F99_prim) 
table(d$F00_F99_prim)
table(d$F00_F99_prim, d$possible_pneumo_code)


### G ###

d$G00_G99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="G"), 1, 0)
d$G00_G99_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("G01","G02","G03", "G04") | d$possible_pneumo_code==1),
                         0, d$G00_G99_prim) # exclude meningitis

alph <- "G"
start <- 0
end <- 9
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$G00_G09_prim, d$possible_pneumo_code)

d$G00_prim <- ifelse(substr(d$dx1, 1, 3)=="G00", 1, 0)

start <- 10
end <- 14
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$G10_G14_prim, d$possible_pneumo_code)

start <- 20
end <- 26
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")
table(d$G20_G26_prim, d$possible_pneumo_code)

start <- 30
end <- 32
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 35
end <- 37
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
end <- 47
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
end <- 59
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
end <- 64
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
end <- 73
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
end <- 83
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
end <- 99
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")


### H ###

d$H00_H59_prim <- ifelse(c(substr(d$dx1, 1, 2) %in% c("H0","H1","H2","H3","H4","H5")), 1, 0)
d$H00_H59_prim <- ifelse(c(substr(d$dx1, 1, 3) =="H10" | d$possible_pneumo_code==1), 0, d$H00_H59_prim) 

d$H60_H95_prim <- ifelse(c(substr(d$dx1, 1, 2) %in% c("H6","H7","H8","H9")), 1, 0)
d$H60_H95_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("H65", "H66") | d$possible_pneumo_code==1), 0, d$H60_H95_prim) 

alph <- "H"
start <- 0
end <- 6
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$H00_H06_prim, d$possible_pneumo_code)

start <- 10
end <- 13
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 15
end <- 22
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 25
end <- 28
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
end <- 36
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
end <- 42
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 43
end <- 45
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 46
end <- 48
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 49
end <- 52
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 53
end <- 54
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 55
end <- 59
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
end <- 62
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 65
end <- 75
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
end <- 83
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
end <- 95
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### I ###

d$I00_I99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="I"), 1, 0)
d$I00_I99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$I00_I99_prim)

alph <- "I"
start <- 0
end <- 2
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$I00_I02_prim, d$possible_pneumo_code)

start <- 5
end <- 9
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$I05_I09_prim, d$possible_pneumo_code)

start <- 10
end <- 15
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 25
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")


start <- 26
  end <- 28
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
  end <- 52
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
  end <- 69
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 79
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 89
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 95
  end <- 99
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")



### K ###

d$K00_K99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="K"), 1, 0)
d$K00_K99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$K00_K99_prim)

alph <- "K"
start1 <- 0
end1 <- 9
start2 <- 10
end2 <- 14
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
              substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
                    substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start1,"_",alph,end2,"_prim",sep="")
table(d$K00_K14_prim, d$possible_pneumo_code)

start <- 20
  end <- 31
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 35
  end <- 38
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 46
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
  end <- 52
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 55
  end <- 63
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 65
  end <- 67
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 77
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 87
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
  end <- 93
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")


### L ###

d$L00_L99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="L"), 1, 0)
d$L00_L99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$L00_L99_prim)

alph <- "L"
start <- 0
end <- 8
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$L00_L08_prim, d$possible_pneumo_code)

start <- 10
  end <- 14
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 30
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 45
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
  end <- 54
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 55
  end <- 59
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
  end <- 75
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 99
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")


### M ###

d$M00_M99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="M"), 1, 0)
d$M00_M99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$M00_M99_prim)

alph <- "M"
start1 <- 0
end1 <- 9
start2 <- 10
end2 <- 25
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
              substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
                    substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start1,"_",alph,end2,"_prim",sep="")
table(d$M00_M25_prim, d$possible_pneumo_code)

start <- 30
  end <- 36
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 54
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
  end <- 79
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 94
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 95
  end <- 99
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")


### N ###

d$N00_N99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="N"), 1, 0)
d$N00_N99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$N00_N99_prim)

alph <- "N"
start <- 0
end <- 8
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$N00_N08_prim, d$possible_pneumo_code)

start <- 10
  end <- 16
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 17
  end <- 19
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 23
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 25
  end <- 29
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
  end <- 39
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 51
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
  end <- 64
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 77
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 98
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### O ###

table(d$dx1[substr(d$dx1, 1, 1) =="O"])
d$O00_O99_prim <- ifelse(substr(d$dx1, 1, 1) =="O", 1, 0)
d$O00_O99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$O00_O99_prim) 
table(d$O00_O99_prim)
table(d$O00_O99_prim, d$possible_pneumo_code)


### P ###

d$P00_P96_prim <- ifelse(c(substr(d$dx1, 1, 1) =="P"), 1, 0)
d$P00_P96_prim <- ifelse(d$possible_pneumo_code==1, 0, d$P00_P96_prim)

alph <- "P"
start <- 0
end <- 4
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$P00_P04_prim, d$possible_pneumo_code)

start <- 5
end <- 8
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")

start <- 10
  end <- 15
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 29
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 35
  end <- 39
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
  end <- 61
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 74
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 75
  end <- 78
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 83
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
  end <- 96
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### Q ###

d$Q00_Q99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="Q"), 1, 0)
d$Q00_Q99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$Q00_Q99_prim)

alph <- "Q"
start <- 0
end <- 7
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$Q00_Q07_prim, d$possible_pneumo_code)

start <- 10
  end <- 18
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 28
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
  end <- 34
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 35
  end <- 37
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 38
  end <- 45
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
  end <- 56
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
  end <- 64
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 65
  end <- 79
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
end <- 89
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
end <- 99
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### R ###

d$R00_R99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="R"), 1, 0)
d$R00_R99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$R00_R99_prim)

alph <- "R"
start <- 0
end <- 9
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$R00_R09_prim, d$possible_pneumo_code)

start <- 10
  end <- 19
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 23
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 25
  end <- 29
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
  end <- 39
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 46
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 47
  end <- 49
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
  end <- 69
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 79
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 82
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 83
  end <- 89
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
  end <- 94
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 95
end <- 99
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### S & T ###

d$S00_T98_prim <- ifelse(c(substr(d$dx1, 1, 1) %in% c("S", "T")), 1, 0)
d$S00_T98_prim <- ifelse(d$possible_pneumo_code==1, 0, d$S00_T98_prim) 

### S ###

alph <- "S"
start <- 0
end <- 9
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$S00_S09_prim, d$possible_pneumo_code)

start <- 10
  end <- 19
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 29
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
  end <- 39
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 49
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 50
  end <- 59
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 60
  end <- 69
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 79
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 89
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
  end <- 99
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")



### T ###

alph <- "T"
start <- 0
end <- 7
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start,"_",alph,"0",end,"_prim",sep="")
table(d$T00_T07_prim, d$possible_pneumo_code)

table(d$dx1[substr(d$dx1, 1, 3) %in% c("T08","T09","T10","T11","T12","T13","T14")])
d$T08_T14_prim <- ifelse(c(substr(d$dx1, 1, 3) %in% c("T08","T09","T10","T11","T12","T13","T14")), 1, 0)
d$T08_T14_prim <- ifelse(d$possible_pneumo_code==1, 0, d$T08_T14_prim) 

start <- 15
  end <- 19
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 20
  end <- 32
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 33
  end <- 35
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 36
  end <- 50
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 51
  end <- 65
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 66
  end <- 78
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

table(d$dx1[substr(d$dx1, 1, 3)=="T79"])
d$T79_T79_prim <- ifelse(c(substr(d$dx1, 1, 3) =="T79"), 1, 0)
d$T79_T79_prim <- ifelse(d$possible_pneumo_code==1, 0, d$T79_T79_prim) 

start <- 80
  end <- 88
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 90
  end <- 98
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### V, W, X ###

alph <- "X"
start1 <- 0
end1 <- 9
start2 <- 10
end2 <- 59
table(d$dx1[substr(d$dx1, 1, 1) %in% c("V", "W") |
              substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
              substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 1) %in% c("V", "W") |
                    substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
                    substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- "V01_X59_prim"
length(d$dx1[substr(d$dx1, 1, 1) %in% c("V", "W") |
               substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
               substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")])
table(d$V01_X59_prim)

### X ### 

alph <- "X"
start <- 60
end <- 84
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### X & Y ###

alph1 <- "X"
start1 <- 0
end1 <- 9
alph2 <- "Y"
start2 <- 85
end2 <- 99
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph1,seq(start2,end2,1),sep="") |
              substr(d$dx1, 1, 3) %in% paste(alph2,"0",seq(start1,end1,1),sep="") ])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph1,seq(start2,end2,1),sep="") |
                    substr(d$dx1, 1, 3) %in% paste(alph2,"0",seq(start1,end1,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- "X85_Y09_prim"
table(d$X85_Y09_prim)

### V & Y ###

d$V00_Y98_prim <- ifelse(c(substr(d$dx1, 1, 1) %in% c("V", 'W',"Y")), 1, 0)
d$V00_Y98_prim <- ifelse(d$possible_pneumo_code==1, 0, d$V00_Y98_prim) 

### Y ###

alph <- "Y"
start <- 10
end <- 99
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

### Z ###

d$Z00_Z99_prim <- ifelse(c(substr(d$dx1, 1, 1) =="Z"), 1, 0)
d$Z00_Z99_prim <- ifelse(d$possible_pneumo_code==1, 0, d$Z00_Z99_prim)

alph <- "Z"
start1 <- 0
end1 <- 9
start2 <- 10
end2 <- 13
table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
              substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,"0",seq(start1,end1,1),sep="") |
                    substr(d$dx1, 1, 3) %in% paste(alph,seq(start2,end2,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
table(d$new)
table(d$new, d$possible_pneumo_code)
colnames(d)[which(colnames(d)=="new")] <- paste(alph,"0",start1,"_",alph,end2,"_prim",sep="")
table(d$Z00_Z13_prim, d$possible_pneumo_code)

start <- 20
  end <- 29
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 30
  end <- 39
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 40
  end <- 54
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 55
  end <- 65
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 70
  end <- 76
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")

start <- 80
  end <- 99
  table(d$dx1[substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")])
d$new <- ifelse(c(substr(d$dx1, 1, 3) %in% paste(alph,seq(start,end,1),sep="")), 1, 0)
d$new <- ifelse(d$possible_pneumo_code==1, 0, d$new) 
colnames(d)[which(colnames(d)=="new")] <- paste(alph,start,"_",alph,end,"_prim",sep="")


#------------------------------------------------------------------------------#
# Date
#------------------------------------------------------------------------------#

# Create monthdate (YYYY-MM-01 for deaths)
d$monthdate <- as.Date(paste(d$Year, d$Month, 1, sep='-'))

# Create quarterdate (YYYY-MM-01 for deaths)
library(zoo)
d$quarter <- as.yearqtr(d$monthdate, format = "%Y-%m-%d")
d$monthq <- NA
d$monthq <- ifelse(substr(d$quarter,6,7)=="Q1", 1,  d$monthq)
d$monthq <- ifelse(substr(d$quarter,6,7)=="Q2", 4,  d$monthq)
d$monthq <- ifelse(substr(d$quarter,6,7)=="Q3", 7,  d$monthq)
d$monthq <- ifelse(substr(d$quarter,6,7)=="Q4", 10, d$monthq)
d$quarterdate <- as.Date(paste(d$Year, d$monthq, 1, sep='-'))
table(d$quarterdate)

# Remove records if yaer of death is <2000 or >=2016
# d <- d[which(d$Year >= 2000 & d$Year < 2016), ]


# Save
setwd("~/Desktop/PAHO_Mortality_New/Data")
write.csv(d, "PAHO_10countries_IndividualLevel_15Dec2018_ICD10reformatted_subchapters.csv", row.names = F)

