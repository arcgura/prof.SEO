library(dplyr)

data <- read.table("/Users/jd5/Dropbox/R/seo/data.csv", header = TRUE , sep = ",", na.strings=c("NA","","."), stringsAsFactors = FALSE)

umls <- read.table("/Users/jd5/Dropbox/R/seo/umls.txt", header = TRUE, sep="\t", quote = "", strip.white = TRUE, na.strings=c("NA","","."), stringsAsFactors = FALSE)

kcdc <- read.csv("/Users/jd5/Dropbox/R/seo/kcdc7.csv", header = TRUE)



# umls(주증상) 기준 burn data
burn_umls <- umls[grepl("burn", umls$english) | grepl("Burn", umls$english) | grepl("BURN", umls$english), ]
burn_umls <- burn_umls[!grepl("burning", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("Burning", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("Heartburn", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("alopecia", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("contracture", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("scar", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("ABDOMINAL", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("Mc", burn_umls$english), ]

burn_umls_code <- burn_umls$CODE

burndata_umls <- filter(data, 주증상.1 %in% burn_umls_code | 주증상.2 %in% burn_umls_code | 주증상.3 %in% burn_umls_code)

#kcdc7(진단코드) 기준 burn data
burn_kcdc <- kcdc[grepl("burn", kcdc$english) | grepl("Burn", kcdc$english) | grepl("BURN", kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("due", burn_kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("M", burn_kcdc$code), ]
burn_kcdc <- burn_kcdc[!grepl("Sequelae", burn_kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("Z", burn_kcdc$code), ]
burn_kcdc <- burn_kcdc[!grepl("R", burn_kcdc$code), ]

burn_kcdc_code <- burn_kcdc$code

str(data)
burndata_kcdc <- filter(data, 응급환자퇴실진단코드01 %in% burn_kcdc_code | 응급환자퇴실진단코드02 %in% burn_kcdc_code | 응급환자퇴실진단코드03 %in% burn_kcdc_code )


# find unmatching data
burndata_umls_unmatch <- filter(burndata_umls, !(응급환자퇴실진단코드01 %in% burn_kcdc_code) )
burndata_kcdc_unmatch <- filter(burndata_kcdc, !(주증상.1 %in% burn_umls_code))



#merge two dataset
burndata <- merge(burndata_kcdc, burndata_umls, all = TRUE)




