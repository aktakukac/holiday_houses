################################# HEADER #########################

## Script created by Mihaly Garamvolgyi
## 2016/01/14
## R version 3.1.2 (2014-10-31) Pumpkin Helmet

# used for cleaning ads downloaded from ingatlan.com
# uses dplyr and sringr

################################# HEADER #########################

# Automatikus package telepítés
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# packages betöltése
packages(dplyr)
packages(stringr)


# set working directory
setwd("C:/Misi/Webscraping")

hirdetesek <- read.table("data_20160114.csv", header=TRUE, sep=",", fileEncoding="UTF-8")

telepules_szoveg <- hirdetesek$Telepules

# exlude unnecessary strings
telepules_szoveg <- str_replace_all(telepules_szoveg, "Áron alul!", "")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Csak nálunk", "")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Vízparti", "")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Árcsökkenés", "")

# exlude anything between brackets
telepules_szoveg <- str_replace_all(telepules_szoveg, "\\((.*?)\\)", "")

# exclude other special characters
telepules_szoveg <- str_replace_all(telepules_szoveg, "é", "e")
telepules_szoveg <- str_replace_all(telepules_szoveg, "É", "E")
telepules_szoveg <- str_replace_all(telepules_szoveg, "á", "a")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Á", "A")
telepules_szoveg <- str_replace_all(telepules_szoveg, "ü", "u")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Ü", "U")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Õ", "O")
telepules_szoveg <- str_replace_all(telepules_szoveg, "õ", "o")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Ú", "U")
telepules_szoveg <- str_replace_all(telepules_szoveg, "ú", "u")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Ö", "O")
telepules_szoveg <- str_replace_all(telepules_szoveg, "ö", "o")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Ü", "U")
telepules_szoveg <- str_replace_all(telepules_szoveg, "ü", "u")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Ó", "O")
telepules_szoveg <- str_replace_all(telepules_szoveg, "ó", "o")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Í", "I")
telepules_szoveg <- str_replace_all(telepules_szoveg, "í", "i")
telepules_szoveg <- str_replace_all(telepules_szoveg, "Û", "U")
telepules_szoveg <- str_replace_all(telepules_szoveg, "û", "u")

split_vector_betu <- regexpr("[a-z]{1}[A-Z]{1}", telepules_szoveg)
split_vector_szam <- regexpr("[0-9]{1}[A-Z]{1}", telepules_szoveg)
split_vector_pont <- regexpr("[.]{1}[A-Z]{1}", telepules_szoveg)

split_1 <- ifelse(split_vector_pont>-1, 
                  str_sub(telepules_szoveg, start=1, end=split_vector_pont),
                  ifelse(split_vector_szam>-1,
                         str_sub(telepules_szoveg, start=1, end=split_vector_szam),
                         ifelse(split_vector_betu>-1,
                                str_sub(telepules_szoveg, start=1, end=split_vector_betu),
                                ""
                                )
                         )
                  )


split_2 <- ifelse(split_vector_pont>-1, 
                  str_sub(telepules_szoveg, start=split_vector_pont+1, end=length(telepules_szoveg)),
                  ifelse(split_vector_szam>-1,
                         str_sub(telepules_szoveg, start=split_vector_szam+1, end=length(telepules_szoveg)),
                         ifelse(split_vector_betu>-1,
                                str_sub(telepules_szoveg, start=split_vector_betu+1, end=length(telepules_szoveg)),
                                ""
                         )
                  )
)

# split_1 <- ifelse(split_vector>-1, str_sub(telepules_szoveg, start=1, end=split_vector), "")
# split_2 <- ifelse(split_vector>-1, str_sub(telepules_szoveg, start=split_vector+1, end=length(telepules_szoveg)), "")

# concatenate the addresses
hirdetesek_cim <- paste(split_1, split_2, sep=" ")  

hirdetesek_cim <- str_replace_all(hirdetesek_cim, "[-]", "")
hirdetesek_cim <- str_replace_all(hirdetesek_cim, "[.]", "")
hirdetesek_cim <- str_replace_all(hirdetesek_cim, "[...]", "")
hirdetesek_cim <- str_replace_all(hirdetesek_cim, "[,]", "")
hirdetesek_cim <- str_replace_all(hirdetesek_cim, "\\n", "")
hirdetesek_cim <- str_replace_all(hirdetesek_cim, "[–]", "")


hirdetesek_final <- cbind(hirdetesek, hirdetesek_cim)

# prices
hirdetes_ar <- hirdetesek$Ar

split_vector_ar <- regexpr("M Ft", hirdetes_ar)

hirdetes_ar <- as.numeric(ifelse(split_vector_ar>-1, str_sub(hirdetes_ar, start=1, end=split_vector_ar-2), ""))

hirdetesek_final <- cbind(hirdetesek_final, hirdetes_ar)


# size of lot
hirdetes_telekmeret <- hirdetesek$Telekmeret

hirdetes_telekmeret <- str_replace_all(hirdetes_telekmeret, "\\s", "")

split_vector_telek <- regexpr("m2", hirdetes_telekmeret)

hirdetes_telekmeret <- as.numeric(str_sub(hirdetes_telekmeret, start=1, end=split_vector_telek-1))

hirdetesek_final <- cbind(hirdetesek_final, hirdetes_telekmeret)

# size of house
hirdetes_ingatlanmeret <- hirdetesek$Ingatlanmeret

hirdetes_ingatlanmeret <- str_replace_all(hirdetes_ingatlanmeret, "\\s", "")

split_vector_ingatlan <- regexpr("m2", hirdetes_ingatlanmeret)

hirdetes_ingatlanmeret <- as.numeric(str_sub(hirdetes_ingatlanmeret, start=1, end=split_vector_ingatlan-1))

hirdetesek_final <- cbind(hirdetesek_final, hirdetes_ingatlanmeret)

# exclude old columns
exclude_columns <- names(hirdetesek_final) %in% c("X", "Ar", "Ingatlanmeret",  "Szobaszam", "Telekmeret", "Telepules", "Ures")

hirdetesek_final <- hirdetesek_final[!exclude_columns]

write.csv(hirdetesek_final, file = "cleaned_data_20160114.csv")

print("Finished!")
