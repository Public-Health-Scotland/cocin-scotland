# Functions from James Hardy via Chris Robertson

#' Function to identify calls related to cold or flu symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunColdFlu <- function(x) {
  z1 <- regexpr("COLD", x, ignore.case = T) # cold sore, cold leg and hands, hot & cold
  z2 <- regexpr("SORE", x, ignore.case = T)
  z3 <- regexpr("HOT", x, ignore.case = T)
  z4 <- regexpr("LEG", x, ignore.case = T)
  z5 <- regexpr("HAND", x, ignore.case = T) # cold feet generally mentioned with hands so not searching for both
  z7 <- regexpr("COLD SORE", x, ignore.case = T)
  z8 <- regexpr("COLDSORE", x, ignore.case = T)
  z9 <- regexpr("SHIVER", x, ignore.case = T)
  z10 <- regexpr("FLU", x, ignore.case = T)
  z11 <- regexpr("[A-Z]FLU", x, ignore.case = T) # highlights reflux
  z12 <- regexpr("FLU[A-Z]", x, ignore.case = T) # highlights fluid, flushed, flutter, fluctuating
  z13 <- regexpr("SWEAT", x, ignore.case = T)
  z14 <- regexpr("CLAMY", x, ignore.case = T)
  z15 <- regexpr("VACCINE", x, ignore.case = T) # 07/11/2012

  return(ifelse((z1 > 0 & z3 == -1) &
    (z1 > 0 & z13 == -1) &
    (z1 > 0 & z14 == -1) &
    (z1 > 0 & z4 == -1) &
    (z1 > 0 & z5 == -1) &
    (z1 > 0 & z8 == -1) &
    (z1 > 0 & z9 == -1) |
    ((z10 > 0 & z11 == -1) &
      (z10 > 0 & z12 == -1) &
      (z10 > 0 & z15 == -1)),
  1, 0
  ))
}


#' Function to identify calls related to cough symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunCoughs <- function(x) {
  z1 <- regexpr("(?<!NO )COUGH", x, ignore.case = T, perl = TRUE) # are there any cough cases which should be excluded?

  return(ifelse(z1 > 0, 1, 0))
}

#' Function to identify calls related to fever symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunFever <- function(x) {
  z1 <- regexpr("(?<!NO )FEVER", x, ignore.case = T, perl = TRUE)
  z2 <- regexpr("(?<!NO )TEMP", x, ignore.case = T, perl = TRUE)
  z3 <- regexpr("[A-Z]TEMP", x, ignore.case = T) # highlighted HIGHTEMP, ATTEMPTED
  z4 <- regexpr("ATTEMPT", x, ignore.case = T)

  return(ifelse((z1 > 0 | z2 > 0) &
    z4 == -1, 1, 0)) # fever, temp, excluded attempted (suicide)
}

#' Function to identify calls related to difficulty breathing symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunDiffBreath <- function(x) {
  z1 <- regexpr("BREATH", x, ignore.case = T)
  z2 <- regexpr("WHEEZ", x, ignore.case = T) # 683 cases, 160 with "BREATH" keyword
  z3 <- regexpr("ASTHMA ATTACK", x, ignore.case = T)

  return(ifelse((z1 > 0 | z2 > 0) &
    z3 == -1, 1, 0)) # exclude asthma attack cases. Is it correct to do this?
}

#' Function to identify calls related to vomiting symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunVomit <- function(x) {
  z1 <- regexpr("VOM", x, ignore.case = T)
  z2 <- regexpr("SICK", x, ignore.case = T)
  z3 <- regexpr("NAUSEA", x, ignore.case = T)
  z4 <- regexpr("BLOOD", x, ignore.case = T)
  z5 <- regexpr("PREGNANT", x, ignore.case = T)

  return(ifelse(((z1 > 0 | z2 > 0 | z3 > 0) & (z4 == -1)) &
    ((z1 > 0 | z2 > 0 | z3 > 0) & (z5 == -1)),
  1, 0
  ))
}

#' Function to identify calls related to diarrhoea symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunDiarrhoea <- function(x) {
  z1 <- regexpr("DIAR", x, ignore.case = T) # think that this may be sufficient
  z2 <- regexpr("DIOR", x, ignore.case = T) # 10 cases

  return(ifelse(z1 > 0 | z2 > 0, 1, 0))
}

#' Function to identify calls related to loss/change of taste or smell symptoms.
#'
#' @param x A string of text (character)
#'
#' @return Logical
FunTasteSmell <- function(x) {
  z1 <- regexpr("SMELL", x, ignore.case = T)
  z2 <- regexpr("TASTE", x, ignore.case = T)
  z3 <- regexpr("NO LOSS", x, ignore.case = T)

  return(ifelse((z1 > 0 | z2 > 0) & z3 == -1, 1, 0))
}
