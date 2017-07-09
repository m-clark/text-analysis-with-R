me_st_stem <- function(word) {
  st_list = c(
    "canst",
    "didst",
    "couldst",
    "mayst",
    "wouldst",
    "shouldst",
    "mightst",
    "lovedst",
    "livedst",
    "uttermost",
    "thou'ldst",
    "erst",
    "madst",
    "diedst",
    "contrivedst",
    "behavedst",
    "sayst",
    "smiledst",
    "criedst",
    "saidst",
    "followedst",
    "tost",
    "knowist",
    "likedst",
    "owedst",
    "knowst",
    "escapedst",
    "strokedst",
    "ldst"
  )
  if (word %in% st_list) {
    stringr::str_sub(word, end=-2)
  }
  else {
    word
  }
}


