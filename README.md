# meld R Package

**Coalescing Data Columns Except When Unequal**

This package handles one particular use case. In a data frame,
  if you want to merge values from two columns under the assumption the data
  is either identical or missing. This functions melds the two columns together,
  but will stop if the values are not identical (unless the value is NA). In this
  sense, it's a "safe" coalesce because you cannot combine two columns with different
  values in the columns. When using the dplyr::coalesce function, it does not tell
  you if the columns are not the same. Instead it uses the first value. This package 
  implements meld, which does a coalesce except it aborts when the values being merged
  are not the same (or NA). 
