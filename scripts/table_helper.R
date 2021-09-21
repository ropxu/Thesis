library(stargazer)
library(stringr)
# Helper function to fit stargazer table to textwidth and change t-stat formating
fitStargazer <- function(tbl, file){
  tbl <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", tbl, fixed = T)
  tbl <- gsub("\\end{tabular}", "\\end{tabular}}", tbl, fixed = T)
  tbl <- str_replace_all(tbl, '(?:t = |t = \\$-\\$)(\\d\\.\\d{3})', '(\\1)')
  
  fileConn <- file(file)
  writeLines(tbl, fileConn)
  close(fileConn)
}


str_replace('t = 0.002', 't =(\\d\\.\\d{3})', '(\\1)')
