#' General tools used across Porject 

find_names_matching <- function(col_names, pattern, indices = F, use_perl = F, ignore_case = F){
  if(indices){
    return(grep(pattern, col_names, ignore.case = ignore_case, perl = use_perl))
  }else{
    return(col_names[grep(pattern, col_names, ignore.case = ignore_case, perl = use_perl)])
  }
  
}