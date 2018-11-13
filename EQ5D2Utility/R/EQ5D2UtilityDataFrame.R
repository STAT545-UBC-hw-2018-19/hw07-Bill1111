#' @title EQ5D2Utility_DataFrame
#' @param x The input is a Dataframe with EQ5D scores in column format with names MO, SC, UA, PD, AD
#' @return Column of Utility Values based on Canadian Value Set of EQ5D Index Scores
#' @export


Cad_EQ.5D_Utility_Dataframe <- function(y){

  y %>%
    select("MO","SC","UA","PD","AD") %>%
    mutate(U = apply(., MARGIN=1, Cad_EQ.5D_Utility))

}

Cad_EQ.5D_Utility_Dataframe
