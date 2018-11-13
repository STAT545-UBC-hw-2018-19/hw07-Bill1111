#' @title EQ5D2Utility
#' @param x The input is a numeric vector of length 5
#' @return Utility Value based on Canadian Value Set of EQ5D Index Scores
#' @export

Cad_EQ.5D_Utility <- function(x) {

  MO <- x[1]
  SC <- x[2]
  UA <- x[3]
  PD <- x[4]
  AD <- x[5]

  part1 <- 1.1351 + (MO)*(-0.0389) + (SC)*(-0.0458) + (UA)*(-0.0195) + (PD)*(-0.0444) + (AD)*(-0.0376)

  part2 <- ifelse(MO>3, -0.051,0) +
    ifelse(SC>3, -0.0584,0) +
    ifelse(UA>3, -0.1103,0) +
    ifelse(PD>3, -0.1409,0) +
    ifelse(AD>3, -0.1277,0)

  y <-     x1 <- length(which(MO>3))
  x2 <- length(which(SC>3))
  x3 <- length(which(UA>3))
  x4 <- length(which(PD>3))
  x5 <- length(which(AD>3))

  y <- x1 + x2 + x3 + x4 + x5

  part3 <- (y-1)^2 * 0.0085

  all <- part1 + part2 + part3
  return(all)
}
