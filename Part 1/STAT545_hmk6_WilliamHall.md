Homework6
================
William Hall
2018-11-01

``` r
#install.packages("expss")
#install.packages("purrr")
library(expss)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:expss':
    ## 
    ##     between, compute, first, last, lst, na_if, recode, vars

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ readr   1.1.1
    ## ✔ tibble  1.4.2     ✔ purrr   0.2.5
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ ggplot2 3.0.0     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks expss::between()
    ## ✖ dplyr::compute()   masks expss::compute()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks expss::first()
    ## ✖ stringr::fixed()   masks expss::fixed()
    ## ✖ purrr::keep()      masks expss::keep()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks expss::last()
    ## ✖ tibble::lst()      masks dplyr::lst(), expss::lst()
    ## ✖ purrr::modify()    masks expss::modify()
    ## ✖ purrr::modify_if() masks expss::modify_if()
    ## ✖ dplyr::na_if()     masks expss::na_if()
    ## ✖ tidyr::nest()      masks expss::nest()
    ## ✖ dplyr::recode()    masks expss::recode()
    ## ✖ stringr::regex()   masks expss::regex()
    ## ✖ purrr::transpose() masks expss::transpose()
    ## ✖ ggplot2::vars()    masks dplyr::vars(), expss::vars()

``` r
library(purrr)
```

In this homework assignment, we had to choose two of six options. The first options I chose was Option \#2: Writing functions. It reads:

"Write one (or more) functions that do something useful to pieces of the Gapminder or Singer data. It is logical to think about computing on the mini-data frames corresponding to the data for each specific country, location, year, band, album, … This would pair well with the prompt below about working with a nested data frame, as you could apply your function there."

"Make it something you can’t easily do with built-in functions. Make it something that’s not trivial to do with the simple dplyr verbs. The linear regression function presented here is a good starting point. You could generalize that to do quadratic regression (include a squared term) or use robust regression, using MASS::rlm() or robustbase::lmrob()."

I have actually chosen to create a function that will be very useful to me in my research as a Health Economics. In Health Economic Evaluation, we compare the costs and benefits of a new intervention compared to the current standard of care. Benefits are measured using a scale between 1 (full health) and 0 (death). Someone's 'utility' is where they are on that scale between 0 and 1.

To calculate someone's utility, we use a survey instrument called the EQ-5D. The EQ-5D gives a series of numbers between 1 and 5 for 5 dimensions of care. These 'index scores' must then be converted into score between 0 and 1 on that scale using an algorithm - more information available here: \#\#give link to Nick's paper\#\#

For this assignment, I will create a function that will convert index scores into utilies - and make a loop that does this for multiple lines for multiple patients in a dataset. We will use the following paper by Xie et al. as our guide: <https://www.ncbi.nlm.nih.gov/pubmed/26492214>

In their paper, Xie et al. describe four models to calculate utilities from index scores. We will use the fourth model they developed since it has the least MAE - mean absolute error, and MSE - mean squared error. There are three steps to this model that will be described in the following code.

Step One: Linear
================

The first step of calculating the utility uses a simple linear regression. The intercept is 1.1351, the coefficient for 'Mobility' is -0.0389, the coefficient for 'Self-Care' is -0.0458, the coefficient for 'Usual Activities' is -0.0195, the coefficient for 'Pain/Discomfort' is -0.0444, and the coefficient for 'Anxiety/Depression' is -0.0376.

The following code creates a function that will calculate sum product utility from these scores. I have commented them all out since this is included in the final function.

``` r
#patient1.EQ5D.IndexScore <- c(2,3,1,4,5)
#
#Cad_EQ.5D_part_1 <- function(x) {
#  MO <- x[1]
#  SC <- x[2]
#  UA <- x[3]
#  PD <- x[4]
#  AD <- x[5]
#  
#  x11 <- 1.1351 + (MO)*(-0.0389) + (SC)*(-0.0458) + (UA)*(-0.0195) + (PD)*(-0.0444) + #(AD)*(-0.0376)
#  x11
#}
#
#Cad_EQ.5D_part_1(2,3,1,4,5)
#Cad_EQ.5D_part_1(patient1.EQ5D.IndexScore)
```

Step Two: 4,5
=============

Unfortunately, this relatively simplistic model is not sufficient. In the paper, Xie et al. observe that "given the impact between the dimension levels, this model was not considered a reasonable reflection of the data." And in fact "the utility decrement associated with level 4 and 5 was disproportionally higher than that associated with the lower levels". Therefore, five additional terms were added to account of this larger impact.

If the score was higher than level 3 (i.e. a 4 or 5), then the following coeffiecients were applied. It must be noted that these variables are dichotomous - therefore, if the level is higher than 3, the coefficient is applied - as opposed to multiplying the level by the coefficient.

See code below. Again, this is all commented out and included in the final version of the function.

``` r
#patient1.EQ5D.IndexScore <- c(2,3,1,4,5)
#
#Cad_EQ.5D_part_2 <- function(x) {
#  MO <- x[1]
#  SC <- x[2]
#  UA <- x[3]
#  PD <- x[4]
#  AD <- x[5]
#  
#  x22 <- ifelse(MO>3, -0.051,0) +
#       ifelse(SC>3, -0.0584,0) + 
#       ifelse(UA>3, -0.1103,0) +
#       ifelse(PD>3, -0.1409,0) +
#       ifelse(AD>3, -0.1277,0)
#  x22
#}
#
#Cad_EQ.5D_part_2(2,3,1,4,5)
#Cad_EQ.5D_part_2(patient1.EQ5D.IndexScore)
```

Step Three: 4,5 Squared
=======================

Although the addition of this second component to the model improves its performance, the disutility might still be overesitmated. Xie et al. asserted that "it is reasonable to expect there might be diminishing marginal effect on the magnitude of the overestimate from the five 45 terms". Therefore, a 'Num45sq' term was added to improve the model fit.

This final variable is perhaps the most complicated to calculate. First, the number of 4s and 5s in the index scores must be calculated. There must be at least one 4 or 5 in order for the 0.0085 terms to be introduced. The equation is in fact (count(4,5)-1)^2\*0.0085. This is reflected in the code below

``` r
#patient1.EQ5D.IndexScore <- c(3,2,5,5,5)
#
#Cad_EQ.5D_part_3 <- function(x) {
#  MO <- x[1]
#  SC <- x[2]
#  UA <- x[3]
#  PD <- x[4]
#  AD <- x[5]
#  
#  x1 <- length(which(MO>3))
#  x2 <- length(which(SC>3))
#  x3 <- length(which(UA>3))
#  x4 <- length(which(PD>3))
#  x5 <- length(which(AD>3))
#  
#  y <- x1 + x2 + x3 + x4 + x5
#  
#  z <- (y-1)^2 * 0.0085
#  
#  return(z)
#}
#
#         
#Cad_EQ.5D_part_3(3,2,5,5,5)
#Cad_EQ.5D_part_3(patient1.EQ5D.IndexScore)
```

Step Four: 4,5 Squared
======================

So now that we have all three parts, let's put them together into one final function.

``` r
# patient1.EQ5D.IndexScore <- c(3,4,1,1,4) - this is for testing purposes

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


# Cad_EQ.5D_Utility2(2,3,1,4,5)               # for testing
# Cad_EQ.5D_Utility(patient1.EQ5D.IndexScore) # for testing
```

Perfect. Now that we have the code working, we need to upload some data and create a for loop that creates a new column.

``` r
library(readxl)
EQ5D_Data <- read_excel("EQ5D_Data.xlsx")
#View(EQ5D_Data)

names(EQ5D_Data) <- c("Study_ID", "MO", "SC", "UA", "PD", "AD")
#View(EQ5D_Data)
```

Now let's apply the function and see if it works.

``` r
EQ5D_Data_Utility <- EQ5D_Data %>% 
                        select("MO","SC","UA","PD","AD") %>%
                        mutate(U = apply(., MARGIN=1, Cad_EQ.5D_Utility))

# View(EQ5D_Data_Utility)
```

It worked! We have created a new column with the Utilities between 0 and 1 based on the EQ 5D index scores of each patient :)
