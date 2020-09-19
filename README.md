
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volleysim

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![openvolley](https://img.shields.io/badge/openvolley-darkblue.svg?logo=data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiNmZmYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+)
<!-- badges: end -->

## Installation

``` r
## install.packages("remotes")
remotes::install_github("scienceuntangled/volleysim")
```

## Example 1

``` r
library(volleysim)
library(datavolley)

## read an example file
x <- dv_read(dv_example_file())

## calculate the rates we need to simulate
rates <- list(vs_estimate_rates(x, target_team = home_team(x)),
              vs_estimate_rates(x, target_team = visiting_team(x)))

vs_simulate_match(rates, simple = TRUE)
#> $pwin
#> [1] 0.9897465
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.7424468
#> 
#> $scores$`3-1`
#> [1] 0.2104837
#> 
#> $scores$`3-2`
#> [1] 0.03681593
#> 
#> $scores$`2-3`
#> [1] 0.007117161
#> 
#> $scores$`1-3`
#> [1] 0.002292478
#> 
#> $scores$`0-3`
#> [1] 0.0008439086
```

So given the performances of the two teams during that match, we expect
that the home team should have won, with 3-0 being the most likely
scoreline. Compare this to the actual match result:

``` r
summary(x)
#> Match summary:
#> Date: 2015-01-25
#> League: Finale mladinke
#> Teams: Braslovče (JERONČIČ ZORAN/MIHALINEC DAMIJANA)
#>        vs
#>        Nova KBM Branik (HAFNER MATJAŽ)
#> Result: 3-0 (25-16, 25-14, 25-22)
#> Duration: 67 minutes
```

## Example 2: exploring match options

Let’s say we have two teams with the following season-average
parameters:

``` r
library(dplyr)
rates <- tribble(~team, ~serve_ace, ~serve_error, ~rec_set_error, ~rec_att_error, ~rec_att_kill, ~trans_set_error, ~trans_att_error, ~trans_att_kill, ~sideout, ~rec_block, ~trans_block,
"My team",    0.062, 0.156, 0.009, 0.071, 0.499, 0.018, 0.082, 0.452, 0.668, 0.075, 0.079,
"Other team", 0.069, 0.190, 0.014, 0.063, 0.523, 0.021, 0.102, 0.435, 0.683, 0.083, 0.109)
knitr::kable(rates)
```

| team       | serve\_ace | serve\_error | rec\_set\_error | rec\_att\_error | rec\_att\_kill | trans\_set\_error | trans\_att\_error | trans\_att\_kill | sideout | rec\_block | trans\_block |
| :--------- | ---------: | -----------: | --------------: | --------------: | -------------: | ----------------: | ----------------: | ---------------: | ------: | ---------: | -----------: |
| My team    |      0.062 |        0.156 |           0.009 |           0.071 |          0.499 |             0.018 |             0.082 |            0.452 |   0.668 |      0.075 |        0.079 |
| Other team |      0.069 |        0.190 |           0.014 |           0.063 |          0.523 |             0.021 |             0.102 |            0.435 |   0.683 |      0.083 |        0.109 |

“My team” is due to play “Other team” in our next match. If we assume
that both teams play to their season-average parameters, what outcome do
we expect?

``` r
vs_simulate_match(rates, simple = TRUE)
#> $pwin
#> [1] 0.4866795
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.1201833
#> 
#> $scores$`3-1`
#> [1] 0.1826209
#> 
#> $scores$`3-2`
#> [1] 0.1838752
#> 
#> $scores$`2-3`
#> [1] 0.1909978
#> 
#> $scores$`1-3`
#> [1] 0.192379
#> 
#> $scores$`0-3`
#> [1] 0.1299437
```

Looks like we expect a close match, but My team is most probably going
to lose.

Perhaps as the coach of My team there are some adjustments I could make
— in choosing the players in our starting lineup, or in our match
tactics. Can simulation help us explore which option might be most
beneficial?

Let’s say that one of our options is to substitute in a different
pass-hitter: Betty, who has a more aggressive serve and attack but who
is a weaker passer than our starting pass-hitter Agnes. With Betty in
the lineup instead of Agnes, we guesstimate our new team parameters to
be:

``` r
rates2 <- rates

## increase my team's serve aces and errors by 1% each, and attack kills by 2%
rates2[1, c("serve_ace", "serve_error", "rec_att_kill", "trans_att_kill")] <-
    rates2[1, c("serve_ace", "serve_error", "rec_att_kill", "trans_att_kill")] + c(0.01, 0.01, 0.02, 0.02)

## increase opposition serve aces by 1%
rates2[2, c("serve_ace")] <- rates2[2, c("serve_ace")] + 0.01

vs_simulate_match(rates2, simple = TRUE)
#> $pwin
#> [1] 0.5233379
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.1334328
#> 
#> $scores$`3-1`
#> [1] 0.195746
#> 
#> $scores$`3-2`
#> [1] 0.1941591
#> 
#> $scores$`2-3`
#> [1] 0.180478
#> 
#> $scores$`1-3`
#> [1] 0.1792539
#> 
#> $scores$`0-3`
#> [1] 0.1169302
```

That makes a slight improvement. Our second option is to change our
serving tactics: we will serve more aggressively in order to put more
pressure on Other team’s reception but accepting that we will make more
serve errors in doing so:

``` r
rates3 <- rates

## increase my team's serve aces by 2% and errors by 5%
rates3[1, c("serve_ace", "serve_error")] <- rates3[1, c("serve_ace", "serve_error")] + c(0.02, 0.05)

## decrease opposition reception kills by 5% due to their expected poorer passing
rates3[2, c("rec_att_kill")] <- rates3[2, c("rec_att_kill")] - 0.05
vs_simulate_match(rates3, simple = TRUE)
#> $pwin
#> [1] 0.565142
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.1557209
#> 
#> $scores$`3-1`
#> [1] 0.2158291
#> 
#> $scores$`3-2`
#> [1] 0.193592
#> 
#> $scores$`2-3`
#> [1] 0.1770885
#> 
#> $scores$`1-3`
#> [1] 0.1591584
#> 
#> $scores$`0-3`
#> [1] 0.09861113
```

This looks like it might be a better option (assuming, of course, that
we have estimated the changes in rates correctly).
