
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volleysim

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
![openvolley](https://img.shields.io/badge/openvolley-darkblue.svg?logo=data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMTAiIGhlaWdodD0iMjEwIj48cGF0aCBkPSJNOTcuODMzIDE4Ny45OTdjLTQuNTUtLjM5Ni0xMi44MTItMS44ODYtMTMuNTgxLTIuNDQ5LS4yNDItLjE3Ny0xLjY5Mi0uNzUzLTMuMjIyLTEuMjgxLTI4LjY5Ni05Ljg5NS0zNS4xNy00NS45ODctMTMuODY4LTc3LjMyMyAyLjY3Mi0zLjkzIDIuNTc5LTQuMTktMS4zOTQtMy45MDYtMTIuNjQxLjktMjcuMiA2Ljk1Mi0zMy4wNjYgMTMuNzQ1LTUuOTg0IDYuOTI3LTcuMzI3IDE0LjUwNy00LjA1MiAyMi44NjIuNzE2IDEuODI2LS45MTgtLjE3LTEuODktMi4zMS03LjM1Mi0xNi4xNzQtOS4xODEtMzguNTYtNC4zMzctNTMuMDc0LjY5MS0yLjA3IDEuNDE1LTMuODY2IDEuNjEtMy45ODkuMTk0LS4xMjMuNzgyLTEuMDUzIDEuMzA3LTIuMDY2IDMuOTQ1LTcuNjE3IDkuNDU4LTEyLjg2MiAxNy44MzktMTYuOTcgMTIuMTcyLTUuOTY4IDI1LjU3NS01LjgyNCA0MS40My40NDUgNi4zMSAyLjQ5NSA4LjgwMiAzLjgwMSAxNi4wNDcgOC40MTMgNC4zNCAyLjc2MiA0LjIxMiAyLjg3NCAzLjU5NC0zLjE3My0yLjgyNi0yNy42ODEtMTYuOTA3LTQyLjE4NS0zNi4wNjgtMzcuMTUxLTQuMjU0IDEuMTE3IDUuMjQtMy4zMzggMTEuNjYtNS40NzMgMTMuMTgtNC4zOCAzOC45MzctNS43NzIgNDYuMDc0LTEuNDg4IDEuMjQ3LjU0NyAyLjIyOCAxLjA5NSAzLjI3NSAxLjYzIDQuMjkgMi4xMDcgMTEuNzMzIDcuNjk4IDE0LjI2NSAxMS40MjcuNDA3LjYgMS4yNyAxLjg2NiAxLjkxNyAyLjgxNCAxMS4zMDggMTYuNTY1IDguNjIzIDQxLjkxLTYuODM4IDY0LjU1Mi0zLjI0OSA0Ljc1OC0zLjI1OCA0Ljc0MiAyLjQ1IDQuMDE4IDMyLjQ4Mi00LjEyMiA0OC41MTUtMjEuOTM1IDM5LjU3OC00My45NzQtMS4xNC0yLjgwOSAxLjU2NiAxLjA2IDMuNTE4IDUuMDMyIDI5LjY5MyA2MC40MTctMjIuNTggMTA3Ljg1My03OS40OTggNzIuMTQzLTUuMDg0LTMuMTktNS4xMjMtMy4xNTItMy45MDIgMy44ODMgNC43MjEgMjcuMjIgMjUuNzgzIDQzLjU2MiA0NC4wODkgMzQuMjEgMS4zNjItLjY5NiAyLjIxLS43NSAyLjIxLS4xNDMtNi43NiAzLjg1Ny0xNi4wMTggNi41NTMtMjMuMTI2IDguMDkxLTcuNTU1IDEuNTQ3LTE4LjM2NiAyLjE3Mi0yNi4wMiAxLjUwNnoiIGZpbGw9IiNmZmYiLz48ZWxsaXBzZSBjeD0iMTA1Ljk3NSIgY3k9IjEwNC40NDEiIHJ4PSI5NC44NCIgcnk9IjkyLjU0MiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjEwLjc0Ii8+PC9zdmc+)
[![R-CMD-check](https://github.com/openvolley/volleysim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openvolley/volleysim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

``` r
options(repos = c(openvolley = "https://openvolley.r-universe.dev",
                  CRAN = "https://cloud.r-project.org"))
install.packages("volleysim")

## or

## install.packages("remotes") ## if needed
remotes::install_github("openvolley/volleysim")
```

The `volleysim` package provides functions for simulating sets and
matches of volleyball. The simulation model can be parameterized in two
ways:

1.  A simple “sideout” parameterization. We simply specify the sideout
    rate for each team. This can be a constant value (i.e. their average
    sideout rate) or can be provided as a function, allowing the sideout
    rate to vary depending on whatever factors you think might be
    appropriate. (Function-based parameterization is experimental: see
    `help("vs_simulate_set")` for more information.)

2.  A more detailed “phase” parameterization. In this model, we specify
    the rates for each team for:

    -   `serve_ace` (serve ace rate)
    -   `serve_error` (serve error rate)
    -   `rec_loss_other` (loss of the point during reception-phase play,
        excluding reception errors and attack errors — e.g. errors on
        reception-phase sets)
    -   `rec_att_error` (error rate on reception-phase attacks)
    -   `rec_att_kill` (kill rate on reception-phase attacks)
    -   `rec_att_replayed` (rate at which reception-phase attacks are
        replayed by the attacking team for a second attack: either
        blocked for reattack, deliberately recycled off the block, or
        dug by the defending team but put back over as a freeball)
    -   `rec_no_att` (proportion of receptions, excluding reception
        errors, on which an attack is not made)
    -   `rec_block` (block kill rate against reception-phase attacks)
    -   `trans_loss_other`, `trans_att_error`, `trans_att_kill`,
        `trans_att_replayed`, `trans_no_att`, `trans_block` - as for the
        `rec_*` parameters, but in transition phase (i.e. everything
        after the reception-phase attack)

## Example 1

``` r
library(volleysim)
library(datavolley)
library(dplyr)

## read an example file
x <- dv_read(dv_example_file())

## calculate the rates we need to simulate
rates <- vs_estimate_rates(x, target_team = "each")

vs_simulate_match(rates)
#> $pwin
#> [1] 0.9860614
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.7094635
#> 
#> $scores$`3-1`
#> [1] 0.230106
#> 
#> $scores$`3-2`
#> [1] 0.04649198
#> 
#> $scores$`2-3`
#> [1] 0.009293843
#> 
#> $scores$`1-3`
#> [1] 0.00338111
#> 
#> $scores$`0-3`
#> [1] 0.001263653
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
rates <- tribble(~team, ~serve_ace, ~serve_error, ~rec_loss_other, ~rec_att_error, ~rec_att_kill, ~rec_att_replayed, ~rec_no_att, ~trans_loss_other, ~trans_att_error, ~trans_att_kill, ~trans_att_replayed, ~trans_no_att, ~rec_block, ~trans_block,
"My team",    0.062, 0.156, 0.009, 0.071, 0.499, 0.05, 0.05, 0.018, 0.082, 0.452, 0.05, 0.08, 0.075, 0.079,
"Other team", 0.069, 0.190, 0.014, 0.063, 0.523, 0.05, 0.05, 0.021, 0.102, 0.435, 0.05, 0.05, 0.083, 0.109)
knitr::kable(rates)
```

| team       | serve_ace | serve_error | rec_loss_other | rec_att_error | rec_att_kill | rec_att_replayed | rec_no_att | trans_loss_other | trans_att_error | trans_att_kill | trans_att_replayed | trans_no_att | rec_block | trans_block |
|:-----------|----------:|------------:|---------------:|--------------:|-------------:|-----------------:|-----------:|-----------------:|----------------:|---------------:|-------------------:|-------------:|----------:|------------:|
| My team    |     0.062 |       0.156 |          0.009 |         0.071 |        0.499 |             0.05 |       0.05 |            0.018 |           0.082 |          0.452 |               0.05 |         0.08 |     0.075 |       0.079 |
| Other team |     0.069 |       0.190 |          0.014 |         0.063 |        0.523 |             0.05 |       0.05 |            0.021 |           0.102 |          0.435 |               0.05 |         0.05 |     0.083 |       0.109 |

“My team” is due to play “Other team” in our next match. If we assume
that both teams play to their season-average parameters, what outcome do
we expect?

``` r
vs_simulate_match(rates)
#> $pwin
#> [1] 0.478248
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.1159543
#> 
#> $scores$`3-1`
#> [1] 0.1784148
#> 
#> $scores$`3-2`
#> [1] 0.1838789
#> 
#> $scores$`2-3`
#> [1] 0.1910657
#> 
#> $scores$`1-3`
#> [1] 0.1965849
#> 
#> $scores$`0-3`
#> [1] 0.1341014
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

vs_simulate_match(rates2)
#> $pwin
#> [1] 0.5305033
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.1379761
#> 
#> $scores$`3-1`
#> [1] 0.2002373
#> 
#> $scores$`3-2`
#> [1] 0.19229
#> 
#> $scores$`2-3`
#> [1] 0.1822207
#> 
#> $scores$`1-3`
#> [1] 0.1747627
#> 
#> $scores$`0-3`
#> [1] 0.1125133
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
vs_simulate_match(rates3)
#> $pwin
#> [1] 0.5605252
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.1518841
#> 
#> $scores$`3-1`
#> [1] 0.2127369
#> 
#> $scores$`3-2`
#> [1] 0.1959041
#> 
#> $scores$`2-3`
#> [1] 0.1760242
#> 
#> $scores$`1-3`
#> [1] 0.1622593
#> 
#> $scores$`0-3`
#> [1] 0.1011914
```

This looks like it might be a better option (assuming, of course, that
we have estimated the changes in rates correctly).

## Example 3

Let’s look at another match: the 2020 Austrian Women’s Volley Cup played
between Hartberg and UVC Graz (the dvw file was downloaded from
<https://www.volleynet.at/dvdownload/information/f-Damen/> and is
bundled with the volleysim package). UVC Graz won the match 3-1:

``` r
x <- dv_read(vs_example_file())
summary(x)
#> Match summary:
#> Date: 2020-11-08
#> League: Austrian Volley Cup Women 2020/21 - DCup
#> Teams: Hartberg w Cup (BEINSEN Birgit/ALMER Katharina)
#>        vs
#>        UVC Graz w Cup (PACK Matthias/APPEL Martin)
#> Result: 1-3 (21-25, 21-25, 25-20, 15-25)
#> Duration: 88 minutes
```

Let’s see what result we expected given the team’s actual performances
during the match:

``` r
rates <- list(vs_estimate_rates(x, target_team = home_team(x)),
              vs_estimate_rates(x, target_team = visiting_team(x)))
sim_result <- vs_simulate_match(rates = rates)
sim_result
#> $pwin
#> [1] 0.3275444
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.0647837
#> 
#> $scores$`3-1`
#> [1] 0.1163446
#> 
#> $scores$`3-2`
#> [1] 0.1464161
#> 
#> $scores$`2-3`
#> [1] 0.2003236
#> 
#> $scores$`1-3`
#> [1] 0.2581142
#> 
#> $scores$`0-3`
#> [1] 0.2140178
```

The simulations suggest that Hartberg had a 32.8% chance of winning,
with the most likely scoreline being 1-3 — consistent with the actual
result.

Now let’s say that the two teams will be playing again soon, and the
Hartberg coach thinks that their first-ball attack could be improved by
improving their passing. What difference might we expect in the match
outcome for an improvement in this area?

First let’s get a handle on the relevant performance parameters.
“Positive” passes here means passes that were rated as perfect or
positive.

``` r
## extract the play-by-play data
xp <- plays(x)

## identify first-ball attacks
fba <- xp %>% dplyr::filter(skill == "Attack" & phase == "Reception") %>%
    mutate(made_attack = TRUE, fbso = evaluation == "Winning attack") %>%
    dplyr::select(point_id, team, made_attack, fbso)

## join that back to the full play-by-play data
xp <- left_join(xp, fba, by = c("point_id", "team")) %>%
    mutate(fbso = if_else(is.na(fbso), FALSE, fbso),
           made_attack = if_else(is.na(made_attack), FALSE, made_attack))

## and pass quality on each rally
pq <- xp %>% dplyr::filter(skill == "Reception") %>% group_by(point_id) %>%
    dplyr::summarize(pass_quality = case_when(n() == 1 ~ evaluation))
xp <- left_join(xp, pq, by = "point_id") %>%
    mutate(pass_quality = case_when(grepl("Perfect|Positive", pass_quality) ~ "Positive",
                                    grepl("Error", pass_quality) ~ "Error",
                                    TRUE ~ "Other"))

## finally summarize the first-ball attacks by pass quality
fb_summary <- xp %>% dplyr::filter(skill == "Reception" & team == home_team(x)) %>%
    group_by(pass_quality) %>% dplyr::summarize(N = n(),
                                                `Attack %` = mean(made_attack)*100,
                                                `Attack kill %` = mean(fbso[made_attack])*100,
                                                `FBSO%` = mean(fbso)*100)
fb_summary
#> # A tibble: 3 × 5
#>   pass_quality     N `Attack %` `Attack kill %` `FBSO%`
#>   <chr>        <int>      <dbl>           <dbl>   <dbl>
#> 1 Error            3        0             NaN       0  
#> 2 Other           26       61.5            18.8    11.5
#> 3 Positive        49       87.8            41.9    36.7
```

Hartberg made 49 positive passes (62.8% positive pass rate). They made
an actual attack on 87.8% of those positive passes, with a kill rate of
41.9% on those attacks. Their overall first-ball sideout rate on
positive passes was 36.7%.

On other passes (excluding pass errors), Hartberg’s first-attack sideout
rate was 11.5%, with only 61.5% of those passes leading to an attack,
and a kill rate of 18.8% on those attacks.

Let’s say that with some focused training, the Hartberg coach thinks
that their positive pass rate can be substantially increased, from 62.8%
to 75%. This would change their reception attack kill rate, because more
attacks would be made from positive passes. The expected reception
attack kill rate would be the weighted average of the kill rates on
positive and other passes (where the weights are the relative numbers of
positive and other passes).

``` r
new_positive_pass_rate <- 0.75
attack_rate_pos <- new_positive_pass_rate * 0.878
attack_rate_other <- (1 - new_positive_pass_rate) * 0.615
new_rec_att_kill <- (attack_rate_pos * 0.419 + ## positive pass rate multiplied by their corresponding kill rate
                     attack_rate_other * 0.188 ## attack rate on other passes * their kill rate
                     ) / (attack_rate_pos + attack_rate_other)
new_rec_att_kill
#> [1] 0.3752742
```

That is, with the hypothesized better passing performance we expect only
a modest increase in the overall reception attack kill rate to 37.5% (up
from 35.6%).

Armed with that estimate, we can explore what effect that might have on
a re-match:

``` r
rates[[1]]$rec_att_kill <- new_rec_att_kill
new_sim_result <- vs_simulate_match(rates = rates)
new_sim_result
#> $pwin
#> [1] 0.3664445
#> 
#> $scores
#> $scores$`3-0`
#> [1] 0.07649556
#> 
#> $scores$`3-1`
#> [1] 0.132131
#> 
#> $scores$`3-2`
#> [1] 0.157818
#> 
#> $scores$`2-3`
#> [1] 0.2004959
#> 
#> $scores$`1-3`
#> [1] 0.2426869
#> 
#> $scores$`0-3`
#> [1] 0.1903727
```

Giving a match win probability of 36.6% (up from 32.8%).
