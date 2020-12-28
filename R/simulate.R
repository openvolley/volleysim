#' Simulate a set of volleyball
#'
#' `vs_simulate_set_theor` and `vs_simulate_set_mc` are convenience functions for `vs_simulate_set(..., method = "theoretical")` and `vs_simulate_set(..., method = "monte carlo")` respectively.
#'
#' @param rates list: A two-element list, each element of which is a set of rates as returned by `vs_estimate_rates`. Experimental: for `process_model` "sideout", the `sideout` rate component can be a function. This function will be called at each step of the simulation with the parameters:
#' * `team_1_score` - the score of team 1 at each point in the set so far
#' * `team_2_score` - the score of team 2 at each point in the set so far
#' * `serving` - the serving team 1 or 2 at each point in the set so far
#' * `point_won_by` - which team won each point in the set so far (this will be NA for the last entry, because that's the current point that hasn't been simulated yet)
#' * `outcome` - the outcome of each point in the set so far, either "Sideout" or "Breakpoint" if `process_model` is "sideout", or details TBD if `process_model` is "phase"
#' @param process_model string: either "sideout" or "phase". Details TBD
#' @param serving logical: if `TRUE`, team 1 will serve first. If `NA`, the team serving first will be chosen at random
#' @param go_to integer: the minimum score that must be reached to end the set (typically 25 for indoor volleyball in sets 1 to 4, 15 in set 5, or 21 in beach volleyball)
#' @param simple logical: if `TRUE`, return simplified output. The content depends on the `method`:
#' * `method = "monte carlo"`. If `simplified = TRUE`, return the team (1 or 2) that won the set. If `simplified = FALSE`, return extra details in a data.frame
#' * `method = "theoretical"`. If `simplified = TRUE`, return the probability that team 1 won the set. If `simplified = FALSE`, return additional details (TBD)
#' @param id : an optional value that (if non-`NULL`) will be returned in the `id` column of the returned data frame, if `simple` is `FALSE`
#' @param method string: the simulation method to use. Either "monte carlo" or "theoretical". Details TBD
#' @param ... : parameters as for `vs_simulate_set`. `vs_simulate_set_theor` and `vs_simulate_set_mc` are convenience functions for `vs_simulate_set(..., method = "theoretical")` and `vs_simulate_set(..., method = "monte carlo")` respectively
#'
#' @return Integer (1 or 2) or a data frame, depending on the value of `simple`
#'
#' @seealso [vs_estimate_rates] [vs_simulate_match]
#'
#' @examples
#' \dontrun{
#'   library(datavolley)
#'   x <- dv_read(dv_example_file())
#'   rates <- list(vs_estimate_rates(x, target_team = home_team(x)),
#'                 vs_estimate_rates(x, target_team = visiting_team(x)))
#'   vs_simulate_set(rates) ## simulate a single set
#'   vs_simulate_match(rates, n = 100, simple = TRUE) ## simulate a match 100 times
#'   ## so given the performances of the two teams during that match, we expect
#'   ##  that the home team should have won, with 3-0 being the most likely scoreline
#'
#'   ## compare to the actual match result
#'   summary(x)
#' }
#'
#' ## sideout rates as a function for team 2
#' sofun2 <- function(serving, point_won_by, ...) {
#'     ## if team 2 won their previous sideout opportunity, their sideout rate is 0.6
#'     ## otherwise it's 0.5
#'     prevso <- tail(na.omit(point_won_by[serving == 1]), 1)
#'     if (length(prevso) < 1 || prevso == 1) {
#'         ## first sideout opportunity or lost the last one
#'         0.5
#'     } else {
#'         0.6
#'     }
#' }
#'
#' rates <- list(list(sideout = 0.55), ## first team has constant 55% sideout rate
#'                list(sideout = sofun2)) ## function for team 2's sideout rate
#'
#' vs_simulate_set(rates = rates, process_model = "sideout")
#'
#' @export
vs_simulate_set <- function(rates, process_model = "phase", serving = NA, go_to = 25, simple = FALSE, id = NULL, method = "monte carlo") {
    assert_that(is.string(process_model))
    process_model <- tolower(process_model)
    process_model <- match.arg(process_model, c("phase", "sideout"))
    assert_that(is.flag(simple), !is.na(simple))
    if (missing(serving) || is.na(serving)) serving <- runif(1) > 0.5 ## random
    assert_that(is.flag(serving), !is.na(serving))
    assert_that(is.string(method))
    method <- tolower(method)
    method <- match.arg(method, c("monte carlo", "theoretical"))
    rates <- precheck_rates(rates, process_model = process_model)
    if (process_model == "sideout") {
        if (is.null(rates[[1]]$sideout) || is.null(rates[[2]]$sideout)) {
            stop("one or both rates are missing their 'sideout' component")
        }
    } else {
        rnms <- c("serve_ace", "serve_error", "rec_set_error", "rec_att_error", "rec_att_kill", "rec_block", "trans_set_error", "trans_att_error", "trans_att_kill", "trans_block")
        if (!all(rnms %in% names(rates[[1]])) || !all(rnms %in% names(rates[[2]]))) {
            stop("one or both rates are missing a required component (required: ", paste(rnms, collapse = ", "), ")")
        }
    }
    sim_fun <- if (method == "monte carlo") do_sim_set_mc else do_sim_set_theor
    sim_fun(rates = rates, process_model = process_model, serving = serving, go_to = go_to, simple = simple, id = id)
}

#' @rdname vs_simulate_set
#' @export
vs_simulate_set_mc <- function(...) {
    vs_simulate_set(..., method = "monte carlo")
}

#' @rdname vs_simulate_set
#' @export
vs_simulate_set_theor <- function(...) {
    vs_simulate_set(..., method = "theoretical")
}

do_sim_set_theor <- function(rates, process_model, serving, go_to, simple, id) {
    if (go_to < 1 | go_to > 25) stop("go_to must be between 1 and 25 inclusive")
    m <- set_win_probabilities_theoretical(c(rates[[1]]$sideout, rates[[2]]$sideout))
    if (isTRUE(serving)) {
        m$s.matrix[26-go_to, 26-go_to]
    } else {
        m$o.matrix[26-go_to, 26-go_to]
    }
}

do_sim_set_mc <- function(rates, process_model, serving, go_to, simple, id) {
    ## rates are list(
    ##  ## for team 1, probs
    ##  data.frame(serve_ace = z, serve_error = z,
    ##             rec_att_error = z, rec_att_kill = z,
    ##             trans_att_error = z, trans_att_kill = z,
    ##             rec_block = z, trans_block = z),
    ##  data.frame(same for team 2))
    ## preallocate random numbers
    prandf <- function() {
        pptr <- 0L
        pprob <- runif(300)
        function() {
            pptr <<- pptr+1
            if (pptr <= length(pprob)) pprob[pptr] else stop("need larger allocation")
        }
    }
    tm1_prandf <- prandf()
    tm2_prandf <- prandf()
    tm_scores <- matrix(NA_integer_, nrow = 100, ncol = 2)
    outcome <- rep(NA_character_, 100)
    tm_srv <- rep(NA_integer_, 100)
    tm_point_won_by <- rep(NA_integer_, 100)
    ptr <- 1 ## pointer into those vectors
    tm_scores[1, ] <- c(0L, 0L) ## scores for team 1, team 2
    srv <- if (serving) 1L else 2L ## serving TRUE => team 1 starts serving
    tm_srv[1] <- srv
    sc <- tm_scores[ptr, ]
    while (all(sc < go_to) || abs(diff(sc)) < 2) {
        tm_scores[ptr + 1, ] <- tm_scores[ptr, ] ## scores at the START of the next point, updated below
        srv_tm_rates <- rates[[srv]] ## serving team's rates
        rec_tm_rates <- rates[[3-srv]] ## other team's rates
        ## the simulation process is basically a hard-coded set of if-else statements here
        ## this should be replaced by something more flexible and configurable, and able to cope with a more highly parameterized simulation model
        if (process_model == "sideout") {
            this_so_rate <- rec_tm_rates$sideout
            if (is.function(this_so_rate)) {
                idx <- seq_len(ptr)
                this_so_rate <- this_so_rate(team_1_score = tm_scores[idx, 1], team_2_score = tm_scores[idx, 2], serving = tm_srv[idx], point_won_by = tm_point_won_by[idx],  outcome = outcome[idx], go_to = go_to)
            }
            lost_serve <- tm2_prandf() <= this_so_rate
            outcome[ptr] <- if (lost_serve) "Sideout" else "Breakpoint"
        } else {
            ## serve
            serve_outc <- sum(tm1_prandf() <= cumsum(c(srv_tm_rates$serve_ace, srv_tm_rates$serve_error)))
            if (serve_outc == 2) {
                lost_serve <- FALSE
                outcome[ptr] <- "Serve ace"
            } else if (serve_outc == 1) {
                lost_serve <- TRUE
                outcome[ptr] <- "Serve error"
            } else {
                ## rec attack by non-serving (other) team
                temp <- c(rec_tm_rates$rec_att_kill, rec_tm_rates$rec_att_error, rec_tm_rates$rec_set_error, srv_tm_rates$rec_block, rec_tm_rates$rec_att_replayed)
                if (sum(temp) > 1) stop("The reception-phase probabilities sum to more than 1")
                ra_outc <- sum(tm2_prandf() <= cumsum(temp))
                if (ra_outc == 5) {
                    lost_serve <- TRUE
                    outcome[ptr] <- "Rec attack kill"
                } else if (ra_outc == 4) {
                    lost_serve <- FALSE
                    outcome[ptr] <- "Rec attack error"
                } else if (ra_outc == 3) {
                    lost_serve <- FALSE
                    outcome[ptr] <- "Rec set error"
                } else if (ra_outc == 2) {
                    lost_serve <- FALSE
                    outcome[ptr] <- "Rec attack block"
                } else {
                    ## transition - iterate back and forth between teams until someone wins the point
                    ## tptr is the team currently in transition, 1 = serving team, 2 = receiving team
                    if (ra_outc == 1) {
                        ## replayed, so next attack (first transition attack) is also by receiving team
                        tptr <- 2L
                    } else {
                        tptr <- 1L ## serving team gets first transition attack opportunity
                    }
                    lost_serve <- NA
                    while (is.na(lost_serve)) {
                        if (tptr < 2) {
                            temp <- c(srv_tm_rates$trans_att_kill, srv_tm_rates$trans_att_error, srv_tm_rates$trans_set_error, rec_tm_rates$trans_block, srv_tm_rates$trans_att_replayed)
                            if (sum(temp) > 1) stop("The transition-phase probabilities sum to more than 1")
                            ta_outc <- sum(tm1_prandf() <= cumsum(temp))
                            if (ta_outc == 5) {
                                lost_serve <- FALSE
                                outcome[ptr] <- "Trans attack kill"
                            } else if (ta_outc == 4) {
                                lost_serve <- TRUE
                                outcome[ptr] <- "Trans attack error"
                            } else if (ta_outc == 3) {
                                lost_serve <- TRUE
                                outcome[ptr] <- "Trans set error"
                            } else if (ta_outc == 2) {
                                lost_serve <- TRUE
                                outcome[ptr] <- "Trans attack block"
                            }
                        } else {
                            temp <- c(rec_tm_rates$trans_att_kill, rec_tm_rates$trans_att_error, rec_tm_rates$trans_set_error, srv_tm_rates$trans_block, rec_tm_rates$trans_att_replayed)
                            if (sum(temp) > 1) stop("The transition-phase probabilities sum to more than 1")
                            ta_outc <- sum(tm2_prandf() <= cumsum(temp))
                            if (ta_outc == 5) {
                                lost_serve <- TRUE
                                outcome[ptr] <- "Trans attack kill"
                            } else if (ta_outc == 4) {
                                lost_serve <- FALSE
                                outcome[ptr] <- "Trans attack error"
                            } else if (ta_outc == 3) {
                                lost_serve <- FALSE
                                outcome[ptr] <- "Trans set error"
                            } else if (ta_outc == 2) {
                                lost_serve <- FALSE
                                outcome[ptr] <- "Trans attack block"
                            }
                        }
                        if (ta_outc == 1) {
                            ## attack replayed, so same team gets another transition attack opportunity
                            ## tptr remains the same
                        } else {
                            tptr <- 3L - tptr ## other team now in transition attack phase
                        }
                    }
                }
            } ## if-else method
        }
        if (lost_serve) {
            srv <- 3L - srv ## sided out, change server
        }
        tm_point_won_by[ptr] <- srv ## who won this point
        tm_srv[ptr + 1] <- srv
        tm_scores[ptr + 1, srv] <- tm_scores[ptr, srv] + 1
        ptr <- ptr + 1
        sc <- tm_scores[ptr, ]
        if (max(sc) > (go_to + 15)) {
            if (simple) return(NA_integer_) else return(NULL)
        }
    }
    if (simple) {
        which.max(sc)
    } else {
        tm_scores <- tm_scores[seq_len(ptr - 1), ]
        out <- setNames(as.data.frame(tm_scores), c("team_1_score", "team_2_score"))
        out$serving <- tm_srv[seq_len(ptr - 1)]
        out$point_won_by <- tm_point_won_by[seq_len(ptr - 1)]
        out$set_won_by <- which.max(sc)
        out$outcome <- outcome[seq_len(ptr - 1)]
        if (!is.null(id)) out$id <- id
        out
    }
}

## not exported
## given the probability of winning sets 1-4, and set 5, calculate the overall probability of winning the match
## currently best-of-5-sets only
vs_set_probs_to_match <- function(sp13, sp24, sp5 = sp13, serve_known = FALSE) {
    ## all possible set outcomes in a 5-set match
    tposs_orig <- matrix(c(c(1, 1, 1, NA_real_, NA_real_), ## 3-0
                      c(1, 1, 0, 1, NA_real_), ## 3-1
                      c(1, 1, 0, 0, 1), ## 3-2
                      c(1, 0, 1, 1, NA_real_), ## 3-1
                      c(1, 0, 1, 0, 1), ## 3-2
                      c(1, 0, 0, 1, 1), ## 3-2
                      c(0, 1, 1, 1, NA_real_), ## 3-1
                      c(0, 1, 1, 0, 1), ## 3-2
                      c(0, 1, 0, 1, 1), ## 3-2
                      c(0, 0, 1, 1, 1)), ## 3-2
                    ncol = 5, byrow = TRUE)
    pwinsc <- c(0, 1, 2, 1, 2, 2, 1, 2, 2, 2) ## losing team score on each of those possibilities
    tposs <- rbind(tposs_orig, 1-tposs_orig)
    pwinsc <- c(pwinsc, 3+pwinsc)
    tposs[,c(1,3)] <- abs(1-tposs[, c(1,3)]-sp13)
    tposs[,c(2,4)] <- abs(1-tposs[, c(2,4)]-sp24)
    tposs[,5] <- abs(1-tposs[, 5]-sp5)
    temp <- apply(tposs, 1, prod, na.rm = TRUE) ## prob of each of the possible ways to win
    
    if(!serve_known){
        tposs2 <- rbind(tposs_orig, 1-tposs_orig)
        tposs2[,c(1,3)] <- abs(1-tposs2[, c(1,3)]-sp24)  ## if we don't know who started with serve then we have to flip sets 1/3 and 2/4
        tposs2[,c(2,4)] <- abs(1-tposs2[, c(2,4)]-sp13)
        tposs2[,5] <- abs(1-tposs2[, 5]-sp5)
        temp2 <- apply(tposs2, 1, prod, na.rm = TRUE)
        temp <- (temp + temp2)/2
    }
    
    list(pwin = sum(temp[1:10]), scores = list("3-0" = temp[1], "3-1" = sum(temp[pwinsc==1]), "3-2" = sum(temp[pwinsc==2]), "2-3" = sum(temp[pwinsc==5]), "1-3" = sum(temp[pwinsc==4]), "0-3" = sum(temp[pwinsc==3])))
}


#' Simulate a volleyball match
#'
#' Currently hard-coded to indoor, best-of-5-set scoring.
#'
#' @param rates list: A two-element list, each element of which is a set of rates as returned by `vs_estimate_rates`
#' @param process_model string: either "sideout" or "phase". Details TBD
#' @param serving logical: if `TRUE`, team 1 will serve first. If `NA`, the team serving first will be chosen at random
#' @param n integer: the number of simulations to run
#' @param simple logical: if `TRUE`, just return the probability of team winning and the probabilities of each possible set score. If `FALSE`, return extra details in a named list. The details will differ between `method = "monte carlo"` and `method = "theoretical"` 
#' @param method string: the simulation method to use. Either "monte carlo" or "theoretical". Details TBD
#' @param ... : parameters as for `vs_simulate_match`. `vs_simulate_match_theor` and `vs_simulate_match_mc` are convenience functions for `vs_simulate_match(..., method = "theoretical")` and `vs_simulate_match(..., method = "monte carlo")` respectively
#'
#' @seealso [vs_estimate_rates] [vs_simulate_match]
#'
#' @examples
#' \dontrun{
#'   library(datavolley)
#'   x <- dv_read(dv_example_file())
#'   rates <- list(vs_estimate_rates(x, target_team = home_team(x)),
#'                 vs_estimate_rates(x, target_team = visiting_team(x)))
#'   vs_simulate_set(rates) ## simulate a single set
#'   vs_simulate_match(rates, n = 100, simple = TRUE) ## simulate a match 100 times
#'   ## so we expect the home team to win, with 3-0 being the most likely scoreline
#'
#'   ## compare to the actual match result
#'   summary(x)
#' }
#'
#' @export
vs_simulate_match <- function(rates, process_model = "phase", serving = NA, n = 2000, simple = FALSE, method = "monte carlo") {
    assert_that(is.flag(simple), !is.na(simple))
    rates <- precheck_rates(rates, process_model = process_model)
    sim_fun <- if (method == "monte carlo") do_sim_match_mc else do_sim_match_theor
    sim_fun(rates = rates, process_model = process_model, serving = serving, n = n, simple = simple)
}

do_sim_match_mc <- function(rates, process_model, serving, n, simple) {
    if (simple) {
        simres14 <- sapply(1:n, function(z) vs_simulate_set(rates = if (nrow(rates[[1]]) == 1) rates else list(rates[[1]][n, ], rates[[2]][n, ]), process_model = process_model, serving = serving, go_to = 25, simple = TRUE, method = "monte carlo"))
        nsims <- sum(!is.na(simres14))
    } else {
        simres14 <- bind_rows(lapply(1:n, function(z) vs_simulate_set(rates = if (nrow(rates[[1]]) == 1) rates else list(rates[[1]][n, ], rates[[2]][n, ]), process_model = process_model, serving = serving, go_to = 25, simple = FALSE, id = z, method = "monte carlo")))
        nsims <- length(unique(simres14$id))
    }
    if (nsims/n < 0.98) {
        ## allow up to 2% that didn't reach result
        warning("More than 2% of set1-4 simulations did not yield a result")
    }
    if (simple) {
        simres5 <- sapply(1:n, function(z) vs_simulate_set(rates = if (nrow(rates[[1]]) == 1) rates else list(rates[[1]][n, ], rates[[2]][n, ]), process_model = process_model, serving = serving, go_to = 15, simple = TRUE, method = "monte carlo"))
        nsims <- sum(!is.na(simres5))
    } else {
        simres5 <- bind_rows(lapply(1:n, function(z) vs_simulate_set(rates = if (nrow(rates[[1]]) == 1) rates else list(rates[[1]][n, ], rates[[2]][n, ]), process_model = process_model, serving = serving, go_to = 15, simple = FALSE, id = z, method = "monte carlo")))
        nsims <- length(unique(simres5$id))
    }
    if (nsims/n < 0.98) {
        ## allow up to 2% that didn't reach result
        warning("More than 2% of set5 simulations did not yield a result")
    }
    if (simple) {
        match_prob <- vs_set_probs_to_match(mean(simres14 == 1, na.rm = TRUE), mean(simres5 == 1, na.rm = TRUE)) ## set probs to match prob
        list(pwin = match_prob$pwin, scores = match_prob$scores)
    } else {
        win14 <- pull(dplyr::filter(simres14, .data$team_1_score < 1 & .data$team_2_score < 1), .data$set_won_by)
        win5 <- pull(dplyr::filter(simres5, .data$team_1_score < 1 & .data$team_2_score < 1), .data$set_won_by)
        match_prob <- vs_set_probs_to_match(mean(win14 == 1, na.rm = TRUE), mean(win14 == 1, na.rm = TRUE), mean(win5 == 1, na.rm = TRUE)) ## set probs to match prob
        ## Note: line above is a temporary fix while figuring out how to run the simulation for sets 1/3 and 2/4 separately
        
        list(pwin = match_prob$pwin, scores = match_prob$scores, simres14 = simres14, simres5 = simres5)
    }
}

do_sim_match_theor <- function(rates, process_model, serving, n, simple) {
    ## rates is a list
    so1 <- estimate_sideout_rates(serving = rates[[2]], receiving = rates[[1]])
    so2 <- estimate_sideout_rates(serving = rates[[1]], receiving = rates[[2]])
    out <- win_probabilities_theoretical(c(so1, so2))
    if (isTRUE(simple)) {
        ## reformat out$result_probabilities to match the output from method = "monte carlo"
        out <- as.list(out$result_probabilities)
        list(pwin = out$W, scores = list(`3-0` = out$W30, `3-1` = out$W31, `3-2` = out$W32, `2-3` = out$L32, `1-3` = out$L31, `0-3` = out$L30))
    } else {
        out
    }
}

#' @rdname vs_simulate_match
#' @export
vs_simulate_match_mc <- function(...) {
    vs_simulate_match(..., method = "monte carlo")
}

#' @rdname vs_simulate_match
#' @export
vs_simulate_match_theor <- function(...) {
    vs_simulate_match(..., method = "theoretical")
}
