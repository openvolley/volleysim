#' Simulate a set of volleyball
#'
#' @param rates list: A two-element list, each element of which is a set of rates as returned by \code{vs_estimate_rates}
#' @param process_model string: either "sideout" or "phase". Details TBD
#' @param serving logical: if \code{TRUE}, team 1 will serve first. If \code{NA}, the team serving first will be chosen at random
#' @param go_to integer: the minimum score that must be reached to end the set (typically 25 for indoor volleyball in sets 1 to 4, 15 in set 5, or 21 in beach volleyball)
#' @param simple logical: if \code{TRUE}, just return the team (1 or 2) that won. If \code{FALSE}, return extra details in a data.frame
#' @param id : an optional value that (if non-\code{NULL}) will be returned in the \code{id} column of the returned data frame, if \code{simple} is \code{FALSE}
#'
#' @return Integer (1 or 2) or a data frame, depending on the value of \code{simple}
#'
#' @seealso \code{\link{vs_estimate_rates}} \code{\link{vs_simulate_match}}
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
vs_simulate_set <- function(rates, process_model = "phase", serving = NA, go_to = 25, simple = FALSE, id = NULL) {
    assert_that(is.string(process_model))
    process_model <- match.arg(tolower(process_model), c("phase", "sideout"))
    assert_that(is.flag(simple), !is.na(simple))
    if (missing(serving) || is.na(serving)) serving <- runif(1) > 0.5 ## random
    assert_that(is.flag(serving), !is.na(serving))
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
    ## rates are list(
    ##  ## for team 1, probs
    ##  list(serve_ace = z, serve_error = z,
    ##       rec_att_error = z, rec_att_kill = z,
    ##       trans_att_error = z, trans_att_kill = z,
    ##       rec_block = z, trans_block = z),
    ##  list(same for team 2))
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
    if (!simple) outcome <- rep(NA_character_, 100)
    tm_srv <- rep(NA_integer_, 100)
    ptr <- 1 ## pointer into those vectors
    tm_scores[1, ] <- c(0L, 0L) ## scores for team 1, team 2
    srv <- if (serving) 1L else 2L ## serving TRUE => team 1 starts serving
    tm_srv[1] <- srv
    sc <- tm_scores[ptr, ]
    while (all(sc < go_to) || abs(diff(sc)) < 2) {
        tm_scores[ptr + 1, ] <- tm_scores[ptr, ] ## scores at the START of the next point
        this_rates <- rates[[srv]] ## serving team's rates
        other_rates <- rates[[3-srv]] ## other team's rates
        ## the simulation process is basically a hard-coded set of if-else statements here
        ## this should be replaced by something more flexible and configurable, and able to cope with a more highly parameterized simulation model
        if (process_model == "sideout") {
            lost_serve <- tm2_prandf() <= other_rates$sideout
        } else {
            ## serve
            serve_outc <- sum(tm1_prandf() <= cumsum(c(this_rates$serve_ace, this_rates$serve_error)))
            if (serve_outc == 2) {
                lost_serve <- FALSE
                if (!simple) outcome[ptr] <- "Serve ace"
            } else if (serve_outc == 1) {
                lost_serve <- TRUE
                if (!simple) outcome[ptr] <- "Serve error"
            } else {
                ## rec attack by non-serving (other) team
                temp <- c(other_rates$rec_att_kill, other_rates$rec_att_error, other_rates$rec_set_error, this_rates$rec_block)
                if (sum(temp) > 1) stop("The reception-phase probabilities sum to more than 1")
                ra_outc <- sum(tm2_prandf() <= cumsum(temp))
                if (ra_outc == 4) {
                    lost_serve <- TRUE
                    if (!simple) outcome[ptr] <- "Rec attack kill"
                } else if (ra_outc == 3) {
                    lost_serve <- FALSE
                    if (!simple) outcome[ptr] <- "Rec attack error"
                } else if (ra_outc == 2) {
                    lost_serve <- FALSE
                    if (!simple) outcome[ptr] <- "Rec set error"
                } else if (ra_outc == 1) {
                    lost_serve <- FALSE
                    if (!simple) outcome[ptr] <- "Rec attack block"
                } else {
                    ## transition - iterate back and forth between teams until someone wins the point
                    lost_serve <- NA
                    tptr <- 1L ## serving team gets first transition attack opportunity
                    while (is.na(lost_serve)) {
                        if (tptr < 2) {
                            temp <- c(this_rates$trans_att_kill, this_rates$trans_att_error, this_rates$trans_set_error, other_rates$trans_block)
                            if (sum(temp) > 1) stop("The transition-phase probabilities sum to more than 1")
                            ta_outc <- sum(tm1_prandf() <= cumsum(temp))
                            if (ta_outc == 4) {
                                lost_serve <- FALSE
                                if (!simple) outcome[ptr] <- "Trans attack kill"
                            } else if (ta_outc == 3) {
                                lost_serve <- TRUE
                                if (!simple) outcome[ptr] <- "Trans attack error"
                            } else if (ta_outc == 2) {
                                lost_serve <- TRUE
                                if (!simple) outcome[ptr] <- "Trans set error"
                            } else if (ta_outc == 1) {
                                lost_serve <- TRUE
                                if (!simple) outcome[ptr] <- "Trans attack block"
                            }
                        } else {
                            temp <- c(other_rates$trans_att_kill, other_rates$trans_att_error, other_rates$trans_set_error, this_rates$trans_block)
                            if (sum(temp) > 1) stop("The transition-phase probabilities sum to more than 1")
                            ta_outc <- sum(tm2_prandf() <= cumsum(temp))
                            if (ta_outc == 4) {
                                lost_serve <- TRUE
                                if (!simple) outcome[ptr] <- "Trans attack kill"
                            } else if (ta_outc == 3) {
                                lost_serve <- FALSE
                                if (!simple) outcome[ptr] <- "Trans attack error"
                            } else if (ta_outc == 2) {
                                lost_serve <- FALSE
                                if (!simple) outcome[ptr] <- "Trans set error"
                            } else if (ta_outc == 1) {
                                lost_serve <- FALSE
                                if (!simple) outcome[ptr] <- "Trans attack block"
                            }
                        }
                        tptr <- 3L - tptr ## other team now in transition attack phase
                    }
                }
            } ## if-else method
        }
        if (lost_serve) {
            srv <- 3L - srv ## sided out, change server
        }
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
        out$point_won_by <- (lead(tm_scores[, 2]) > tm_scores[, 2])+1
        out$point_won_by[nrow(out)] <- which.max(sc)
        out$set_won_by <- which.max(sc)
        out$outcome <- outcome[seq_len(ptr - 1)]
        if (!is.null(id)) out$id <- id
        out
    }
}

## not exported
## given the probability of winning sets 1-4, and set 5, calculate the overall probability of winning the match
## currently best-of-5-sets only
vs_set_probs_to_match <- function(sp14, sp5 = sp14) {
    ## all possible set outcomes in a 5-set match
    tposs <- matrix(c(c(1, 1, 1, NA_real_, NA_real_), ## 3-0
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
    tposs <- rbind(tposs, 1-tposs)
    pwinsc <- c(pwinsc, 3+pwinsc)
    tposs[,1:4] <- abs(1-tposs[, 1:4]-sp14)
    tposs[,5] <- abs(1-tposs[, 5]-sp5)
    temp <- apply(tposs, 1, prod, na.rm = TRUE) ## prob of each of the possible ways to win
    list(pwin = sum(temp[1:10]), scores = list("3-0" = temp[1], "3-1" = sum(temp[pwinsc==1]), "3-2" = sum(temp[pwinsc==2]), "2-3" = sum(temp[pwinsc==5]), "1-3" = sum(temp[pwinsc==4]), "0-3" = sum(temp[pwinsc==3])))
}


#' Simulate a volleyball match
#'
#' Currently hard-coded to indoor, best-of-5-set scoring.
#'
#' @param rates list: A two-element list, each element of which is a set of rates as returned by \code{vs_estimate_rates}
#' @param process_model string: either "sideout" or "phase". Details TBD
#' @param serving logical: if \code{TRUE}, team 1 will serve first. If \code{NA}, the team serving first will be chosen at random
#' @param n integer: the number of simulations to run
#' @param simple logical: if \code{TRUE}, just return the probability of team winning and the probabilities of each possible set score. If \code{FALSE}, return extra details in a named list
#' @seealso \code{\link{vs_estimate_rates}} \code{\link{vs_simulate_match}}
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
vs_simulate_match <- function(rates, process_model = "phase", serving = NA, n = 2000, simple = FALSE) {
    assert_that(is.flag(simple), !is.na(simple))
    if (simple) {
        simres14 <- sapply(1:n, function(z) vs_simulate_set(rates = rates, process_model = process_model, serving = serving, go_to = 25, simple = TRUE))
        nsims <- sum(!is.na(simres14))
    } else {
        simres14 <- bind_rows(lapply(1:n, function(z) vs_simulate_set(rates = rates, process_model = process_model, serving = serving, go_to = 25, simple = FALSE, id = z)))
        nsims <- length(unique(simres14$id))
    }
    if (nsims/n < 0.98) {
        ## allow up to 2% that didn't reach result
        warning("More than 2% of set1-4 simulations did not yield a result")
    }
    if (simple) {
        simres5 <- sapply(1:n, function(z) vs_simulate_set(rates = rates, process_model = process_model, serving = serving, go_to = 15, simple = TRUE))
        nsims <- sum(!is.na(simres5))
    } else {
        simres5 <- bind_rows(lapply(1:n, function(z) vs_simulate_set(rates = rates, process_model = process_model, serving = serving, go_to = 15, simple = FALSE, id = z)))
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
        match_prob <- vs_set_probs_to_match(mean(win14 == 1, na.rm = TRUE), mean(win5 == 1, na.rm = TRUE)) ## set probs to match prob
        list(pwin = match_prob$pwin, scores = match_prob$scores, simres14 = simres14, simres5 = simres5)
    }
}
