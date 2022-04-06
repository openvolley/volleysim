set_win_probabilities_theoretical <- function(so) {
    ## so = vector c(P(Team 1 sideouts), P(Team 2 sideouts))
    s.matrix <- matrix(0, 26, 26)
    o.matrix <- matrix(0, 26, 26)

    s.matrix[26, -c(25, 26)] <- 1
    o.matrix[26, -c(25, 26)] <- 1

    rownames(s.matrix) <- rownames(o.matrix) <- colnames(s.matrix) <- colnames(o.matrix) <- 0:25

    denom <- (1 - so[1])^2 + (1 - so[2]) * so[1] * (1 - so[1] + (1 - so[2]))
    s.matrix[25, 25] <- (1 - so[2])^2 / denom
    o.matrix[25, 25] <- (1 - so[2]) * so[1] * ((1 - so[1]) + (1 - so[2])) / denom

    for (k in 47:0) {
        for (i in seq(max(k - 24, 0), min(24, k))) {
            s.matrix[i+1, k-i+1] <- (1 - so[2]) * s.matrix[i+2, k-i+1] + so[2] * o.matrix[i+1, k-i+2]
            o.matrix[i+1, k-i+1] <- so[1] * s.matrix[i+2, k-i+1] + (1 - so[1]) * o.matrix[i+1, k-i+2]
        }
    }

    s.matrix[26, 25] <- s.matrix[25, 24]
    s.matrix[25, 26] <- s.matrix[24, 25]
    s.matrix[26, 26] <- s.matrix[25, 25]
    o.matrix[25, 26] <- o.matrix[24, 25]
    o.matrix[26, 25] <- o.matrix[25, 24]
    o.matrix[26, 26] <- o.matrix[25, 25]

    list(s.matrix = s.matrix, o.matrix = o.matrix)
}

win_probabilities_theoretical <- function(so, serve1_start = NA, serve5_start = NA, go_to = 25, go_to_tiebreak = 15, max_sets = 5) {
    ## create P(Team 1 Wins Set) and P(Team 1 Wins Match) as a function of
    ## so = vector c(P(Team 1 sideouts), P(Team 2 sideouts))
    ## serve1_start = TRUE if Team 1 starts match with serve, FALSE if Team 2 starts match with serve
    ## serve5_start = TRUE if Team 1 starts set 5 with serve, FALSE if Team 2 starts set 5 with serve
    ## if serving team(s) is unknown, will average P(Team 1 Wins Set | Serve 0-0) and P(Team 1 Wins Set | Receive 0-0)
    ## returns a list of the following objects:
    ## team_serve: a matrix of set win probabilities given score and Team 1 is serving
    ## opponent_serve: a matrix of set win probabilities given score and Team 2 is serving
    ## start_serve_set1_wins: a vector of match win probabilities given Team 1's results of previous sets and that Team 1 began Set 1 with serve
    ## opponent_serve_set1_wins: a vector of match win probabilities given Team 1's results in previous sets that Team 2 began Set 1 with serve
    ## result_probabilities: a vector of match win/loss probabilities

    assert_that(max_sets %in% c(3,5), msg = "Only 3-set and 5-set matches are supported")
    assert_that(go_to <= 25, go_to_tiebreak <= 25, msg = "Target set points must be at most 25")
    
    m <- set_win_probabilities_theoretical(so)
    
    zero_eq <- 25 - go_to
    zero_eq_tiebreak <- 25 - go_to_tiebreak
    
    pu.s <- m$s.matrix[1 + zero_eq, 1 + zero_eq] ##P(Win | Serve)
    pu.o <- m$o.matrix[1 + zero_eq, 1 + zero_eq] ##P(Win | Opp Serve)
    pu.s5 <- m$s.matrix[1 + zero_eq_tiebreak, 1 + zero_eq_tiebreak] ##P(Win Game 5 | Serve)
    pu.o5 <- m$o.matrix[1 + zero_eq_tiebreak, 1 + zero_eq_tiebreak] ##P(Win Game 5 | Opp Serve)
    
    
    # set chance of winning set 5 based on who starts
    w5 <- dplyr::case_when(
        serve5_start ~ pu.s5,
        !serve5_start ~ pu.o5,
        TRUE ~ 0.5*(pu.s5 + pu.o5)
    )
    
    ## now match outcomes from set outcomes
    wu3.s <- pu.s*pu.o*pu.s
    wu4.s <- 2*(1-pu.s)*pu.o*pu.s*pu.o + pu.s*(1-pu.o)*pu.s*pu.o
    
    ##L1, L2; L1, L4; L2, L3; L3, L4 --- L1, L3; L2, L4
    wu5.s <- w5*(4*(1-pu.s)*pu.o*pu.s*(1-pu.o) + pu.s^2*(1-pu.o)^2 + pu.o^2*(1-pu.s)^2)
    
    lu3.s <- (1 - pu.s)*(1 - pu.o)*(1 - pu.s)
    lu4.s <- 2*pu.s*(1-pu.o)*(1-pu.s)*(1-pu.o) + (1-pu.s)*pu.o*(1-pu.s)*(1-pu.o)
    lu5.s <- (1-w5)*(4*pu.o*(1-pu.s)*(1-pu.o)*pu.s + (1-pu.o)^2*(pu.s)^2 + (1-pu.s)^2*pu.o^2)
    
    wu3.o <- pu.o*pu.s*pu.o
    wu4.o <- 2*(1-pu.o)*pu.s*pu.o*pu.s + pu.o*(1-pu.s)*pu.o*pu.s
    wu5.o <- w5*(4*(1-pu.s)*pu.o*pu.s*(1-pu.o) + pu.s^2*(1-pu.o)^2 + pu.o^2*(1-pu.s)^2)
    
    lu3.o <- (1 - pu.o)*(1 - pu.s)*(1 - pu.o)
    lu4.o <- 2*pu.o*(1-pu.s)*(1-pu.o)*(1-pu.s) + (1-pu.o)*pu.s*(1-pu.o)*(1-pu.s)
    lu5.o <- (1-w5)*(4*pu.s*(1-pu.o)*(1-pu.s)*pu.o + (1-pu.s)^2*(pu.o)^2 + (1-pu.o)^2*pu.s^2)
    
    wu3.s.w1 <- pu.o*pu.s
    wu4.s.w1 <- pu.o*(1-pu.s)*pu.o + (1-pu.o)*pu.s*pu.o
    
    ##L2, L3; L3, L4; L2, L4
    wu5.s.w1 <- w5*(2*(1-pu.o)*(1-pu.s)*pu.o + pu.s*(1-pu.o)^2)
    
    wu3.o.w1 <- pu.s*pu.o
    wu4.o.w1 <- (1-pu.s)*pu.o*pu.s + pu.s*(1-pu.o)*pu.s
    wu5.o.w1 <- w5*(2*(1-pu.s)*(1-pu.o)*pu.s + pu.o*(1-pu.s)^2)
    
    wu4.s.l1 <- pu.o*pu.s*pu.o
    
    ##W2, W3; W3, W4; W2, W4
    wu5.s.l1 <- w5*(2*pu.o*pu.s*(1-pu.o) + pu.o^2*(1-pu.s))
    
    wu4.o.l1 <- pu.s*pu.o*pu.s
    wu5.o.l1 <- w5*(2*pu.s*pu.o*(1-pu.s) + pu.s^2*(1-pu.o))
    
    
    w.s.w1 <- wu3.s.w1 + wu4.s.w1 + wu5.s.w1
    w.o.w1 <- wu3.o.w1 + wu4.o.w1 + wu5.o.w1
    
    w.s.l1 <- wu4.s.l1 + wu5.s.l1
    w.o.l1 <- wu4.o.l1 + wu5.o.l1
    
    ##Game 2 result
    
    wu3.s.w2 <- pu.s
    wu4.s.w2 <- (1-pu.s)*pu.o
    wu5.s.w2 <- w5*((1-pu.s)*(1-pu.o))
    
    wu4.s.wl <- pu.s*pu.o
    wu5.s.wl <- w5*((1-pu.s)*pu.o + pu.s*(1-pu.o))
    
    wu5.s.l2 <- w5*(pu.s*pu.o)
    
    wu3.o.w2 <- pu.o
    wu4.o.w2 <- (1-pu.o)*pu.s
    wu5.o.w2 <- w5*((1-pu.s)*(1-pu.o))
    
    wu4.o.wl <- pu.o*pu.s
    wu5.o.wl <- w5*((1-pu.o)*pu.s + pu.o*(1-pu.s))
    
    wu5.o.l2 <- w5*(pu.o*pu.s)
    
    w.s.w2 <- wu3.s.w2 + wu4.s.w2 + wu5.s.w2
    w.s.wl <- wu4.s.wl + wu5.s.wl
    w.s.l2 <- wu5.s.l2
    
    w.o.w2 <- wu3.o.w2 + wu4.o.w2 + wu5.o.w2
    w.o.wl <- wu4.o.wl + wu5.o.wl
    w.o.l2 <- wu5.o.l2
    
    ##Game 3 result
    
    wu4.s.w2l <- pu.o
    wu5.s.w2l <- w5*(1-pu.o)
    
    wu5.s.wl2 <- w5*pu.o
    
    wu4.o.w2l <- pu.s
    wu5.o.w2l <- w5*(1-pu.s)
    wu5.o.wl2 <- w5*pu.s
    
    w.s.w2l <- wu4.s.w2l + wu5.s.w2l
    w.s.wl2 <- wu5.s.wl2
    
    w.o.w2l <- wu4.o.w2l + wu5.o.w2l
    w.o.wl2 <- wu5.o.wl2
    
    ##Game 4 result
    wu5.w2l2 <- w5
    
    ##unconditional
    wu.s <- pu.s*w.s.w1 + (1-pu.s)*w.s.l1 ##P(win Set 1 | Serve)*P(Win | Win Set 1) + P(Lose Set 1 | Serve)*P(Win Match | Lose Set 1)
    wu.o <- pu.o*w.o.w1 + (1-pu.o)*w.o.l1
    
    ## unconditional, w, l, ww, wl, lw, ll, www, wwl, wlw, lww, wll, lwl, llw, lll
    ## wwlw, wlww, lwww, wwll, wlwl, wllw, llww, lwlw, lwwl, llwl, lwll, wlll

    if (max_sets == 5){
        s.wins <- c(wu.s,w.s.w1, w.s.l1, w.s.w2, w.s.wl, w.s.l2, w.s.w2l, w.s.wl2, wu5.w2l2)
        o.wins <- c(wu.o,w.o.w1, w.o.l1, w.o.w2, w.o.wl, w.o.l2, w.o.w2l, w.o.wl2, wu5.w2l2)
        
        names(s.wins) <- names(o.wins) <- c("U","W","L","WW", "WL", "LL", "WLW", "WLL", "WLWL")
        
        ## overall results - chance of winning/losing in each number of games
        results <- dplyr::case_when(
            serve1_start ~ list(pwin = wu.s,
                                scores = list(
                                    `3-0` = wu3.s,
                                    `3-1` = wu4.s,
                                    `3-2` = wu5.s,
                                    `2-3` = lu5.s,
                                    `1-3` = lu4.s,
                                    `0-3` = lu3.s
                                    
                                )),
            !serve1_start ~ list(pwin = wu.o,
                                 scores = list(
                                     `3-0` = wu3.o,
                                     `3-1` = wu4.o,
                                     `3-2` = wu5.o,
                                     `2-3` = lu5.o,
                                     `1-3` = lu4.o,
                                     `0-3` = lu3.o
                                     
                                 )),
            TRUE ~ list(pwin = (wu.s + wu.o)/2,  # if serve1_start is NA, coin toss hasn't happened, average
                        scores = list(
                            `3-0` = (wu3.s + wu3.o)/2,
                            `3-1` = (wu4.s + wu4.o)/2,
                            `3-2` = (wu5.s + wu5.o)/2,
                            `2-3` = (lu5.s + lu5.o)/2,
                            `1-3` = (lu4.s + lu4.o)/2,
                            `0-3` = (lu3.s + lu3.o)/2
                        ))
        )
        names(results) <- c("pwin", "scores")  # case_when can return a list but the first level names are NA'd out
        
    } else { # Max sets = 3, can only win 2-0 or 2-1
        # This is much easier as no need to worry about serve vs. receive start
        
        s.wins <- c(w.s.wl, w.s.w2l, w.s.wl2, wu5.w2l2)
        o.wins <- c(w.o.wl, w.o.w2l, w.o.wl2, wu5.w2l2)
        names(s.wins) <- names(o.wins) <- c("U","W","L","WL")
        
        wu2 <- pu.s*pu.o
        wu3.3 <- pu.s*(1-pu.o)*w5 + (1-pu.s)*pu.o*w5
        lu2 <- (1-pu.s)*(1-pu.o)
        lu3.3 <- pu.s*(1-pu.o)*(1-w5) + (1-pu.s)*pu.o*(1-w5)
    
        ## overall results - chance of winning/losing in each number of games
        results <- list(pwin = wu2 + wu3.3,
                                scores = list(
                                    `2-0` = wu2,
                                    `2-1` = wu3.3,
                                    `1-2` = lu3.3,
                                    `0-2` = lu2
                                )
                        )
    }
    
    return(list(team_serve = m$s.matrix, opponent_serve = m$o.matrix,
                start_serve_set1_wins = s.wins, opponent_serve_set1_wins = o.wins,
                result_probabilities = results))
}


#' Estimate theoretical sideout rates given 'phase' parameters
#'
#' The [vs_estimate_rates()] function returns a team's performance rates across a range of aspects of play, including serve ace rate, serve error rate, and so on. Using [vs_theoretical_sideout_rates()] We can estimate the theoretical sideout rate that we would expect to see, given those parameters. This can be compared to the actual sideout rate achieved by the team.
#'
#' @param rates list: rates as returned by [vs_estimate_rates()]
#' @param process_model string: currently only "phase". See [vs_estimate_rates()]
#'
#' @return The theoretical sideout rates of the two teams
#'
#' @seealso [vs_estimate_rates()]
#'
#' @examples
#' \dontrun{
#'   library(datavolley)
#'   x <- dv_read(dv_example_file())
#'   rates <- list(vs_estimate_rates(x, target_team = home_team(x)),
#'                 vs_estimate_rates(x, target_team = visiting_team(x)))
#'
#'   ## the theoretical sideout rates
#'   vs_theoretical_sideout_rates(rates)
#'
#'   ## compare to their actual sideout rates
#'   c(rates[[1]]$sideout, rates[[2]]$sideout)
#'}
#' @export
vs_theoretical_sideout_rates <- function(rates, process_model = "phase") {
    ##c(estimate_sideout_rates(rates[[2]], rates[[1]]), estimate_sideout_rates(rates[[1]], rates[[2]]))
    MM <- rates_to_MC(rates, process_model = process_model, target_team = "each")
    c(MC_sideout_rates(rev(MM)), MC_sideout_rates(MM))
}


## no longer used internally, see MC_sideout_rates
estimate_sideout_rates <- function(serving, receiving){

    ## estimate sideout probabilities from rate stats using terminating Markov chain theory
    ## serving and receiving are the rate stats for the serving and receiving teams, respectively
    ## these rate stats are computed by, e.g., vs_simulate_match
    
    
    ## Step 1: create the transition matrix
    ## SW: Serving team wins point
    ## RW: Receiving team wins point
    ## SS: Serving team serves - nothing should transition to this column
    ## RR: Receiving team receives - only SS should transition to this column
    ## RAR: Receiving team attacks off reception
    ## SAT: Serving team attacks in transition
    ## RAT: Receiving team attacks in transition
    ## SD: Serving team digs/freeball reception - note: technically we should have transition from RR and RD to this column to account for overpasses and downballs
    ## RD: Receiving team digs/freeball reception - note: technically we should have transition from SD to this column to account for overpasses and downballs
    A <- data.matrix(
        data.frame(
            SW = c(1, 0, serving$serve_ace, receiving$rec_set_error, (receiving$rec_att_error + serving$rec_block), serving$trans_att_kill, (receiving$trans_att_error + serving$trans_block), 0, receiving$trans_set_error),
            RW = c(0, 1, serving$serve_error, 0, receiving$rec_att_kill, (receiving$trans_block + serving$trans_att_error), receiving$trans_att_kill, serving$trans_set_error, 0),
            SS = rep(0, 9),
            RR = c(0, 0, 1 - serving$serve_ace - serving$serve_error, rep(0, 6)),
            RAR = c(rep(0, 3), 1 - receiving$rec_set_error, rep(0, 5)),
            SAT = c(rep(0, 7), 1 - serving$trans_set_error, 0),
            RAT = c(rep(0, 8), 1 - receiving$trans_set_error),
            SD = c(rep(0, 4), 1 - (receiving$rec_att_kill + receiving$rec_att_error + serving$rec_block + receiving$rec_att_replayed), serving$trans_att_replayed, 1 - (receiving$trans_att_kill + receiving$trans_att_error + serving$trans_block + receiving$trans_att_replayed), 0, 0),
            RD = c(rep(0, 4), receiving$rec_att_replayed, 1 - (serving$trans_att_kill + serving$trans_att_error + receiving$trans_block + serving$trans_att_replayed), receiving$trans_att_replayed, rep(0, 2))
        )
    )
    
    rownames(A) <- c("SW", "RW", "SS", "RR", "RAR", "SAT", "RAT", "SD", "RD")

    ## Step 2: find the asymptotic win probability - since it's a terminating matrix everything has to end up eventually in either SW or RW    
    I_rc <- which(rownames(A) %in% c("SW", "RW"))  # rows and columns of the identity submatrix
    N <- solve(diag(nrow(A) - length(I_rc)) - A[-I_rc, -I_rc])
    R <- A[-I_rc, I_rc]
    
    point_prob <- N%*%R
    
    return(point_prob[1,2]) #receiving team sideout rate is what we want
}


## given two sets of rates, construct markovchain object(s) for one or both teams
# @param name string: the name assigned internally to the object
# @param process_model string: "phase" is as for e.g. vs_simulate_set
rates_to_MC <- function(rates, process_model = "phase", target_team = "each", name = "M") {
    assert_that(is.string(process_model))
    rates <- precheck_rates(rates, process_model = process_model)
    if (is.numeric(target_team)) target_team <- as.character(target_team)
    assert_that(is.string(target_team))
    target_team <- tolower(target_team)
    target_team <- match.arg(target_team, c("each", "1", "2"))
    if (target_team == "each") {
        return(list(rates_to_MC(rates, process_model = process_model, target_team = "1", name = "M1"),
                    rates_to_MC(rates, process_model = process_model, target_team = "2", name = "M2")))
    } else if (target_team == "2") {
        rates <- rev(rates)
    }
    state_names <- switch(tolower(process_model),
                          phase = c("SS", "SS=", "SS#", "RR", "RRO=", "RRA", "RRA=", "RRA#", "RRA/",
                                    "RT", "ST",
                                    "STO=", "STA", "STA=", "STA#", "STA/",
                                    "RTO=", "RTA", "RTA=", "RTA#", "RTA/"),
                          phase_simple = c("SS", "SS=", "SS#", ## serving team serves (SS), serve ace (S#), serve error (S=)
                                           "RR", "RR#", "RR=", ## receiving team receives (RR), wins on reception (R#), loses on reception (R=)
                                           "RT", "ST", ## receiving team in transition (RT), serving team in transition (ST)
                                           "ST#", "ST=",  ## serving team wins in transition (ST#), loses in transition (ST=)
                                           "RT#", "RT="),
                          stop("unrecognized process_model:", process_model)
                          )
    M <- matrix(0, length(state_names), length(state_names))
    colnames(M) <- rownames(M) <- state_names
    if (process_model == "phase") {
        M["SS", "SS="] <- rates[[1]]$serve_error
        M["SS", "SS#"] <- rates[[1]]$serve_ace
        M["SS", "RR"] <- abs(1 - sum(M["SS", ])) ## using abs(1 - ...) will avoid negative probs, which aren't picked up by markovchain
        M["RR", "RRO="] <- rates[[2]]$rec_loss_other
        M["RR", "ST"] <- rates[[2]]$rec_no_att
        M["RR", "RRA"] <- abs(1 - sum(M["RR", ]))
        M["RRA", "RRA="] <- rates[[2]]$rec_att_error
        M["RRA", "RRA#"] <- rates[[2]]$rec_att_kill
        M["RRA", "RRA/"] <- rates[[1]]$rec_block
        M["RRA", "RT"] <- rates[[2]]$rec_att_replayed
        M["RRA", "ST"] <- abs(1 - sum(M["RRA", ]))

        M["ST", "STO="] <- rates[[1]]$trans_loss_other
        M["ST", "RT"] <- rates[[1]]$trans_no_att
        M["ST", "STA"] <- abs(1 - sum(M["ST", ]))
        M["STA", "STA="] <- rates[[1]]$trans_att_error
        M["STA", "STA#"] <- rates[[1]]$trans_att_kill
        M["STA", "STA/"] <- rates[[2]]$trans_block
        M["STA", "ST"] <- rates[[1]]$trans_att_replayed
        M["STA", "RT"] <- abs(1 - sum(M["STA", ]))

        M["RT", "RTO="] <- rates[[2]]$trans_loss_other
        M["RT", "ST"] <- rates[[2]]$trans_no_att
        M["RT", "RTA"] <- abs(1 - sum(M["RT", ]))
        M["RTA", "RTA="] <- rates[[2]]$trans_att_error
        M["RTA", "RTA#"] <- rates[[2]]$trans_att_kill
        M["RTA", "RTA/"] <- rates[[1]]$trans_block
        M["RTA", "RT"] <- rates[[2]]$trans_att_replayed
        M["RTA", "ST"] <- abs(1 - sum(M["RTA", ]))
    } else if (process_model == "phase_simple") {
        M["SS", "SS="] <- rates[[1]]$serve_error
        M["SS", "SS#"] <- rates[[1]]$serve_ace
        M["SS", "RR"] <- abs(1 - sum(M["SS", ]))
        M["RR", "RR="] <- rates[[2]]$rec_loss
        M["RR", "RR#"] <- rates[[2]]$rec_win
        M["RR", "RT"] <- rates[[2]]$rec_replayed
        M["RR", "ST"] <- abs(1 - sum(M["RR", ])) ## no attack or attack in play

        M["ST", "ST="] <- rates[[1]]$trans_loss
        M["ST", "ST#"] <- rates[[1]]$trans_win
        M["ST", "ST"] <- rates[[1]]$trans_replayed
        M["ST", "RT"] <- abs(1 - sum(M["ST", ]))

        M["RT", "RT="] <- rates[[2]]$trans_loss
        M["RT", "RT#"] <- rates[[2]]$trans_win
        M["RT", "RT"] <- rates[[2]]$trans_replayed
        M["RT", "ST"] <- abs(1 - sum(M["RT", ]))
    } else {
        stop("unrecognized process_model:", process_model)
    }
    ## all rally-ending states become sinks
    for (tstate in grep("[/#=]$", state_names)) M[tstate, tstate] <- 1.0
    new("markovchain", states = state_names, transitionMatrix = M, name = name)
}

## MM <- rates_to_MC(rates, process_model = "phase")
## plot(MM[[1]])

MC_sideout_rates <- function(servingM, receivingM) {
    if (is.list(servingM) && (missing(receivingM) || is.null(receivingM)) && length(servingM) == 2 && all(vapply(servingM, inherits, "markovchain", FUN.VALUE = TRUE))) {
        receivingM <- servingM[[2]]
        servingM <- servingM[[1]]
    }
    winR_states <- states(servingM)[grepl("^S.*[=/]$", states(servingM)) | grepl("^R.*#$", states(servingM))]
    sum(absorptionProbabilities(servingM)[1, winR_states])
}

## given the team markovchain objects, construct the serve-sideout-breakpoint markovchain
MC2MCP <- function(M1, M2) {
    if (is.list(M1) && (missing(M2) || is.null(M2)) && length(M1) == 2 && all(vapply(M1, inherits, "markovchain", FUN.VALUE = TRUE))) {
        M2 <- M1[[2]]
        M1 <- M1[[1]]
    }
    ## take per-team MC matrices, calculate sideout rates
    winR_states <- states(M1)[grepl("^S.*[=/]$", states(M1)) | grepl("^R.*#$", states(M1))]
    winS_states <- states(M1)[grepl("^S.*#$", states(M1)) | grepl("^R.*[=/]$", states(M1))]
    so <- c(sum(absorptionProbabilities(M2)[1, winR_states]), sum(absorptionProbabilities(M1)[1, winR_states]))

    ## the markovchain object representing the serve-sideout-breakpoint process
    state_names <- c("1S", "1BP", "2SO", "2S", "2BP", "1SO")
    MM <- matrix(0, length(state_names), length(state_names))
    colnames(MM) <- rownames(MM) <- state_names
    MM["1S", "2SO"] <- so[2] ## team 1 serves, team 2 sides out
    MM["1S", "1BP"] <- 1 - so[2] ## team 1 serves, team 1 wins breakpoint
    MM["2S", "1SO"] <- so[1] ## team 2 serves, team 1 sides out
    MM["2S", "2BP"] <- 1 - so[1] ## team 2 serves, team 2 wins breakpoint
    MM["2SO", "2S"] <- MM["2BP", "2S"] <- MM["1SO", "1S"] <- MM["1BP", "1S"] <- 1.0
    new("markovchain", states = state_names, transitionMatrix = MM, name = "MMP")
}

## relative proportion of serves by each team
MCP_serve_proportions <- function(MMP) {
    if (markovchain::name(MMP) != "MMP") stop("MMP needs to be a markovchain object as returned by MC2MCP")
    sprop <- steadyStates(MMP)[1, c("1S", "2S")]
    sprop/sum(sprop)
}

MC_to_points_breakdown <- function(rates, process_model) {
    M1 <- rates_to_MC(rates, process_model = process_model, target_team = "each")
    so <- c(MC_sideout_rates(rev(M1)), MC_sideout_rates(M1)) ## theor sideout rates
    ## separate the MC objects
    M2 <- M1[[2]]
    M1 <- M1[[1]]
    out <- do_MC_to_points_breakdown(M1, M2, this_team = 1L)
    out2 <- do_MC_to_points_breakdown(M2, M1, this_team = 2L)
    out <- rbind(out, out2)

    ## also do breakdown by points per set
    ## first need the distribution of scores
    ## sprop gives proportion of serves for team 1 and team 2
    sprop <- MCP_serve_proportions(MC2MCP(M1, M2))
    if (FALSE) {
        ## this is much slower than using absorbp
        SM1 <- scoreMC(so = so, serving = 1, go_to = 25, stop_at = 35)
        ## set scores when team 1 is serving
        ss <- states(SM1)[grep("^WIN", states(SM1))]
        set_scores1 <- absorptionProbabilities(SM1)[1, ss]
    } else {
        A1 <- score_transition_matrix(so = so, serving = 1, go_to = 25, stop_at = 35)
        set_scores1 <- absorbp(A1)[1, ]
        set_scores1 <- set_scores1[grep("^WIN", names(set_scores1))]
        ss <- names(set_scores1)
    }
    set_scores1 <- data.frame(p = set_scores1/sum(set_scores1), id = ss)
    temp <- sub(".*@", "", ss)
    set_scores1$team_1_score <- as.integer(sub(":.*", "", temp))
    set_scores1$team_2_score <- as.integer(sub(".*:", "", temp))
    mss1 <- c(sum(set_scores1$team_1_score * set_scores1$p), sum(set_scores1$team_2_score * set_scores1$p))
    ## ss1 is the mean points per set by team, when team 1 starts serving
    A2 <- score_transition_matrix(so = so, serving = 2, go_to = 25, stop_at = 35)
    set_scores2 <- absorbp(A2)[1, ]
    set_scores2 <- set_scores2[grep("^WIN", names(set_scores2))]
    ss <- names(set_scores2)
    set_scores2 <- data.frame(p = set_scores2/sum(set_scores2), id = ss)
    temp <- sub(".*@", "", ss)
    set_scores2$team_1_score <- as.integer(sub(":.*", "", temp))
    set_scores2$team_2_score <- as.integer(sub(".*:", "", temp))
    mss2 <- c(sum(set_scores2$team_1_score * set_scores2$p), sum(set_scores2$team_2_score * set_scores2$p))
    ## combine to overall mss which is mean points per set by team, accounting for sprop
    mss <- c(mss1[1] * sprop[1] + mss2[1] * sprop[2], mss1[2] * sprop[1] + mss2[2] * sprop[2])
    ## now given mss and the res$points_breakdown (proportion_of_team_points) we can now estimate the breakdown in terms of points_per_set
    out$points_per_set <- NA_real_
    out$points_per_set[out$point_won_by == 1] <- out$proportion_of_team_points[out$point_won_by == 1] * mss[1]
    out$points_per_set[out$point_won_by == 2] <- out$proportion_of_team_points[out$point_won_by == 2] * mss[2]

    out
}

do_MC_to_points_breakdown <- function(M1, M2, this_team) {
    stopifnot(identical(states(M1), states(M2)))
    winR_states <- states(M1)[grepl("^S.*[=/]$", states(M1)) | grepl("^R.*#$", states(M1))]
    winS_states <- states(M1)[grepl("^S.*#$", states(M1)) | grepl("^R.*[=/]$", states(M1))]
    ## relative number of serves by team
    sprop <- MCP_serve_proportions(MC2MCP(M1, M2))
    ## relative proportion of points won by team 1 in serve/reception, scaled by their serve proportion
    T1points <- c(absorptionProbabilities(M1)[1, winS_states] * sprop[1], absorptionProbabilities(M2)[1, winR_states] * sprop[2])
    names(T1points) <- substr(names(T1points), 2, 4)
    temp <- list()
    for (uu in unique(names(T1points))) temp[[uu]] <- sum(T1points[names(T1points) == uu])
    temp <- unlist(temp)
    out <- data.frame(point_won_by = this_team, outcome = states_to_factor(names(temp)), proportion_of_team_points = unname(temp)/sum(temp), proportion_of_all_points = unname(temp), stringsAsFactors = FALSE)
    out <- out[order(out$outcome), ]
    out$outcome <- as.character(out$outcome)
    rownames(out) <- NULL
    out
}
states_as_factors <- function() c("S=" = "Serve error", "S#" = "Serve ace", "RO=" = "Rec loss other", "RA=" = "Rec attack error", "RA/" = "Rec attack block", "RA#" = "Rec attack kill", "R=" = "Rec loss", "R#" = "Rec win", "TO=" = "Trans loss other", "TA=" = "Trans attack error", "TA#" = "Trans attack kill", "TA/" = "Trans attack block", "T=" = "Trans loss", "T#" = "Trans win")
states_to_factor <- function(s) {
    remap <- states_as_factors()
    for (us in na.omit(unique(s))) if (us %in% names(remap)) s[which(s == us)] <- remap[names(remap) == us]
    factor(s, levels = remap)
}

#' Create a win probability graph for a match
#'
#' @param pbp data frame: a data frame containing the set number, home team, visiting team, serving team, point-winning team, home team score, and visiting team score at the end of each point,
#'  easiest to obtain by subsetting the plays component of a datavolley object as returned by [datavolley::dv_read()] to only include rows where `point == TRUE`
#' @param so integer: a two-element vector of sideout rates for the home team and visiting team, easiest to obtain using [vs_estimate_rates()]
#' @param go_to integer: the minimum score that must be reached to end a non-tiebreaker set (typically 25 for indoor volleyball in sets 1 to 4, or 21 in beach volleyball)
#' @param go_to_tiebreak integer: the minimum score that must be reached to end a tiebreaker set (typically 15)
#' @param max_sets integer: the maximum number of sets that can be played, either 3 or 5
#' @param show_plot logical: if `TRUE`, produce a graph showing the home team's win probability at each point in the match
#' @param home_color string: the color used to indicate points in the match where the home team is favored to win
#' @param visiting_color string: the same as `home_color`, but for the visiting team
#'
#' @return A data frame containing the home team's probability of winning the set (`set_probs`) and match (`match_probs`) at each point in the set. 
#' The first row of the data frame refers to the start of the match (0-0, Set 1).
#'
#' @examples
#' \dontrun{
#'   library(datavolley)
#'   x <- dv_read(vs_example_file())
#'   sideout_rates <- vs_estimate_rates(x, target_team = "each")$sideout
#'   play_by_play <- subset(plays(x), point)
#'   
#'   vs_match_win_probability(play_by_play, sideout_rates)  ## data frame is not printed to console
#'   ## but can be stored in a variable
#'   match_win_probs <- vs_match_win_probability(play_by_play, sideout_rates)
#'}
#' @export
#' 
vs_match_win_probability <- function(pbp, so, go_to = 25, go_to_tiebreak = 15, max_sets = 5, show_plot = TRUE, home_color = "blue", visiting_color = "darkred"){

    assert_that("set_number" %in% names(pbp), msg = "Set number not detected - please rename the set column set_number")
    assert_that("serving_team" %in% names(pbp), msg = "Serving team not detected - please rename the serving team column serving_team")
    assert_that("point_won_by" %in% names(pbp), msg = "Point-winning team not detected - please rename the column containing the team winning the point to point_won_by")

    assert_that(max_sets %in% c(3,5), msg = "Only 3-set and 5-set matches are supported")
    assert_that(go_to <= 25, go_to_tiebreak <= 25, msg = "Target set points must be at most 25")
    assert_that(max(pbp$set_number) <= max_sets, msg = "More sets have been played in this match than maximum number of sets allowed")

    # fix potential issues with column names not matching exactly
    if(!("visiting_team" %in% names(pbp)) && ("away_team" %in% names(pbp))){
        pbp <- dplyr::rename(pbp, visiting_team = "away_team")
    }

    if(!("visiting_team_score" %in% names(pbp)) && (("away_team_score" %in% names(pbp)))){
        pbp <- dplyr::rename(pbp, visiting_team_score = "away_team_score")
    } else if(!("visiting_team_score" %in% names(pbp)) && (("away_score" %in% names(pbp)))){
        pbp <- dplyr::rename(pbp, visiting_team_score = "away_score")
    } else if(!("visiting_team_score" %in% names(pbp)) && (("visiting_score" %in% names(pbp)))){
        pbp <- dplyr::rename(pbp, visiting_team_score = "visiting_score")
    }

    if(!("home_team_score" %in% names(pbp)) && ("home_score" %in% names(pbp))){
        pbp <- dplyr::rename(pbp, home_team_score = "home_score")
    }

    # Now let's add some asserts after hopefully fixing the issue
    assert_that("home_team" %in% names(pbp), msg = "Home team not detected - please rename the home team column home_team")
    assert_that("visiting_team" %in% names(pbp), msg = "Visiting team not detected - please rename the visiting team column visiting_team")
    assert_that("home_team_score" %in% names(pbp), msg = "Home team score not detected - please rename the home team score column home_team_score")
    assert_that("visiting_team_score" %in% names(pbp), msg = "Visiting team score not detected - please rename the visiting team score column visiting_team_score")

    # Find the start and end points of each set
    n <- nrow(pbp)
    set.start.points <- c(1, (which(diff(pbp$set_number) != 0) - 1))
    set.end.points <- c(which(diff(pbp$set_number)!=0), n)

    # Figure out who serves at start of match/set 5
    match_start_serve <- pbp$serving_team[1] == pbp$home_team[1] # TRUE if home team starts match with serve
    set5_start_serve <- case_when(
        length(set.start.points) == 5 ~ pbp$serving_team[set.start.points[5]] == pbp$home_team[set.start.points[5]],
        length(set.start.points) == 3 & max_sets == 3 ~ pbp$serving_team[set.start.points[5]] == pbp$home_team[set.start.points[5]],
        TRUE ~ NA
    )

    wp_list <- win_probabilities_theoretical(so, 
                                             serve1_start = match_start_serve,
                                             serve5_start = set5_start_serve,
                                             go_to = go_to,
                                             go_to_tiebreak = go_to_tiebreak)

    zero_eq <- 25 - go_to
    zero_eq_tiebreak <- 25 - go_to_tiebreak
    
    wp_df <- pbp %>% group_by(.data$set_number) %>% mutate(
        next_serve = lead(.data$serving_team),
        index.left = 1 + zero_eq + .data$home_team_score + (.data$set_number == max_sets)*zero_eq_tiebreak,
        index.right = 1 + zero_eq + .data$visiting_team_score + (.data$set_number == max_sets)*zero_eq_tiebreak
    ) %>%
        mutate(
            next_serve = if_else(is.na(.data$next_serve), .data$point_won_by, .data$next_serve),
            index.left2 = if_else(.data$index.left > 26 | .data$index.right > 26, 24 + pmax(0, .data$home_team_score - .data$visiting_team_score), .data$index.left),
            index.right2 = if_else(.data$index.left > 26 | .data$index.right > 26, 24 + pmax(0, .data$visiting_team_score - .data$home_team_score), .data$index.right)
        ) %>%
        mutate(
            set_probs = if_else(.data$point_won_by == .data$home_team, 
                                     diag(wp_list$team_serve[.data$index.left2, .data$index.right2]),  # these end up being matrices so have to take the diagonal values to get the right numbers
                                     diag(wp_list$opponent_serve[.data$index.left2, .data$index.right2])
                                     )
        ) %>% ungroup()

    serve_list <- if(match_start_serve) wp_list$start_serve_set1_wins else wp_list$opponent_serve_set1_wins
    
    # need to know how many sets have been won by each team at this point in the match
    set_winners <- wp_df %>% slice(set.end.points) %>%
        transmute(
            .data$set_number,
            set_won_by = if_else(.data$home_team == .data$point_won_by, .data$home_team, .data$visiting_team),
            home_team_sets_won = lag(cumsum(.data$set_won_by == .data$home_team), default = 0),
            visiting_team_sets_won = lag(cumsum(.data$set_won_by == .data$visiting_team), default = 0)
        ) %>% right_join(wp_df, by = "set_number")
    
    if(max_sets == 5){ # Max sets is 5, normal for indoor
        match_winners <- set_winners %>% transmute(
            .data$home_team,
            .data$visiting_team,
            .data$serving_team,
            .data$set_number,
            .data$home_team_score,
            .data$visiting_team_score,
            .data$set_probs,
            match_probs = case_when(
                .data$home_team_sets_won == 0 & .data$visiting_team_sets_won == 0 ~ .data$set_probs*serve_list[2] + (1 - .data$set_probs)*serve_list[3],
                .data$home_team_sets_won == 1 & .data$visiting_team_sets_won == 0 ~ .data$set_probs*serve_list[4] + (1 - .data$set_probs)*serve_list[5],
                .data$home_team_sets_won == 0 & .data$visiting_team_sets_won == 1 ~ .data$set_probs*serve_list[5] + (1 - .data$set_probs)*serve_list[6],
                .data$home_team_sets_won == 1 & .data$visiting_team_sets_won == 1 ~ .data$set_probs*serve_list[7] + (1 - .data$set_probs)*serve_list[8],
                .data$home_team_sets_won == 2 & .data$visiting_team_sets_won == 0 ~ .data$set_probs + (1 - .data$set_probs)*serve_list[7],
                .data$home_team_sets_won == 0 & .data$visiting_team_sets_won == 2 ~ .data$set_probs*serve_list[8],
                .data$home_team_sets_won == 2 & .data$visiting_team_sets_won == 1 ~ .data$set_probs + (1 - .data$set_probs)*serve_list[9],
                .data$home_team_sets_won == 1 & .data$visiting_team_sets_won == 2 ~ .data$set_probs*serve_list[9],
                .data$home_team_sets_won == 2 & .data$visiting_team_sets_won == 2 ~ .data$set_probs,
                TRUE ~ NA_real_ # if there is a problem here, just output NA and move on
            )
        )
    } else { #max_sets is 3, equivalent to starting in set 3 with WL
        match_winners <- set_winners %>% transmute(
            .data$home_team,
            .data$visiting_team,
            .data$serving_team,
            .data$set_number,
            .data$home_team_score,
            .data$visiting_team_score,
            .data$set_probs,
            match_probs = case_when(
                .data$home_team_sets_won == 0 & .data$visiting_team_sets_won == 0 ~ .data$set_probs*serve_list[7] + (1 - .data$set_probs)*serve_list[8],
                .data$home_team_sets_won == 1 & .data$visiting_team_sets_won == 0 ~ .data$set_probs + (1 - .data$set_probs)*serve_list[9],
                .data$home_team_sets_won == 0 & .data$visiting_team_sets_won == 1 ~ .data$set_probs*serve_list[9],
                .data$home_team_sets_won == 1 & .data$visiting_team_sets_won == 1 ~ .data$set_probs,
                TRUE ~ NA_real_ # if there is a problem here, just output NA and move on
            )
        )
        
    }
    
    # add the initial row for 0-0 at the beginning of the match
    match_start <- match_winners[1,] %>% mutate(
        home_team_score = 0,
        visiting_team_score = 0,
        set_probs = if(match_start_serve) wp_list$team_serve[1 + zero_eq, 1 + zero_eq] else wp_list$opponent_serve[1 + zero_eq, 1 + zero_eq],
        match_probs = if(max_sets == 5) serve_list[1] else serve_list[5]
    )
    match_winners <- bind_rows(match_start, match_winners) %>%
        dplyr::arrange(.data$set_number, .data$visiting_team_score, .data$home_team_score)
    # the above line should fix issues of points getting out of order, assuming only one line per point
    
    # if we make the win probability graph, make it
    if(show_plot){
        
        yaxis.label <- paste0("P(", pbp$home_team[1], " Win)")
        title.label <- paste0(pbp$home_team[1], " vs. ", pbp$visiting_team[1])
        
        # Quick plot that can be made nicer with ggplot if desired
        par(xaxt = "n")
        plot(x = seq(0, nrow(match_winners) - 1),
             y = match_winners$match_probs,
             type = "n",
             xlab = "",
             ylab = yaxis.label,
             main = title.label,
             ylim = c(0,1))
        s <- seq(nrow(match_winners) - 1)
        x <- c(0, s)
        segments(x[s], match_winners$match_probs[s], x[s+1], match_winners$match_probs[s+1],
                 col = (if_else(lead(match_winners$match_probs) >= 0.5, home_color, visiting_color)),
                 lwd = 2)
        abline(v = set.end.points[-length(set.end.points)], lty = 2)  # dashed lines at set breaks
        abline(v = c(0, set.end.points[length(set.end.points)]), lty = 2)  # dashed lines at start and end of match too
        
        # Add set number labels
        set.labels <- paste0("Set ", seq(1, length(set.end.points)))
        # essentially, put the set number at top margin of graph
        set.mid.points <- (set.start.points + set.end.points)/2
        for (i in 1:length(set.mid.points)){
         mtext(set.labels[i], side = 3, at = set.mid.points[i])   
        }
    }
    
    invisible(match_winners) # only return the data frame if there is somewhere to return to

}

## functions to build a markov chain of score transitions through a set
sshash <- function(score1, score2, serving) serving * 10e4 + score1 * 10e2 + score2
## what outcomes can we get from a given point?
point_transition <- function(p) {
    ## parms are c(starting_score1, starting_score2, so1, so2, serving, go_to, stop_at, current_index)
    stopifnot(length(p) == 8)
    t1won <- ifelse(p[5] < 2, 1 - p[4], p[3])
    m <- matrix(c(p[1], p[1], ## col 1 = starting team_1_score
                  p[2], p[2], ## 2 = starting team_2_score
                  p[5], p[5], ## 3 = starting serving team
                  p[1] + c(1, 0), ## 4 = next team_1_score
                  p[2] + c(0, 1), ## 5 = next team_2_score
                  t1won, 1 - t1won, ## 6 = prob
                  1, 2, ## 7 = result - who won / is serving at the start of the next point
                  sshash(p[1] + c(0, 0), p[2] + c(0, 0), p[5]), ## 8 = "hash" of starting scores and serving team
                  rep(p[8], 2)), ## 9 = current_index
                ncol = 9)
    ## other 'result' values
    m[m[, 4] >= p[7] | m[, 5] >= p[7], 7] <- 0 ## don't continue
    m[m[, 4] > (m[, 5] + 1) & m[, 4] >= p[6], 7] <- 91 ## team 1 won
    m[m[, 5] > (m[, 4] + 1) & m[, 5] >= p[6], 7] <- 92 ## team 2 won
    m
}

## generate a long-form matrix representing score transitions
score_transition_long_matrix <- function(so, serving = 1, go_to = 25, stop_at = 35, start_scores = c(0, 0)) {
    p <- c(start_scores, so, serving, go_to, stop_at, 0) ## starting params
    ## preallocate
    M <- matrix(NA_real_, nrow = 10000, ncol = 9)
    qi <- c(1L) ## index of item in Q to process
    seen <- c(0) ## hashes that we've queued/processed (don't queue again)
    Q <- list(p) ## queue of items
    i <- 1L ## index into M
    while (length(qi) > 0) {
        ## find the transitions from this set of starting_scores and serving team
        res <- point_transition(Q[[qi[1]]])
        M[c(i, i + 1L), ] <- res
        ## add to the queue any result rows representing a continuing game (not won, not aborted)
        ## also use the next score hash to avoid adding already-seen starting_score/serving team combos
        next_hashes <- sshash(res[, 4], res[, 5], res[, 7])
        res_propagate <- res[, 7] > 0 & res[, 7] < 3 & !next_hashes %in% seen
        res[, 8] <- next_hashes ## the starting hashes for these are the "next" hashes from the point_transition output
        res[, 9] <- c(i, i + 1L) ## update "from" for these
        nextq <- res[res_propagate, c(4:5, 7:9), drop = FALSE] ## cols 1,2 = next starting scores, 3 = result (next serving team), 4 = hash, 5 = from
        seen <- c(seen, nextq[, 4])
        i <- i + 2L
        qi <- qi[-1]
        if (nrow(nextq) > 0) {
            ## cat("\nnext_hashes was: ", capture.output(str(next_hashes)), "\n")
            ## cat("res_propagate was: ", capture.output(str(res_propagate)), "\n")
            ## cat("M contains hashes: ", paste(na.omit(M[, 8]), collapse = ", "), "\n")
            ## cat("adding hashes to Q: ", paste(nextq[, 4], collapse = ", "), "\n")
            Q <- c(Q, lapply(seq_len(nrow(nextq)), function(z) c(nextq[z, 1:2], so, nextq[z, 3], go_to, stop_at, nextq[z, 5])))
            qi <- c(qi, length(Q) + 1 - seq_len(nrow(nextq)))
        }
    }
    colnames(M) <- c("starting_team_1_score", "starting_team_2_score", "starting_serving_team", "team_1_score", "team_2_score", "prob", "result", "score_hash", "from")
    M[seq_len(i - 1L), ]
}

score_transition_matrix <- function(so, serving = 1, go_to = 25, stop_at = 35, start_scores = c(0, 0)) {
    M <- score_transition_long_matrix(so = so, serving = serving, go_to = go_to, stop_at = stop_at, start_scores = start_scores)
    M <- as.data.frame(M)
    M$rowid <- seq_len(nrow(M))
    M$from_id <- paste0(M$starting_serving_team, "@", M$starting_team_1_score, ":", M$starting_team_2_score)
    M$to_id <- M$result
    M$to_id[M$result == 91] <- "WIN1"
    M$to_id[M$result == 92] <- "WIN2"
    M$to_id <- paste0(M$to_id, "@", M$team_1_score, ":", M$team_2_score)
    M$to_id[M$result < 1] <- "NO RESULT"
    all_id <- sort(unique(c(M$from_id, M$to_id)))
    temp <- setNames(seq_along(all_id), all_id)
    from_idn <- unname(temp[M$from_id])
    to_idn <- unname(temp[M$to_id])
    A <- matrix(0, nrow = length(all_id), ncol = length(all_id))
    for (i in seq_len(nrow(M))) {
        A[from_idn[i], to_idn[i]] <- A[from_idn[i], to_idn[i]] + M$prob[i]
    }
    ## sink states
    for (ss in grep("^(WIN|NO RESULT)", all_id)) A[ss, ss] <- 1.0
    colnames(A) <- rownames(A) <- all_id
    ## rearrange to canonical form
    aidx <- diag(A) == 1 ## absorbing states
    aidx <- c(which(!aidx), which(aidx))
    A[aidx, aidx]
}

scoreMC <- function(so, serving = 1, go_to = 25, stop_at = 35) {
    A <- score_transition_matrix(so = so, serving = serving, go_to = go_to, stop_at = stop_at)
    new("markovchain", states = rownames(A), transitionMatrix = A, name = "SCM") ## note that this is fairly slow with a large transition matrix
}

## faster direct implementation of absorption probs
absorbp <- function(A) {
    aidx <- which(diag(A) == 1)
    stopifnot(all(diff(aidx) == 1), max(aidx) == nrow(A))
    Q <- A[seq_len(aidx[1] - 1L), seq_len(aidx[1] - 1L)]
    R <- A[seq_len(aidx[1] - 1L), aidx]
    N <- solve(diag(1, nrow(Q), ncol(Q)) - Q)
    N %*% R
}
