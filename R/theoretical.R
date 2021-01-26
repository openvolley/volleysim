set_win_probabilities_theoretical <- function(so) {
    ## so = vector c(P(Team 1 sideouts), P(Team 2 sideouts))
    s.matrix <- matrix(0, 26, 26)
    o.matrix <- matrix(0, 26, 26)

    s.matrix[26,-c(25,26)] <- 1
    o.matrix[26,-c(25,26)] <- 1

    rownames(s.matrix) <- seq(0,25)
    rownames(o.matrix) <- seq(0,25)
    colnames(s.matrix) <- seq(0,25)
    colnames(o.matrix) <- seq(0,25)

    back <- seq(47, 0, by = -1)

    denom <- (1-so[1])^2 + (1-so[2])*so[1]*(1-so[1]+(1-so[2]))
    s.matrix[25, 25] <- (1-so[2])^2/denom
    o.matrix[25, 25] <- (1-so[2])*so[1]*((1-so[1])+(1-so[2]))/denom

    for (k in back) {
        for (i in seq(max(k-24,0), min(24,k))){
            s.matrix[i+1, k-i+1] <- (1-so[2])*s.matrix[i+2, k-i+1] + so[2]*o.matrix[i+1, k-i+2]
            o.matrix[i+1, k-i+1] <- so[1]*s.matrix[i+2, k-i+1] + (1-so[1])*o.matrix[i+1, k-i+2]
        }
    }

    s.matrix[26,25] <- s.matrix[25,24]
    s.matrix[26,26] <- s.matrix[25,25]
    o.matrix[25,26] <- o.matrix[24,25]
    o.matrix[26,26] <- o.matrix[25,25]

    list(s.matrix = s.matrix, o.matrix = o.matrix)
}

win_probabilities_theoretical <- function(so, serve1_start = NA, serve5_start = NA) {
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

    m <- set_win_probabilities_theoretical(so)
    pu.s <- m$s.matrix[1,1] ##P(Win | Serve)
    pu.o <- m$o.matrix[1,1] ##P(Win | Opp Serve)
    pu.s5 <- m$s.matrix[11,11] ##P(Win Game 5 | Serve)
    pu.o5 <- m$o.matrix[11,11] ##P(Win Game 5 | Opp Serve)
    
    
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

    return(list(team_serve = m$s.matrix, opponent_serve = m$o.matrix,
                start_serve_set1_wins = s.wins, opponent_serve_set1_wins = o.wins,
                result_probabilities = results))
}


#' Estimate theoretical sideout rates given 'phase' parameters
#'
#' The [vs_estimate_rates()] function returns a team's performance rates across a range of aspects of play, including serve ace rate, serve error rate, and so on. Using [vs_theoretical_sideout_rates()] We can estimate the theoretical sideout rate that we would expect to see, given those parameters. This can be compared to the actual sideout rate achieved by the team.
#'
#' @param rates list: rates as returned by [vs_estimate_rates()]
#' @param process_model string: "phase" or "phase_simple"
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
        M["SS", "RR"] <- 1 - sum(M["SS", ])
        M["RR", "RRO="] <- rates[[2]]$rec_loss_other
        M["RR", "ST"] <- rates[[2]]$rec_no_att
        M["RR", "RRA"] <- 1 - sum(M["RR", ])
        M["RRA", "RRA="] <- rates[[2]]$rec_att_error
        M["RRA", "RRA#"] <- rates[[2]]$rec_att_kill
        M["RRA", "RRA/"] <- rates[[1]]$rec_block
        M["RRA", "RT"] <- rates[[2]]$rec_att_replayed
        M["RRA", "ST"] <- 1 - sum(M["RRA", ])

        M["ST", "STO="] <- rates[[1]]$trans_loss_other
        M["ST", "RT"] <- rates[[1]]$trans_no_att
        M["ST", "STA"] <- 1 - sum(M["ST", ])
        M["STA", "STA="] <- rates[[1]]$trans_att_error
        M["STA", "STA#"] <- rates[[1]]$trans_att_kill
        M["STA", "STA/"] <- rates[[2]]$trans_block
        M["STA", "ST"] <- rates[[1]]$trans_att_replayed
        M["STA", "RT"] <- 1 - sum(M["STA", ])

        M["RT", "RTO="] <- rates[[2]]$trans_loss_other
        M["RT", "ST"] <- rates[[2]]$trans_no_att
        M["RT", "RTA"] <- 1 - sum(M["RT", ])
        M["RTA", "RTA="] <- rates[[2]]$trans_att_error
        M["RTA", "RTA#"] <- rates[[2]]$trans_att_kill
        M["RTA", "RTA/"] <- rates[[1]]$trans_block
        M["RTA", "RT"] <- rates[[2]]$trans_att_replayed
        M["RTA", "ST"] <- 1 - sum(M["RTA", ])
    } else if (process_model == "phase_simple") {
        M["SS", "SS="] <- rates[[1]]$serve_error
        M["SS", "SS#"] <- rates[[1]]$serve_ace
        M["SS", "RR"] <- 1 - sum(M["SS", ])
        M["RR", "RR="] <- rates[[2]]$rec_loss
        M["RR", "RR#"] <- rates[[2]]$rec_win
        M["RR", "RT"] <- rates[[2]]$rec_replayed
        M["RR", "ST"] <- 1 - sum(M["RR", ]) ## no attack or attack in play

        M["ST", "ST="] <- rates[[1]]$trans_loss
        M["ST", "ST#"] <- rates[[1]]$trans_win
        M["ST", "ST"] <- rates[[1]]$trans_replayed
        M["ST", "RT"] <- 1 - sum(M["ST", ])

        M["RT", "RT="] <- rates[[2]]$trans_loss
        M["RT", "RT#"] <- rates[[2]]$trans_win
        M["RT", "RT"] <- rates[[2]]$trans_replayed
        M["RT", "ST"] <- 1 - sum(M["RT", ])
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
    if (is.list(servingM) && length(servingM) == 2 && all(vapply(servingM, inherits, "markovchain", FUN.VALUE = TRUE))) {
        receivingM <- servingM[[2]]
        servingM <- servingM[[1]]
    }
    winR_states <- states(servingM)[grepl("^S.*[=/]$", states(servingM)) | grepl("^R.*#$", states(servingM))]
    sum(absorptionProbabilities(servingM)[1, winR_states])
}

## given the team markovchain objects, construct the serve-sideout-breakpoint markovchain
MC2MCP <- function(M1, M2) {
    if (is.list(M1) && length(M1) == 2 && all(vapply(M1, inherits, "markovchain", FUN.VALUE = TRUE))) {
        M2 <- M1[[2]]
        M1 <- M1[[1]]
    }
    ## take per-team MC matrices, calculate sideout and breakpoint rates, and create that MC object
    ## sideout rates
    ##so <- c(absorptionProbabilities(M2)[1, 2], absorptionProbabilities(M1)[1, 2])
    winR_states <- states(M1)[grepl("^S.*[=/]$", states(M1)) | grepl("^R.*#$", states(M1))]
    winS_states <- states(M1)[grepl("^S.*#$", states(M1)) | grepl("^R.*[=/]$", states(M1))]
    so <- c(sum(absorptionProbabilities(M2)[1, winR_states]), sum(absorptionProbabilities(M1)[1, winR_states]))

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

MC_to_points_breakdown <- function(M1, M2) {
    if (is.list(M1) && length(M1) == 2 && all(vapply(M1, inherits, "markovchain", FUN.VALUE = TRUE))) {
        M2 <- M1[[2]]
        M1 <- M1[[1]]
    }
    out <- do_MC_to_points_breakdown(M1, M2, this_team = 1L)
    out2 <- do_MC_to_points_breakdown(M2, M1, this_team = 2L)
    rbind(out, out2)
}
do_MC_to_points_breakdown <- function(M1, M2, this_team) {
    stopifnot(identical(states(M1), states(M2)))
    winR_states <- states(M1)[grepl("^S.*[=/]$", states(M1)) | grepl("^R.*#$", states(M1))]
    winS_states <- states(M1)[grepl("^S.*#$", states(M1)) | grepl("^R.*[=/]$", states(M1))]
    ## relative number of serves by team
    sprop <- MCP_serve_proportions(MC2MCP(M1, M2))
    ## points won by team 1, scaled by their serve proportion
    T1points <- c(absorptionProbabilities(M1)[1, winS_states] * sprop[1], absorptionProbabilities(M2)[1, winR_states] * sprop[2])
    T1points <- T1points/sum(T1points)
    names(T1points) <- substr(names(T1points), 2, 4)
    temp <- list()
    for (uu in unique(names(T1points))) temp[[uu]] <- sum(T1points[names(T1points) == uu])
    temp <- unlist(temp)
    out <- data.frame(point_won_by = this_team, outcome = states_to_factor(names(temp)), proportion = unname(temp), stringsAsFactors = FALSE)
    out <- out[order(out$outcome), ]
    out$outcome <- as.character(out$outcome)
    rownames(out) <- NULL
    out
}
states_as_factors <- function() c("S=" = "Serve error", "S#" = "Serve ace", "RO=" = "Rec other loss", "RA=" = "Rec attack error", "RA/" = "Rec attack block", "RA#" = "Rec attack kill", "R=" = "Rec loss", "R#" = "Rec win", "TO=" = "Trans other loss", "TA=" = "Trans attack error", "TA#" = "Trans attack kill", "TA/" = "Trans attack block", "T=" = "Trans loss", "T#" = "Trans win")
states_to_factor <- function(s) {
    remap <- states_as_factors()
    for (us in na.omit(unique(s))) if (us %in% names(remap)) s[which(s == us)] <- remap[names(remap) == us]
    factor(s, levels = remap)
}
## ## compare to monte carlo sim results
## sim_result <- vs_simulate_match(rates = rates, n = 5e3, method = "monte carlo", simple = FALSE)
## chk <- sim_result$simres14 %>% group_by(point_won_by) %>% count(outcome) %>% mutate(n = n/sum(n)) %>% ungroup 
## 
## chk %>% dplyr::filter(point_won_by == 1) %>% left_join(temp, by = "outcome")
