#' Estimate parameters required for simulation
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by \code{\link[datavolley]{dv_read}}
#' @param target_team string: the team name to calculate rates for. If missing or NULL, rates will be calculated across the entire data.frame \code{x}
#'
#' @return A named list, currently with the names serve_ace, serve_error, rec_set_error, rec_att_error, rec_att_kill, trans_set_error, trans_att_error, trans_att_kill, rec_block, and trans_block
#'
#' @seealso \code{\link{vs_simulate_set}}
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
#' @export
vs_estimate_rates <- function(x, target_team) {
    if (inherits(x, c("datavolley", "peranavolley"))) x <- x$plays
    if (missing(target_team) || is.null(target_team)) {
        target_team <- NULL
    } else {
        assert_that(is.string(target_team))
    }
    if (!"opposition" %in% names(x)) x <- mutate(x, opposition = case_when(.data$team == .data$home_team ~ .data$visiting_team,
                                                                           .data$team == .data$visiting_team ~ .data$home_team))
    xt <- if (!is.null(target_team)) dplyr::filter(x, .data$team == target_team) else x
    out <- dplyr::summarize(dplyr::filter(xt, .data$skill == "Serve"), serve_ace = mean(.data$evaluation == "Ace", na.rm = TRUE),
                            serve_error = mean(.data$evaluation == "Error", na.rm = TRUE))
    ## reception set error
    out <- cbind(out, dplyr::summarize(dplyr::filter(xt, .data$phase == "Reception" & .data$skill == "Set"), rec_set_error = mean(.data$evaluation == "Error", na.rm = TRUE)))
    ## reception attack
    out <- cbind(out, dplyr::summarize(dplyr::filter(xt, .data$phase == "Reception" & .data$skill == "Attack"), rec_att_error = mean(.data$evaluation == "Error", na.rm = TRUE),
                                       rec_att_kill = mean(.data$evaluation == "Winning attack", na.rm = TRUE)))
    ## transition set error
    out <- cbind(out, dplyr::summarize(dplyr::filter(xt, .data$phase == "Transition" & .data$skill == "Set"), trans_set_error = mean(.data$evaluation == "Error", na.rm = TRUE)))
    ## transition attack
    out <- cbind(out, dplyr::summarize(dplyr::filter(xt, .data$phase == "Transition" & .data$skill == "Attack"), trans_att_error = mean(.data$evaluation == "Error", na.rm = TRUE),
                                       trans_att_kill = mean(.data$evaluation == "Winning attack", na.rm = TRUE)))
    ## blocking
    xnt <- if (!is.null(target_team)) dplyr::filter(x, .data$opposition == target_team) else x
    out <- cbind(out, dplyr::summarize(dplyr::filter(xnt, .data$skill == "Attack" & .data$phase == "Reception"), N_opp_rec_att = n(),
                                       rec_block = mean(.data$evaluation == "Blocked", na.rm = TRUE)))
    ## invasions not used yet
    ##out$rec_block_invasion <- sum(xt$evaluation == "Invasion" & xt$phase == "Reception", na.rm = TRUE)/out$N_opp_rec_att
    out <- cbind(out, dplyr::summarize(dplyr::filter(xnt, .data$skill == "Attack" & .data$phase == "Transition"), N_opp_trans_att = n(),
                                       trans_block = mean(.data$evaluation == "Blocked", na.rm = TRUE)))
    ##out$trans_block_invasion <- sum(xt$evaluation == "Invasion" & xt$phase == "Transition", na.rm = TRUE)/out$N_opp_trans_att
    as.list(dplyr::select(out, -"N_opp_rec_att", -"N_opp_trans_att"))
}
