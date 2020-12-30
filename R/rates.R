#' Estimate parameters required for simulation
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by [datavolley::dv_read()]
#' @param target_team string: the team name to calculate rates for. If missing or NULL, rates will be calculated across the entire data.frame `x`. If `target_team` is "each", rates will be calculated for each team separately
#' @param by string: grouping to calculate rates by. Either "none" (calculate whole-data set rates), "match" (by match), or "set" (by match and set)
#' @param moderate logical: if `TRUE`, apply some checks to attempt to ensure that the estimated rates are reasonable. Currently these checks include:
#' * setting error rates are limited to a maximum of 5%. Some scouts do not include setting actions, except where they are errors or otherwise exceptional, which can lead to unrealistic estimates of setting error rates
#'
#' @return A tibble, currently with the columns sideout, serve_ace, serve_error, rec_set_error, rec_att_error, rec_att_kill, rec_att_replayed, trans_set_error, trans_att_error, trans_att_kill, trans_att_replayed, rec_block, and trans_block, plus (if `by` is "match") match_id and (if `by` is "set") set_number and (if `target_team` is "each") "team"
#'
#' @seealso [vs_simulate_set()]
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
vs_estimate_rates <- function(x, target_team, by = "none", moderate = TRUE) {
    if (inherits(x, c("datavolley", "peranavolley"))) x <- x$plays
    if (missing(target_team)) target_team <- NULL else assert_that(is.string(target_team))
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("none", "match", "set"))
    assert_that(is.flag(moderate), !is.na(moderate))
    if (!"opposition" %in% names(x)) x <- mutate(x, opposition = case_when(.data$team == .data$home_team ~ .data$visiting_team,
                                                                           .data$team == .data$visiting_team ~ .data$home_team))
    if (!is.null(target_team) && target_team == "each") {
        return(dplyr::bind_rows(lapply(unique(na.omit(x$team)), function(tm) {
            dplyr::select(dplyr::mutate(vs_estimate_rates(x, target_team = tm, by = by), team = tm), "team", dplyr::everything())
        })))
    }
    if (by == "none") {
        by <- NULL
    } else if (by == "match") {
        by <- "match_id"
    } else {
        by <- c("match_id", "set_number")
    }
    ## figure out attack sequences in each rally, used in *_att_replayed
    x$ROWNUM <- seq_len(nrow(x))
    temp <- dplyr::ungroup(dplyr::mutate(group_by(dplyr::filter(x, .data$skill == "Attack"), .data$match_id, .data$point_id), made_next_attack = lead(.data$team) %eq% .data$team))
    x <- dplyr::select(left_join(x, dplyr::select(temp, "made_next_attack", "ROWNUM"), by = "ROWNUM"), -"ROWNUM")
    xt <- if (!is.null(target_team)) dplyr::filter(x, .data$team == target_team) else x
    if (!is.null(by)) xt <- group_by(xt, across(by))
    out <- dplyr::summarize(dplyr::filter(xt, .data$skill == "Serve"), serve_ace = mean(.data$evaluation == "Ace", na.rm = TRUE),
                            serve_error = mean(.data$evaluation == "Error", na.rm = TRUE), .groups = "drop")
    ## reception set error
    f_seterr <- if (moderate) function(z) min(0.05, z) else function(z) z
    rset <- dplyr::summarize(dplyr::filter(xt, .data$phase == "Reception" & .data$skill == "Set"), rec_set_error = f_seterr(mean(.data$evaluation == "Error", na.rm = TRUE)), .groups = "drop")

    ## reception attack
    ratt <- dplyr::summarize(dplyr::filter(xt, .data$phase == "Reception" & .data$skill == "Attack"), rec_att_error = mean(.data$evaluation == "Error", na.rm = TRUE),
                             rec_att_kill = mean(.data$evaluation == "Winning attack", na.rm = TRUE),
                             rec_att_replayed = mean(.data$evaluation %eq% "Blocked for reattack" | .data$special_code %eq% "Block control" | .data$made_next_attack, na.rm = TRUE),
                             .groups = "drop")

    ## transition set error
    tset <- dplyr::summarize(dplyr::filter(xt, .data$phase == "Transition" & .data$skill == "Set"), trans_set_error = f_seterr(mean(.data$evaluation == "Error", na.rm = TRUE)), .groups = "drop")
    ## transition attack
    tatt <- dplyr::summarize(dplyr::filter(xt, .data$phase == "Transition" & .data$skill == "Attack"), trans_att_error = mean(.data$evaluation == "Error", na.rm = TRUE),
                             trans_att_kill = mean(.data$evaluation == "Winning attack", na.rm = TRUE),
                             trans_att_replayed = mean(.data$evaluation %eq% "Blocked for reattack" | .data$special_code %eq% "Block control" | .data$made_next_attack, na.rm = TRUE),
                             .groups = "drop")
    ## blocking
    xnt <- if (!is.null(target_team)) dplyr::filter(x, .data$opposition == target_team) else x
    if (!is.null(by)) xnt <- group_by(xnt, across(by))

    so <- dplyr::summarize(dplyr::filter(xnt, .data$skill == "Serve"), sideout = mean(.data$point_won_by == .data$opposition, na.rm = TRUE), .groups = "drop")
    rblk <- dplyr::summarize(dplyr::filter(xnt, .data$skill == "Attack" & .data$phase == "Reception"), N_opp_rec_att = n(),
                                       rec_block = mean(.data$evaluation == "Blocked", na.rm = TRUE), .groups = "drop")
    ## invasions not used yet
    ##out$rec_block_invasion <- sum(xt$evaluation == "Invasion" & xt$phase == "Reception", na.rm = TRUE)/out$N_opp_rec_att
    tblk <- dplyr::summarize(dplyr::filter(xnt, .data$skill == "Attack" & .data$phase == "Transition"), N_opp_trans_att = n(),
                             trans_block = mean(.data$evaluation == "Blocked", na.rm = TRUE), .groups = "drop")
    ##out$trans_block_invasion <- sum(xt$evaluation == "Invasion" & xt$phase == "Transition", na.rm = TRUE)/out$N_opp_trans_att

    if (is.null(by)) {
        out <- cbind(out, rset, ratt, tset, tatt, so, rblk, tblk)
    } else {
        out <- left_join(out, rset, by = by)
        out <- left_join(out, ratt, by = by)
        out <- left_join(out, tset, by = by)
        out <- left_join(out, tatt, by = by)
        out <- left_join(out, so, by = by)
        out <- left_join(out, rblk, by = by)
        out <- left_join(out, tblk, by = by)
    }
    out <- dplyr::mutate_all(out, function(z) ifelse(is.na(z), 0, z))
    dplyr::select(out, -"N_opp_rec_att", -"N_opp_trans_att")
}

## convert df to list
precheck_rates <- function(rates, process_model = "sideout") {
    assert_that(is.string(process_model))
    process_model <- match.arg(tolower(process_model), c("phase", "sideout"))
    if (is.data.frame(rates)) {
        ## convert to two-element list, if we can
        if ("team" %in% names(rates) && length(unique(na.omit(rates$team))) == 2) {
            tms <- unique(na.omit(rates$team))
            rates <- list(rates[which(rates$team == tms[1]), ], rates[which(rates$team == tms[2]), ])
        }
    }
    if (!(is.list(rates) && length(rates) == 2)) {
        stop("rates must be a two-element list")
    }
    if (is.data.frame(rates[[1]])) rates[[1]] <- as.list(rates[[1]])
    if (is.data.frame(rates[[2]])) rates[[2]] <- as.list(rates[[2]])
    ## make sure that all elements are present
    expected <- if (process_model == "phase") c("serve_ace", "serve_error", "rec_set_error", "rec_att_error", "rec_att_kill", "rec_att_replayed", "trans_set_error", "trans_att_error", "trans_att_kill", "trans_att_replayed", "rec_block", "trans_block") else "sideout"
    if (!all(expected %in% names(rates[[1]]))) stop("team 1 rates missing at least one parameter: ", paste(setdiff(expected, names(rates[[1]])), collapse = ", "))
    if (!all(expected %in% names(rates[[2]]))) stop("team 2 rates missing at least one parameter: ", paste(setdiff(expected, names(rates[[2]])), collapse = ", "))
    rates
}
