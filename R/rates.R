#' Estimate parameters required for simulation
#'
#' @param x datavolleyplays: the plays component of a datavolley object as returned by [datavolley::dv_read()]
#' @param target_team string: the team name to calculate rates for. If missing or NULL, rates will be calculated across the entire data.frame `x`. If `target_team` is "each", rates will be calculated for each team separately
#' @param by string: grouping to calculate rates by. Either "none" (calculate whole-data set rates), "match" (by match), or "set" (by match and set)
#' @param moderate logical: if `TRUE`, apply some checks to attempt to ensure that the estimated rates are reasonable. Currently these checks include:
#' * setting error rates are limited to a maximum of 5%. Some scouts do not include setting actions, except where they are errors or otherwise exceptional, which can lead to unrealistic estimates of setting error rates
#' @param process_model string: estimate the rates required for which process model? Either "sideout", "phase", "phase_simple", or "all"
#'
#' @return A tibble, currently with the columns sideout, serve_ace, serve_error, rec_set_error, rec_att_error, rec_att_kill, rec_att_replayed, trans_set_error, trans_att_error, trans_att_kill, trans_att_replayed, rec_block, and trans_block, plus (if `by` is "match") match_id and (if `by` is "set") set_number and (if `target_team` is "each") "team"
#'
#' @seealso [vs_simulate_set()]
#'
#' @examples
#' \dontrun{
#'   library(datavolley)
#'   x <- dv_read(dv_example_file())
#'   rates <- vs_estimate_rates(x, target_team = "each")
#'
#'   vs_simulate_set(rates) ## simulate a single set
#'   vs_simulate_match(rates) ## simulate a match
#'   ## so given the performances of the two teams during that match, we expect
#'   ##  that the home team should have won, with 3-0 being the most likely scoreline
#'
#'   ## compare to the actual match result
#'   summary(x)
#' }
#'
#' @export
vs_estimate_rates <- function(x, target_team, by = "none", moderate = TRUE, process_model = "all") {
    if (inherits(x, c("datavolley", "peranavolley"))) x <- x$plays
    if (missing(target_team)) target_team <- NULL else assert_that(is.string(target_team))
    assert_that(is.string(by))
    by <- match.arg(tolower(by), c("none", "match", "set"))
    assert_that(is.flag(moderate), !is.na(moderate))
    process_model <- tolower(process_model)
    process_model <- match.arg(process_model, c("phase", "sideout", "phase_simple", "all"))
    if (!"opposition" %in% names(x)) x <- mutate(x, opposition = case_when(.data$team == .data$home_team ~ .data$visiting_team,
                                                                           .data$team == .data$visiting_team ~ .data$home_team))
    if (!is.null(target_team) && target_team == "each") {
        return(dplyr::bind_rows(lapply(unique(na.omit(x$team)), function(tm) {
            dplyr::select(dplyr::mutate(vs_estimate_rates(x, target_team = tm, by = by, process_model = process_model), team = tm), "team", dplyr::everything())
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

    ## add some variables to help with rec_no_att and trans_no_att
    ## note that these will need revision if we want to cope with files that don't have all attacks scouted
    is_active_skill <- function(z) !is.na(z) & !z %in% c("Timeout", "Technical timeout", "Substitution")
    ## treat blocks against reception attack (which will be marked as phase Reception) as phase Transition
    x <- mutate(x, phase = case_when(.data$skill == "Block" & .data$phase == "Reception" ~ "Transition", TRUE ~ .data$phase))

    ## did the rally end on this team touch, excluding dig and block errors?
    temp <- dplyr::ungroup(dplyr::mutate(group_by(dplyr::filter(x, is_active_skill(.data$skill) & !(.data$skill %in% c("Dig", "Block") & .data$evaluation == "Error")), .data$match_id, .data$point_id), was_last_touch = .data$team_touch_id == max(.data$team_touch_id)))
    temp <- dplyr::distinct(temp, .data$match_id, .data$team_touch_id, .data$was_last_touch)
    nrow0 <- nrow(x)
    x <- mutate(left_join(x, temp, by = c("match_id", "team_touch_id")), was_last_touch = case_when(is.na(.data$was_last_touch) ~ FALSE, TRUE ~ .data$was_last_touch))
    if (nrow(x) != nrow0) stop("error with was_last_touch")

    ## did we make an attack during a given team touch?
    x <- dplyr::ungroup(mutate(group_by(x, .data$match_id, .data$team_touch_id), made_attack = any(.data$skill == "Attack")))

    if (process_model %in% c("phase_simple", "all")) {
        ## did we get a replay (make the next action that isn't a dig or block)
        temp <- dplyr::slice(group_by(dplyr::filter(x, is_active_skill(.data$skill) & !.data$skill %in% c("Block", "Dig")), .data$match_id, .data$point_id, .data$team_touch_id), 1L)
        temp <- mutate(group_by(temp, .data$match_id, .data$point_id), got_replay = lead(.data$team) == .data$team)
        nrow0 <- nrow(x)
        x <- mutate(left_join(x, temp[, c("match_id", "team_touch_id", "got_replay")], by = c("match_id", "team_touch_id")), got_replay = case_when(is.na(.data$got_replay) ~ FALSE, TRUE ~ .data$got_replay))
        if (nrow(x) != nrow0) stop("error with got_replay")
    }

    xt <- if (!is.null(target_team)) dplyr::filter(x, .data$team == target_team) else x
    if (!is.null(by)) xt <- if (packageVersion("dplyr") >= "1.0.0") group_by(xt, dplyr::across(by)) else group_by_at(xt, by)
    if (process_model %in% c("sideout")) {
        out <- NULL
    } else {
        out <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$skill == "Serve"),
                                        serve_ace = mean(.data$evaluation == "Ace", na.rm = TRUE),
                                        serve_error = mean(.data$evaluation == "Error", na.rm = TRUE)))
    }

    if (process_model %in% c("phase", "all")) {
        ## reception set error
        f_seterr <- if (moderate) function(z) min(0.05, z) else function(z) z
        rset <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$phase == "Reception" & .data$skill == "Set"),
                                         rec_set_error = f_seterr(mean(.data$evaluation == "Error", na.rm = TRUE))))
        ## reception attack
        ratt <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$phase == "Reception" & .data$skill == "Attack"),
                                         rec_att_error = mean(.data$evaluation == "Error", na.rm = TRUE),
                                         rec_att_kill = mean(.data$evaluation == "Winning attack", na.rm = TRUE),
                                         rec_att_replayed = mean(.data$evaluation %eq% "Blocked for reattack" | .data$special_code %eq% "Block control" | .data$made_next_attack, na.rm = TRUE)))
    }

    if (process_model %in% c("phase_simple", "all")) {
        ## reception-phase point loss/win excluding reception errors (these are counted in serve aces)
        ## these don't add to 1 because the rally does not need to end in reception phase
        rlw <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$skill == "Reception" & .data$evaluation != "Error"),
                                        rec_loss = mean(.data$was_last_touch & .data$point_won_by != .data$team, na.rm = TRUE),
                                        rec_win = mean(.data$was_last_touch & .data$point_won_by == .data$team, na.rm = TRUE)))

        rnoatt <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$skill == "Reception" & .data$evaluation != "Error"), rec_no_att = mean(!.data$made_attack, na.rm = TRUE)))
        r_replayed <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$skill == "Reception" & .data$evaluation != "Error"), rec_replayed = mean(.data$got_replay, na.rm = TRUE)))
    }

    if (process_model %in% c("phase", "all")) {
        ## transition set error
        tset <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$phase == "Transition" & .data$skill == "Set"),
                                         trans_set_error = f_seterr(mean(.data$evaluation == "Error", na.rm = TRUE))))
        ## transition attack
        tatt <- ungroup(dplyr::summarize(dplyr::filter(xt, .data$phase == "Transition" & .data$skill == "Attack"),
                                         trans_att_error = mean(.data$evaluation == "Error", na.rm = TRUE),
                                         trans_att_kill = mean(.data$evaluation == "Winning attack", na.rm = TRUE),
                                         trans_att_replayed = mean(.data$evaluation %eq% "Blocked for reattack" | .data$special_code %eq% "Block control" | .data$made_next_attack, na.rm = TRUE)))
    }

    if (process_model %in% c("phase_simple", "all")) {
        ## transition-phase point loss/win
        ## these don't add to 1 because they are the probs of winning *on this team touch*, but the rally can continue
        temp <- ungroup(dplyr::slice(group_by(dplyr::filter(xt, .data$phase == "Transition" & is_active_skill(.data$skill)), .data$team_touch_id, .add = TRUE), 1L))
        if (!is.null(by)) temp <- if (packageVersion("dplyr") >= "1.0.0") group_by(temp, dplyr::across(by)) else group_by_at(temp, by)
        tlw <- ungroup(dplyr::summarize(temp, trans_loss = mean(.data$was_last_touch & .data$point_won_by != .data$team, na.rm = TRUE),
                                        trans_win = mean(.data$was_last_touch & .data$point_won_by == .data$team, na.rm = TRUE)))

        temp <- ungroup(dplyr::slice(group_by(dplyr::filter(xt, .data$phase == "Transition" & is_active_skill(.data$skill) & !(.data$skill %in% c("Dig", "Block") & .data$evaluation == "Error") & !(.data$skill == "Block" & grepl("opposition to replay", .data$evaluation, fixed = TRUE))), .data$team_touch_id, .add = TRUE), 1L)) ## one row from each transition team touch excluding dig/block errors and block replays
        if (!is.null(by)) temp <- if (packageVersion("dplyr") >= "1.0.0") group_by(temp, dplyr::across(by)) else group_by_at(temp, by)
        tnoatt <- ungroup(dplyr::summarize(temp, trans_no_att = mean(!.data$made_attack, na.rm = TRUE), trans_replayed = mean(.data$got_replay, na.rm = TRUE)))
    }

    ## blocking
    xnt <- if (!is.null(target_team)) dplyr::filter(x, .data$opposition == target_team) else x
    if (!is.null(by)) xnt <- if (packageVersion("dplyr") >= "1.0.0") group_by(xnt, dplyr::across(by)) else group_by_at(xnt, by)
    so <- ungroup(dplyr::summarize(dplyr::filter(xnt, .data$skill == "Serve"), sideout = mean(.data$point_won_by == .data$opposition, na.rm = TRUE)))

    if (process_model %in% c("phase", "all")) {
        rblk <- ungroup(dplyr::summarize(dplyr::filter(xnt, .data$skill == "Attack" & .data$phase == "Reception"), N_opp_rec_att = n(),
                                         rec_block = mean(.data$evaluation == "Blocked", na.rm = TRUE)))
        ## invasions not used yet
        ##out$rec_block_invasion <- sum(xt$evaluation == "Invasion" & xt$phase == "Reception", na.rm = TRUE)/out$N_opp_rec_att
        tblk <- ungroup(dplyr::summarize(dplyr::filter(xnt, .data$skill == "Attack" & .data$phase == "Transition"), N_opp_trans_att = n(),
                                         trans_block = mean(.data$evaluation == "Blocked", na.rm = TRUE)))
        ##out$trans_block_invasion <- sum(xt$evaluation == "Invasion" & xt$phase == "Transition", na.rm = TRUE)/out$N_opp_trans_att
    }

    if (is.null(by)) {
        out <- if (is.null(out)) so else cbind(out, so)
        if (process_model %in% c("phase", "all")) out <- cbind(out, rset, ratt, tset, tatt, rblk, tblk)
        if (process_model %in% c("phase_simple", "all")) out <- cbind(out, rlw, rnoatt, r_replayed, tlw, tnoatt)
    } else {
        out <- if (is.null(out)) so else left_join(out, so, by = by)
        if (process_model %in% c("phase", "all")) {
            out <- left_join(out, rset, by = by)
            out <- left_join(out, ratt, by = by)
            out <- left_join(out, tset, by = by)
            out <- left_join(out, tatt, by = by)
            out <- left_join(out, rblk, by = by)
            out <- left_join(out, tblk, by = by)
        }
        if (process_model %in% c("phase_simple", "all")) {
            out <- left_join(out, rlw, by = by)
            out <- left_join(out, rnoatt, by = by)
            out <- left_join(out, r_replayed, by = by)
            out <- left_join(out, tlw, by = by)
            out <- left_join(out, tnoatt, by = by)
        }
    }
    out <- dplyr::mutate_all(out, function(z) ifelse(is.na(z), 0, z))
    out[, setdiff(names(out), c("N_opp_rec_att", "N_opp_trans_att"))]
}

## convert df to list
precheck_rates <- function(rates, process_model = "sideout") {
    assert_that(is.string(process_model))
    process_model <- match.arg(tolower(process_model), c("phase", "sideout", "phase_simple"))
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
    if (process_model == "phase") {
        expected <- c("serve_ace", "serve_error", "rec_set_error", "rec_att_error", "rec_att_kill", "rec_att_replayed", "trans_set_error", "trans_att_error", "trans_att_kill", "trans_att_replayed", "rec_block", "trans_block")
    } else if (process_model == "phase_simple") {
        ## TODO simplify
        expected <- c("serve_ace", "serve_error", "rec_loss", "rec_win", "rec_replayed", "trans_loss", "trans_win", "trans_replayed")
    } else {
        expected <- "sideout"
    }
    if (!all(expected %in% names(rates[[1]]))) stop("team 1 rates missing at least one parameter: ", paste(setdiff(expected, names(rates[[1]])), collapse = ", "))
    if (!all(expected %in% names(rates[[2]]))) stop("team 2 rates missing at least one parameter: ", paste(setdiff(expected, names(rates[[2]])), collapse = ", "))
    rates
}
