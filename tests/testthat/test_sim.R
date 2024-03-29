context("Tests of simulation results")

test_that("Simulation with known parameters gives expected results", {
    rates <- list(
        list(serve_ace = 0, serve_error = 0, rec_loss_other = 0, rec_att_error = 0, rec_att_kill = 0.5, rec_att_replayed = 0, rec_no_att = 0, trans_loss_other = 0,
             trans_att_error = 0, trans_att_kill = 1.0, trans_att_replayed = 0, trans_no_att = 0, rec_block= 0, trans_block = 0),
        list(serve_ace = 0, serve_error = 0, rec_loss_other = 0, rec_att_error = 0, rec_att_kill = 0.5, rec_att_replayed = 0, rec_no_att = 0, trans_loss_other = 0,
             trans_att_error = 0, trans_att_kill = 1.0, trans_att_replayed = 0, trans_no_att = 0, rec_block= 0, trans_block = 0))
    res <- bind_rows(lapply(seq_len(5e3), function(z) vs_simulate_set(rates, method = "monte carlo")))
    chk <- table(res[, c("serving", "point_won_by")])
    chk <- t(apply(chk, 1, function(z) z/sum(z))) ## normalize by row sums, i.e. percentage of outcome by serving team
    ## all should be around 0.5
    expect_true(all(abs(chk - 0.5) < 0.01))

    ## team 1 recycles reception phase attacks
    rates[[1]]$rec_att_replayed <- 0.5
    res <- bind_rows(lapply(seq_len(5e3), function(z) vs_simulate_set(rates, method = "monte carlo")))
    chk <- table(res[, c("serving", "point_won_by")])
    chk <- t(apply(chk, 1, function(z) z/sum(z)))
    ## team 1 serving, should see 50% team 2 wins on rec_att_kill and 50% team 1 wins on their transition attack
    ## team 2 serving should see 100% team 1 wins
    ## team 2 should never get a transition attack opportunity/kill
    expect_true(all(abs(chk - matrix(c(0.5, 0.5, 1.0, 0), nrow = 2, byrow = TRUE)) < 0.005))
    ## team 2 serving should see 100% team 1 wins but 50% should be rec att kill and 50% trans att kill
    chk <- table(res$outcome[res$serving == 2])
    expect_true(all(abs(chk/sum(chk) - 0.5) < 0.005))
})

test_that("Simulation varies according to who served first", {
    ## firstly with fixed set win probs, calculate match outcomes
    res1 <- vs_set_probs_to_match(sp13 = 0.75, sp24 = 0.25, sp5 = 0.6, serve_known = TRUE) ## team 1 receives first with higher set win rate in 1,3
    res2 <- vs_set_probs_to_match(sp13 = 0.25, sp24 = 0.75, sp5 = 0.6, serve_known = TRUE) ## vice-versa
    ## the probability of reaching set 5 should not depend on who served first
    expect_equal(res1$scores$`3-2` + res1$scores$`2-3`, res2$scores$`3-2` + res2$scores$`2-3`)
    ## the overall win rates for res1 and res2 should be the same, because this comes down to who served first in set 5 (which is the same here in res1 and res2)
    expect_equal(res1$pwin, res2$pwin)
    ## but overall set score distributions should be different
    expect_false(all(unlist(res1$scores) == unlist(res2$scores)))

    ## now check the same against simulations
    rates <- list(
        list(serve_ace = 0.1, serve_error = 0.1, rec_loss_other = 0, rec_att_error = 0.1, rec_att_kill = 0.5, rec_att_replayed = 0.1, rec_no_att = 0.1, trans_loss_other = 0,
             trans_att_error = 0.1, trans_att_kill = 0.4, trans_att_replayed = 0.1, trans_no_att = 0.1, rec_block= 0.05, trans_block = 0.05),
        list(serve_ace = 0.1, serve_error = 0.1, rec_loss_other = 0, rec_att_error = 0.1, rec_att_kill = 0.4, rec_att_replayed = 0.1, rec_no_att = 0.1, trans_loss_other = 0,
             trans_att_error = 0.1, trans_att_kill = 0.35, trans_att_replayed = 0.1, trans_no_att = 0.1, rec_block= 0.05, trans_block = 0.05))

    res1 <- suppressWarnings(vs_simulate_match(rates, n = 5e3, serving = TRUE, method = "monte carlo"))
    res2 <- suppressWarnings(vs_simulate_match(rates, n = 5e3, serving = FALSE, method = "monte carlo"))

    ## the probability of reaching set 5 should not depend on who served first
    expect_true(abs((res1$scores$`3-2` + res1$scores$`2-3`) - (res2$scores$`3-2` + res2$scores$`2-3`)) < 0.02)
    ## the overall win rates for res1 and res2 should be the same, because this comes down to who served first in set 5 (which is random here)
    expect_true(abs(res1$pwin - res2$pwin) < 0.02)
    
    res1 <- suppressWarnings(vs_simulate_match(rates, n = 5e3, serving = TRUE, method = "theoretical"))
    res2 <- suppressWarnings(vs_simulate_match(rates, n = 5e3, serving = FALSE, method = "theoretical"))

    ## the probability of reaching set 5 should not depend on who served first
    expect_true(abs((res1$scores$`3-2` + res1$scores$`2-3`) - (res2$scores$`3-2` + res2$scores$`2-3`)) < 0.02)
    ## the overall win rates for res1 and res2 should be the same, because this comes down to who served first in set 5 (which is random here)
    expect_true(abs(res1$pwin - res2$pwin) < 0.02)
})

test_that("Simulation copes with non-standard variable names", {
    x <- datavolley::dv_read(datavolley::dv_example_file())
    rates <- vs_estimate_rates(x$plays, "each")
    xp <- x$plays
    xp <- xp[which(xp$point), ]
    ## use show_plot = FALSE for testing, GitHub actions is giving
    ## Error in `title(...)`: conversion failure on 'Braslovče vs. Nova KBM Branik' in 'mbcsToSbcs': for č (U+010D)
    ## which is probably something to do with a non-standard graphics device in that test environment
    p1 <- vs_match_win_probability(xp, rates$sideout, go_to = 25, go_to_tiebreak = 15, max_sets = 5, show_plot = FALSE, home_color = "blue", visiting_color = "darkred")
    names(xp)[names(xp) == "set_number"] <- "unexpected_set_number_column_name"
    ## should error
    expect_error(vs_match_win_probability(xp, rates$sideout, go_to = 25, go_to_tiebreak = 15, max_sets = 5, show_plot = FALSE, home_color = "blue", visiting_color = "darkred"), "please rename")
    names(xp)[names(xp) == "unexpected_set_number_column_name"] <- "set_number"
    names(xp)[names(xp) == "visiting_team_score"] <- "visiting_score"
    ## should work
    p2 <- vs_match_win_probability(xp, rates$sideout, go_to = 25, go_to_tiebreak = 15, max_sets = 5, show_plot = FALSE, home_color = "blue", visiting_color = "darkred")
    expect_equal(p1, p2)
})
