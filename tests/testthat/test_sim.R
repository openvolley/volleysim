context("Tests of simulation results")

test_that("Simulation with known parameters gives expected results", {
    rates <- list(
        list(serve_ace = 0, serve_error = 0, rec_set_error = 0, rec_att_error = 0, rec_att_kill = 0.5, rec_att_replayed = 0, trans_set_error = 0,
             trans_att_error = 0, trans_att_kill = 1.0, trans_att_replayed = 0, rec_block= 0, trans_block = 0),
        list(serve_ace = 0, serve_error = 0, rec_set_error = 0, rec_att_error = 0, rec_att_kill = 0.5, rec_att_replayed = 0, trans_set_error = 0,
             trans_att_error = 0, trans_att_kill = 1.0, trans_att_replayed = 0, rec_block= 0, trans_block = 0))
    res <- bind_rows(lapply(seq_len(5e3), function(z) vs_simulate_set(rates)))
    chk <- table(res[, c("serving", "point_won_by")])
    chk <- t(apply(chk, 1, function(z) z/sum(z))) ## normalize by row sums, i.e. percentage of outcome by serving team
    ## all should be around 0.5
    expect_true(all(abs(chk - 0.5) < 0.01))

    ## team 1 recycles reception phase attacks 
    rates[[1]]$rec_att_replayed <- 0.5
    res <- bind_rows(lapply(seq_len(5e3), function(z) vs_simulate_set(rates)))
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
    rates <- list(
        list(serve_ace = 0.1, serve_error = 0.1, rec_set_error = 0, rec_att_error = 0.1, rec_att_kill = 0.5, rec_att_replayed = 0.1, trans_set_error = 0,
             trans_att_error = 0.1, trans_att_kill = 0.4, trans_att_replayed = 0.1, rec_block= 0.05, trans_block = 0.05),
        list(serve_ace = 0.1, serve_error = 0.1, rec_set_error = 0, rec_att_error = 0.1, rec_att_kill = 0.4, rec_att_replayed = 0.1, trans_set_error = 0,
             trans_att_error = 0.1, trans_att_kill = 0.35, trans_att_replayed = 0.1, rec_block= 0.05, trans_block = 0.05))

    res1 <- suppressWarnings(vs_simulate_match(rates, n = 5e3, simple = TRUE, serving = TRUE))
    res2 <- suppressWarnings(vs_simulate_match(rates, n = 5e3, simple = TRUE, serving = FALSE))
    res3 <- suppressWarnings(vs_simulate_match(rev(rates), n = 5e3, simple = TRUE, serving = TRUE))
    res4 <- suppressWarnings(vs_simulate_match(rev(rates), n = 5e3, simple = TRUE, serving = FALSE))

    ## the overall win rates for res1 and res2 should be the same, because this comes down to who served first in set 5 (which is random here)
    expect_true(abs(res1$pwin - res2$pwin) < 0.01)

    ## same when using theoretical
    res1t <- vs_simulate_match(rates, serving = TRUE, method = "theoretical")$result_probabilities
    res2t <- vs_simulate_match(rates, serving = FALSE, method = "theoretical")$result_probabilities
    expect_equal(res1t, res2t)

    ## res1 (team 1 serving) should be the same as res4 (teams swapped and team 2 serving)
    ## reorient res3, res4 to original team order
    chk <- c(res1$pwin, unlist(res1$scores)) - c(1-res3$pwin, rev(unlist(res3$scores)))
    expect_true(all(abs(chk) < 0.02))
    chk <- c(res2$pwin, unlist(res2$scores)) - c(1-res4$pwin, rev(unlist(res4$scores)))
    expect_true(all(abs(chk) < 0.02))
})
