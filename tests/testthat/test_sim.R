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
