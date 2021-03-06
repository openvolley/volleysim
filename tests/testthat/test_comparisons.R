context("compare theoretical and MC methods")

test_that("set probs to match probs are consistent between methods", {
    rates <- list(
        list(serve_ace = 0.108, serve_error = 0.108, rec_set_error = 0, rec_att_error = 0.071, rec_att_kill = 0.288, rec_att_replayed = 0.179, trans_set_error = 0.05, trans_att_error = 0.04, trans_att_kill = 0.405, trans_att_replayed = 0.135, sideout = 0.630, rec_block = 0.058, trans_block = 0.054),
        list(serve_ace = 0.185, serve_error = 0.185, rec_set_error = 0, rec_att_error = 0.058, rec_att_kill = 0.192, rec_att_replayed = 0.153, trans_set_error = 0.05, trans_att_error = 0.125, trans_att_kill = 0.232, trans_att_replayed = 0.142, sideout = 0.432, rec_block = 0.036, trans_block = 0.027))

    so <- c(rates[[1]]$sideout, rates[[2]]$sideout)
    chk <- win_probabilities_theoretical(so)
    p1 <- chk$result_probabilities
    ## compare to
    m <- set_win_probabilities_theoretical(so)
    pu.s <- m$s.matrix[1,1] ##P(Win | Serve)
    pu.o <- m$o.matrix[1,1] ##P(Win | Opp Serve)
    pu.s5 <- m$s.matrix[11,11] ##P(Win Game 5 | Serve)
    p2 <- vs_set_probs_to_match(pu.s, pu.o, pu.s5)
    expect_true(all(abs(unlist(p1) - unlist(p2)) < 0.01))
})

test_that("theoretical and monte carlo give consistent results", {
    x <- datavolley::dv_read(datavolley::dv_example_file())
    rates <- vs_estimate_rates(x, target_team = "each")
    set.seed(123)
    res_t <- vs_simulate_match(rates = rates, process_model = "phase", method = "theoretical", simple = FALSE)
    res_mo <- vs_simulate_match(rates = rates, process_model = "phase", n = 1e3, method = "monte carlo", simple = FALSE)
    ## overall win prob
    expect_lt(abs(res_t$result_probabilities$pwin - res_mo$pwin), 0.01)
    ## match score distribution
    expect_true(all(abs(unlist(res_t$result_probabilities$scores) - unlist(res_mo$scores)) < 0.02))
    ## compare point breakdowns
    pbd_t <- res_t$points_breakdown
    pbd_mo <- res_mo$points_breakdown14
    names(pbd_mo)[names(pbd_mo) == "proportion"] <- "proportion_mo"
    chk <- merge(pbd_t, pbd_mo)
    chk$diff <- chk$proportion - chk$proportion_mo
    expect_true(all(abs(chk$diff) < 0.01))
})
