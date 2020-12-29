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
