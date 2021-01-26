context("markovchain functions")

test_that("sideout rates match between markovchain and other", {
    rates <- list(
        list(serve_ace = 0.1, serve_error = 0.1, rec_loss_other = 0, rec_att_error = 0.1, rec_att_kill = 0.5, rec_att_replayed = 0.1, rec_no_att = 0.1, trans_loss_other = 0,
             trans_att_error = 0.1, trans_att_kill = 0.4, trans_att_replayed = 0.1, trans_no_att = 0.1, rec_block= 0.05, trans_block = 0.05),
        list(serve_ace = 0.1, serve_error = 0.1, rec_loss_other = 0, rec_att_error = 0.1, rec_att_kill = 0.4, rec_att_replayed = 0.1, rec_no_att = 0.1, trans_loss_other = 0,
             trans_att_error = 0.1, trans_att_kill = 0.35, trans_att_replayed = 0.1, trans_no_att = 0.1, rec_block= 0.05, trans_block = 0.05))
    for (tt in 1:2) {
        rates[[tt]]$rec_loss <- rates[[tt]]$rec_loss_other + (1 - rates[[tt]]$rec_loss_other - rates[[tt]]$rec_no_att) * (rates[[tt]]$rec_att_error + rates[[3 - tt]]$rec_block)
        rates[[tt]]$rec_win <- (1 - rates[[tt]]$rec_loss_other - rates[[tt]]$rec_no_att) * rates[[tt]]$rec_att_kill
        rates[[tt]]$rec_replayed <- (1 - rates[[tt]]$rec_loss_other - rates[[tt]]$rec_no_att) * rates[[tt]]$rec_att_replayed
        rates[[tt]]$trans_loss <- rates[[tt]]$trans_loss_other + (1 - rates[[tt]]$trans_loss_other - rates[[tt]]$trans_no_att) * (rates[[tt]]$trans_att_error + rates[[3 - tt]]$trans_block)
        rates[[tt]]$trans_win <- (1 - rates[[tt]]$trans_loss_other - rates[[tt]]$trans_no_att) * rates[[tt]]$trans_att_kill
        rates[[tt]]$trans_replayed <- (1 - rates[[tt]]$trans_loss_other - rates[[tt]]$trans_no_att) * rates[[tt]]$trans_att_replayed
    }

    MM <- rates_to_MC(rates, process_model = "phase")
    MMs <- rates_to_MC(rates, process_model = "phase_simple")
    expect_equal(MC_sideout_rates(MM), MC_sideout_rates(MMs))
    expect_equal(MC_sideout_rates(rev(MM)), MC_sideout_rates(rev(MMs)))
})

test_that("phase and phase_simple process models are consistent", {
    skip("skipping until phase_simple rate estimation revised")
    x <- datavolley::dv_read(datavolley::dv_example_file())
    rates <- vs_estimate_rates(x, target_team = "each")

    ## these will be close but not identical
    rates$rec_prop_attacked <- 1 - rates$rec_loss_other - rates$rec_no_att
    rates$trans_prop_attacked <- 1 - rates$trans_loss_other - rates$trans_no_att
    expect_true(all(abs(rates$rec_prop_attacked * rates$rec_att_kill - rates$rec_win) < 0.005))
    expect_true(all(abs(rates$rec_loss_other + rates$rec_prop_attacked * (rates$rec_att_error) - rates$rec_loss) < 0.005))
    expect_true(all(abs(rates$rec_prop_attacked * rates$rec_att_replayed - rates$rec_replayed) < 0.02)) ## should this be closer?
    ## transition wins are attack kills PLUS (all) block kills
    expect_true(all(rates$trans_prop_attacked * rates$trans_att_kill <= rates$trans_win))
    ## being blocked is not a trans loss, it is a trans_win for the other team
    expect_true(all(abs(rates$trans_loss_other + rates$trans_prop_attacked * rates$trans_att_error - rates$trans_loss) < 0.02)) ## should this be closer?
    expect_equal(rates$trans_prop_attacked * rates$trans_att_replayed, rates$trans_replayed)

    expect_true(all(abs(vs_theoretical_sideout_rates(rates, process_model = "phase") -
                        vs_theoretical_sideout_rates(rates, process_model = "phase_simple")) < 0.02)) ## expect closer?

    expect_lt(abs(vs_simulate_set(rates, process_model = "phase") -
                        vs_simulate_set(rates, process_model = "phase_simple")), 0.02) ## expect closer?

    ## and those will be different from this:
    ##vs_simulate_set(rates, process_model = "sideout")
    ## because this uses observed actual sideout rates, which won't necessarily be the same as the theoretical sideout rates from the process models
})
