context("markovchain functions")

test_that("sideout rates match between markovchain and other", {
    rates <- list(
        list(serve_ace = 0.1, serve_error = 0.1, rec_set_error = 0, rec_att_error = 0.1, rec_att_kill = 0.5, rec_att_replayed = 0.1, trans_set_error = 0,
             trans_att_error = 0.1, trans_att_kill = 0.4, trans_att_replayed = 0.1, rec_block= 0.05, trans_block = 0.05),
        list(serve_ace = 0.1, serve_error = 0.1, rec_set_error = 0, rec_att_error = 0.1, rec_att_kill = 0.4, rec_att_replayed = 0.1, trans_set_error = 0,
             trans_att_error = 0.1, trans_att_kill = 0.35, trans_att_replayed = 0.1, rec_block= 0.05, trans_block = 0.05))
    for (tt in 1:2) {
        rates[[tt]]$rec_loss <- rates[[tt]]$rec_set_error + (1 - rates[[tt]]$rec_set_error) * (rates[[tt]]$rec_att_error + rates[[3 - tt]]$rec_block)
        rates[[tt]]$rec_win <- (1 - rates[[tt]]$rec_set_error) * rates[[tt]]$rec_att_kill
        rates[[tt]]$rec_replayed <- (1 - rates[[tt]]$rec_set_error) * rates[[tt]]$rec_att_replayed
        rates[[tt]]$trans_loss <- rates[[tt]]$trans_set_error + (1 - rates[[tt]]$trans_set_error) * (rates[[tt]]$trans_att_error + rates[[3 - tt]]$trans_block)
        rates[[tt]]$trans_win <- (1 - rates[[tt]]$trans_set_error) * rates[[tt]]$trans_att_kill
        rates[[tt]]$trans_replayed <- (1 - rates[[tt]]$trans_set_error) * rates[[tt]]$trans_att_replayed
    }

    MM <- rates_to_MC(rates, process_model = "phase")
    MMs <- rates_to_MC(rates, process_model = "phase_simple")
    expect_equal(MC_sideout_rates(MM), MC_sideout_rates(MMs))
    expect_equal(MC_sideout_rates(rev(MM)), MC_sideout_rates(rev(MMs)))

    expect_equal(MC_sideout_rates(MM), estimate_sideout_rates(rates[[1]], rates[[2]]))
    expect_equal(MC_sideout_rates(rev(MM)), estimate_sideout_rates(rates[[2]], rates[[1]]))

})

test_that("phase and phase_simple process models are consistent", {
    x <- datavolley::dv_read(datavolley::dv_example_file())
    rates <- vs_estimate_rates(x, target_team = "each")

    ## these will be close but not identical
    expect_true(all(abs(vs_theoretical_sideout_rates(rates, process_model = "phase") -
                        vs_theoretical_sideout_rates(rates, process_model = "phase_simple")) < 0.01))

    expect_true(all(abs(vs_simulate_set(rates, process_model = "phase") -
                        vs_simulate_set(rates, process_model = "phase_simple")) < 0.01))

    ## and those will be different from this:
    ##vs_simulate_set(rates, process_model = "sideout")
    ## because this uses observed actual sideout rates, which won't necessarily be the same as the theoretical sideout rates from the process models
})
