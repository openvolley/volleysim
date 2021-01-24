context("Test rates functions")

test_that("vs_estimate_rates returns the correct rates for a given process model", {
    x <- datavolley::dv_read(datavolley::dv_example_file())
    rates <- vs_estimate_rates(x, target_team = "each", process_model = "all")
    for (pm in c("sideout", "phase", "phase_simple")) blah <- precheck_rates(rates, process_model = pm)

    rates <- vs_estimate_rates(x, target_team = "each", process_model = "sideout")
    for (pm in c("sideout")) blah <- precheck_rates(rates, process_model = pm)
    for (pm in c("phase", "phase_simple")) expect_error(blah <- precheck_rates(rates, process_model = pm))

    rates <- vs_estimate_rates(x, target_team = "each", process_model = "phase_simple")
    for (pm in c("sideout", "phase_simple")) blah <- precheck_rates(rates, process_model = pm)
    for (pm in c("phase")) expect_error(blah <- precheck_rates(rates, process_model = pm))

    rates <- vs_estimate_rates(x, target_team = "each", process_model = "phase")
    for (pm in c("sideout", "phase")) blah <- precheck_rates(rates, process_model = pm)
    for (pm in c("phase_simple")) expect_error(blah <- precheck_rates(rates, process_model = pm))

    ## and when doing by match
    x2 <- x
    x2$plays$match_id <- "ljfdlafoidn"
    x <- rbind(x$plays, x2$plays)
    rates <- vs_estimate_rates(x, target_team = "each", process_model = "all", by = "match")
    for (pm in c("sideout", "phase", "phase_simple")) blah <- precheck_rates(rates, process_model = pm)

    rates <- vs_estimate_rates(x, target_team = "each", process_model = "sideout", by = "match")
    for (pm in c("sideout")) blah <- precheck_rates(rates, process_model = pm)
    for (pm in c("phase", "phase_simple")) expect_error(blah <- precheck_rates(rates, process_model = pm))

    rates <- vs_estimate_rates(x, target_team = "each", process_model = "phase_simple", by = "match")
    for (pm in c("sideout", "phase_simple")) blah <- precheck_rates(rates, process_model = pm)
    for (pm in c("phase")) expect_error(blah <- precheck_rates(rates, process_model = pm))

    rates <- vs_estimate_rates(x, target_team = "each", process_model = "phase", by = "match")
    for (pm in c("sideout", "phase")) blah <- precheck_rates(rates, process_model = pm)
    for (pm in c("phase_simple")) expect_error(blah <- precheck_rates(rates, process_model = pm))
})
