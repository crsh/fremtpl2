# Libraries ---------------------------------------------------------------

library("targets")
library("tarchetypes")
library("crew")

## Packages for project

project_packages <- c(
  "dplyr"
  , "tidyr"
  , "brms"
  , "loo"
  , "modelr"
  , "tidybayes"
  , "ggplot2"
)

tar_option_set(seed = 1)

# Configure plan execution ------------------------------------------------

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = project_packages
  , controller = crew_controller_local(workers = 3)
  , storage = "main"
  , retrieval = "main"
  , memory = "transient"
  , garbage_collection = TRUE
  # , error = "continue"
  , workspace_on_error = TRUE
  , seed = 1
)

# Define plan -------------------------------------------------------------

list(
  # Prepare data
  tar_target(
    mtpl2
    , {
      mtpl2_freq <- read_parquet("./data/freMTPL2freq.parquet")
      mtpl2_sev <- read_parquet("./data/freMTPL2sev.parquet")

      mtpl2_freq <- mtpl2_freq |>
        filter(ClaimNb == 0 | (ClaimNb > 0 & IDpol %in% mtpl2_sev$IDpol))

      mtpl2_sev_nested <- mtpl2_sev |>
        group_by(IDpol) |>
        summarize(ClaimAmount = list(ClaimAmount))
      
      mtpl2 <- left_join(mtpl2_freq, mtpl2_sev_nested, by = "IDpol") |>
        filter(
          !(ClaimNb > 3 & Area == "D" & VehPower == 4 & VehAge %in% 12:13 & DrivAge %in% 52:53 & BonusMalus == 50 & VehBrand == "B1" & VehGas == "Regular" & Density == 824 & Region == "R91")
        ) |>
        select(-c("Region", "Area"))

        set.seed(8641)
        used_in_exploration <- mtpl2_sample <- sample_n(mtpl2, 33000)

        filter(mtpl2, !IDpol %in% used_in_exploration$IDpol)
    }
    , deployment = "main"
    , packages = c("arrow", "dplyr")
  )
  , tar_target(
    mtpl2_train
    , {
      sample_n(mtpl2, round(nrow(mtpl2) * 0.05))
    }
    , deployment = "main"
    , packages = "dplyr"
  )
  , tar_target(
    mtpl2_test
    , {
      filter(mtpl2, !IDpol %in% mtpl2_train$IDpol)
    }
    , deployment = "main"
    , packages = "dplyr"
  )

  # Model claim number ----------------------------------------------------

  # Specify models
  , tar_target(
    predictors
    , c(
      "s(BonusMalus, bs = 'ts')"
      , "s(DrivAge, bs = 'ts')"
      , "Density"
      , "VehPower"
      , "s(VehAge, bs = 'ts')"
      , "VehGas"
      , "VehBrand"
      , "s(BonusMalus, DrivAge, bs = 'ts')"
    )
    , deployment = "main"
  )
  , tar_target(
    biv_claimnb_formulae
    , {
      paste0("ClaimNb ~ ", predictors, " + offset(log(Exposure))") |>
        formula()
    }
    , pattern = map(predictors)
    , deployment = "main"
  )
  , tar_target(
    baseline_claimnb_fit
    , {
      brm(
        ClaimNb ~ 1 + offset(log(Exposure))
        , data = mtpl2_train
        , family = poisson()
        , warmup = 1000
        , iter = round(4000/3 + 1000)
        , chains = 3
        , cores = 3
        , backend = "cmdstanr"
        , control = list(adapt_delta = 0.90)
      )
    }
    , deployment = "main"
    , packages = "brms"
  )
  , tar_target(
    biv_claimnb_fits
    , {
      brm(
        biv_claimnb_formulae
        , data = mtpl2_train
        , family = poisson()
        , warmup = 1000
        , iter = round(4000/3 + 1000)
        , chains = 3
        , cores = 3
        , backend = "cmdstanr"
        , control = list(adapt_delta = 0.90)
      )
    }
    , pattern = map(biv_claimnb_formulae)
    , deployment = "main"
    , packages = "brms"
    , iteration = "list"
  )

  # Approximate leave-one-out cross-validation
  , tar_target(
    baseline_claimnb_loo
    , {
      loo_subsample(baseline_claimnb_fit, observations = 300, cores = 3)
    }
    , deployment = "main"
    , packages = "brms"
  )
  , tar_target(
    biv_claimnb_loo
    , {
      loo_subsample(biv_claimnb_fits, observations = baseline_claimnb_loo)
    }
    , pattern = map(biv_claimnb_fits)
    , deployment = "worker"
    , packages = "brms"
    , iteration = "list"
  )
  , tar_target(
    biv_claimnb_loo_compare
    , {
      loo_compare(c(list(baseline_claimnb_loo), biv_claimnb_loo))
    }
    , deployment = "main"
    , packages = "loo"
  )

  # Hold-out cross-validation
  , tar_target(
    biv_claimnb_ho_elpd
    , {
        log_lik(biv_claimnb_fits, newdata = mtpl2_test, ndraws = 100) |>
          elpd()
    }
    , pattern = map(biv_claimnb_fits)
    , deployment = "main"
    , packages = c("loo", "brms")
    , iteration = "list"
  )
  , tar_target(
    biv_claimnb_ho_compare
    , {
      loo_compare(biv_claimnb_ho_elpd)
    }
    , deployment = "main"
    , packages = "loo"
  )

  # Model claim amount ----------------------------------------------------

  , tar_target(
    mtpl2_amount_train
    , {
      mtpl2_train |>
        rowwise() |>
        tidyr::unnest(ClaimAmount) |>
        filter(ClaimAmount > 0)
    }
    , deployment = "main"
    , packages = "dplyr"
  )
  , tar_target(
    mtpl2_amount_test
    , {
      mtpl2_test |>
        rowwise() |>
        tidyr::unnest(ClaimAmount) |>
        filter(ClaimAmount > 0)
    }
    , deployment = "main"
    , packages = "dplyr"
  )

  # Specify models
  , tar_target(
    biv_claimamount_formulae
    , {
      paste0("ClaimAmount ~ ", predictors, " + (1 | IDpol)") |>
        formula()
    }
    , pattern = map(predictors)
    , deployment = "main"
  )
  , tar_target(
    baseline_claimamount_fit
    , {
      brm(
        ClaimAmount ~ 1 + (1 | IDpol)
        , data = mtpl2_amount_train
        , family = lognormal()
        , warmup = 1000
        , iter = round(4000/3 + 1000)
        , chains = 3
        , cores = 3
        , backend = "cmdstanr"
        , control = list(adapt_delta = 0.90)
      )
    }
    , deployment = "main"
    , packages = "brms"
  )
  , tar_target(
    biv_claimamount_fits
    , {
      brm(
        biv_claimamount_formulae
        , data = mtpl2_amount_train
        , family = lognormal()
        , warmup = 1000
        , iter = round(4000/3 + 1000)
        , chains = 3
        , cores = 3
        , backend = "cmdstanr"
        , control = list(adapt_delta = 0.90)
      )
    }
    , pattern = map(biv_claimamount_formulae)
    , deployment = "main"
    , packages = "brms"
    , iteration = "list"
  )

  # Approximate leave-one-out cross-validation
  , tar_target(
    baseline_claimamount_loo
    , {
      loo_subsample(baseline_claimamount_fit, observations = 300, cores = 3)
    }
    , deployment = "main"
    , packages = "brms"
  )
  , tar_target(
    biv_claimamount_loo
    , {
      loo_subsample(biv_claimamount_fits, observations = baseline_claimamount_loo, cores = 3)
    }
    , pattern = map(biv_claimamount_fits)
    , deployment = "main"
    , packages = "brms"
    , iteration = "list"
  )
  , tar_target(
    biv_claimamount_loo_compare
    , {
      loo_compare(c(baseline_claimamount_loo, biv_claimamount_loo))
    }
    , deployment = "main"
    , packages = "loo"
  )

  # Hold-out cross-validation
  , tar_target(
    biv_claimamount_ho_elpd
    , {
        log_lik(biv_claimamount_fits, newdata = mtpl2_amount_test) |>
          elpd()
    }
    , pattern = map(biv_claimamount_fits)
    , deployment = "worker"
    , packages = c("loo", "brms")
    , iteration = "list"
  )
  , tar_target(
    biv_claimamount_ho_compare
    , {
      loo_compare(biv_claimamount_ho_elpd)
    }
    , deployment = "main"
    , packages = "loo"
  )

  # Reports
  , tar_quarto(
    exploration
    , "./results/exploration.qmd"
  )
)
