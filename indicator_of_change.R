# Indicator of change ----
# https://arxiv.org/abs/2011.14807
# 2020 Brauen - On absolute and relative change
# https://en.wikipedia.org/wiki/Relative_change

# An antisymmetric, additive and normed measure of change
# Either
# \lambda =   1 -> ln (b/a)
# \lambda = 1/2 -> 2 * (sqrt(b) - sqrt(a)) Puts equal weight on absolute and relative change!

# If we compare traffic work instead of volume, the relative change will be the same, but the absolute change will be somewhat different.
# And this will only affect F, not the log difference.

F_pi_i_lambda_half <- function(a, b) {

  F <-  2 * (sqrt(b) - sqrt(a))

}

point_index_tests <-
  tibble::tibble(
    base_volume = c(
      # Same absolute change
      200,
      1000,
      10000,
      10200,
      1200,
      400,
      # No change
      2000,
      # Same relative change
      200,
      2000,
      20000,
      # Additive
      2000,
      2200,
      2000
    ),
    calc_volume = c(
      # Same absolute change
      400,
      1200,
      10200,
      10000,
      1000,
      200,
      # No change
      2000,
      # Same relative change
      205,
      2050,
      20500,
      # Additive
      2200,
      2500,
      2500
    )
  ) |>
  dplyr::mutate(
    abs_change = calc_volume - base_volume,
    rel_change = calc_volume / base_volume,
    log_change = log(calc_volume / base_volume),
    prc_change = 100 * (calc_volume / base_volume - 1),
    log_prc_change = 100 * log(calc_volume / base_volume),
    # f_pi_i_lambda_half = (calc_volume - base_volume) / sqrt(base_volume),
    F = F_pi_i_lambda_half(base_volume, calc_volume)
  ) 

# F for the city doesn't say much. It would still be perilous to compare cities.


# Examples ----
a <- 1000
b <- 1200
c <- 1300

## Antisymmetry ----
ab <- 100 * log(b/a)
ba <- 100 * log(a/b)
# Yes

ab_F <- F_pi_i_lambda_half(a, b)
ba_F <- F_pi_i_lambda_half(b, a)
# Yes


## Additivity again ----
ac <- 100 * log(c/a)
bc <- 100 * log(c/b)

abc <- ab + bc
# Yes

ac_F <- F_pi_i_lambda_half(a, c)
bc_F <- F_pi_i_lambda_half(b, c)

abc_F <- ab_F + bc_F
# Yes

## Normed ----
abn <- log(b/a)
acn <- log(c/a)
bcn <- log(c/b)

abcn <- abn + bcn


# Ratio of private traffic ----
index_i_from_ratio <- function(a) {

  index_i = (1 - 0.12) / (1 - a)
}

as <- seq(10, 20, 0.1) / 100

i_df <-
  tibble::tibble(
    as
  ) |>
  dplyr::mutate(
    is = index_i_from_ratio(as),
    ips = 100 * (is - 1),
    aps = 100 * (as / 0.12 - 1)
  )

plot(as, i_df$ips)
