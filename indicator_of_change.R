# Indicator of change ----
# https://arxiv.org/abs/2011.14807
# 2020 Brauen - On absolute and relative change
# https://en.wikipedia.org/wiki/Relative_change

# An antisymmetric, additive and normed measure of change
# Either ln (p) or 2 * (sqrt(v_2) - sqrt(v_1))

point_index_tests <-
  tibble::tibble(
    base_volume = c(
      100,
      1000,
      10000,
      10200,
      1200,
      200,
      2000,
      2000,
      2000,
      20000,
      20050,
      20000,
      1000000,
      1000900,
      1000000,
      100
    ),
    calc_volume = c(
      200,
      1200,
      10200,
      10000,
      1000,
      100,
      2000,
      2050,
      2100,
      20050,
      20250,
      20250,
      1000900,
      1000000,
      1010000,
      101
    )
  ) |>
  dplyr::mutate(
    pi_i = calc_volume / base_volume,
    pi_p = 100 * (calc_volume / base_volume - 1),
    pi_i_ln = 100 * log(pi_i),
    f_pi_i_lambda_half = (calc_volume - base_volume) / sqrt(base_volume),
    F_pi_i_lambda_half = 2 * (sqrt(calc_volume) - sqrt(base_volume))
  ) |>
  dplyr::summarise(
    pi_i = sum(calc_volume) / sum(base_volume),
    total_F_pi_i_lambda_half = 2 * (sqrt(sum(calc_volume)) - sqrt(sum(base_volume))),
    mean_F_pi_i_lambda_half = mean(F_pi_i_lambda_half)
  )

sum(point_index_tests$f_pi_i_lambda_half[10:11])
sum(point_index_tests$F_pi_i_lambda_half[10:11])

# I.e. F is additive, but f is not.
# F has high values when comparing monthly traffic, but this will of course be smaller when we would compare monthly daily traffic.

# F for the city doesn't say much. It would still be perilous to compare cities.


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
