# Functions for use in reports on daily index

# Function for generating table
# make_border_table <- function(border_base, caption_text) {
#
#   border_base_table_ready <- border_base %>%
#     dplyr::select(ukedag, index, ukenr) %>%
#     tidyr::pivot_wider(names_from = ukedag,
#                        values_from = index) %>%
#     dplyr::mutate(periode = paste0("Uke ", ukenr)) %>%
#     dplyr::select(periode, Mandag:'Søndag')
#
#   border_table <- border_base_table_ready %>%
#     flextable() %>%
#     set_header_labels(periode = "") %>%
#     bold(part = "header") %>%
#     bg(bg = "#ED9300", part = "header") %>%
#     border_remove() %>%
#     hline_top(part = "header", border = borderline) %>%
#     hline_bottom(part = "all", border = borderline) %>%
#     colformat_double(digits = 1) %>%
#     autofit() %>%
#     height_all(height = .2) %>%
#     set_caption(caption_text,
#                 autonum = table_numbers,
#                 style = "Tabelltekst")
#
#   return(border_table)
# }


make_border_trp_table <- function(border_trp_dt, caption_text) {

  border_trp_dt_table_ready <- border_trp_dt %>%
    select(-ukenr)

  border_trp_table <- border_trp_dt_table_ready %>%
    flextable() %>%
    set_header_labels(name = "Punktnavn") %>%
    bold(part = "header") %>%
    bg(bg = "#ED9300", part = "header") %>%
    border_remove() %>%
    hline_top(part = "header", border = borderline) %>%
    hline_bottom(part = "all", border = borderline) %>%
    colformat_double(digits = 1) %>%
    autofit() %>%
    height_all(height = .2) %>%
    set_caption(caption_text,
                autonum = table_numbers,
                style = "Tabelltekst")

  return(border_trp_table)
}


make_daily_border_plot <- function(area_index, subtitle_text) {

  area_index %>%
    dplyr::mutate(periode = paste0("Uke \n ", ukenr)) %>%
    dplyr::mutate(periode = factor(periode,
                                   levels = stringr::str_sort(periode, numeric = TRUE))) %>%
    ggplot2::ggplot(aes(ukedag, index)) +
    geom_bar(aes(fill = index > 0), stat = "identity",
             color = "#000000",
             alpha = 0.6) +
    scale_fill_manual(guide = FALSE, breaks = c(FALSE, TRUE),
                      values = c("#ed1c2e", "#58b02c")) +
    geom_hline(yintercept = 0, color = "#000000") +
    facet_grid(periode ~., switch = "y") +
    scale_y_continuous(position = "right",
                       limits = c(-100, 100),
                       breaks = c(-80, 0, 80)) +
    xlab("") +
    ylab("Endring i trafikkmengde (%)\n") +
    ggtitle(label = "Daglig endring i trafikkmengde over riksgrensen i 2021",
            subtitle = subtitle_text) +
    theme(strip.text.y = element_text(angle = 180),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank()) +
    labs(caption = "Data: Statens vegvesen")
}


make_daily_dodged_bar_plot <- function(area_index, subtitle_text) {

  area_index %>%
    dplyr::mutate(periode = paste0("Uke \n ", ukenr)) %>%
    dplyr::mutate(periode = factor(periode,
                                   levels = stringr::str_sort(unique(periode), numeric = TRUE))) %>%
    ggplot2::ggplot(aes(ukedag, index, fill = compared_to)) +
    geom_bar(stat = "identity",
             color = "#000000", alpha = 0.6,
             position = "dodge") +
    scale_fill_manual(#guide = FALSE, #breaks = c(FALSE, TRUE),
                      values = c("2016 - 2019" = "#dadada",
                                 "2020" = "#ed9300"),
                      name = "Sammenligningsperiode") +
    geom_hline(yintercept = 0, color = "#000000") +
    facet_grid(periode ~., switch = "y") +
    scale_y_continuous(position = "right",
                       limits = c(-100, 100),
                       breaks = c(-80, 0, 80)) +
    xlab("") +
    ylab("Endring i trafikkmengde (%)\n") +
    ggtitle(label = "Daglig endring i trafikkmengde over riksgrensen i 2021",
            subtitle = subtitle_text) +
    theme(strip.text.y = element_text(angle = 180),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          legend.position = "bottom") +
    labs(caption = "Data: Statens vegvesen")
}



# Calculate daily point index
calculate_daily_point_index <- function(dt_table, normal_days) {

  trp_index <- dt_table %>%
    dplyr::select(-point_name) %>%
    dplyr::mutate(year = lubridate::year(from),
                  month = lubridate::month(from),
                  weekno = lubridate::isoweek(from),
                  weekday = lubridate::wday(
                    from,
                    week_start = getOption("lubridate.week.start", 1))) %>%
    dplyr::select(-from) %>%
    dplyr::filter(coverage > 99) %>%
    dplyr::left_join(normal_days,
                     by = c(
                       "point_id" = "trp_id",
                       "month" = "month",
                       "weekday" = "day_number_of_week"
                     )) %>%
    dplyr::filter(!is.na(daily_traffic)) %>%
    dplyr::mutate(index = round((total_volume / daily_traffic - 1) * 100,
                                digits = 1),
                  index = dplyr::na_if(index, "Inf")) %>%
    dplyr::left_join(norske_ukedager) %>%
    dplyr::select(trp_id = point_id, ukedag, ukenr = weekno, total_volume, daily_traffic, index) %>%
    dplyr::arrange(factor(ukedag, levels = ukedager))
}
