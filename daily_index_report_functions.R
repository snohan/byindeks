# Functions for use in reports on daily index

make_daily_plot <- function(area_index) {

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
                       limits = c(-50, 50),
                       breaks = c(-20, 0, 200)) +
    xlab("") +
    ylab("Endring i trafikkmengde (%)\n") +
    ggtitle(label = "Daglig endring i trafikkmengde i 2021",
            subtitle = "Sammenlignet med normalperioden 2016-2019") +
    theme(strip.text.y = element_text(angle = 180),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank()) +
    labs(caption = "Data: Statens vegvesen")
}

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


make_daily_dodged_bar_plot <- function(area_index, subtitle_text, x_min, x_max) {

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
                       limits = c(x_min - 20, x_max + 20),
                       breaks = c(x_min, 0, x_max)) +
    xlab("") +
    ylab("Endring i trafikkmengde (%)\n") +
    ggtitle(label = "Daglig endring i trafikkmengde i 2021",
            subtitle = subtitle_text) +
    theme(strip.text.y = element_text(angle = 180),
          axis.text.y = element_text(size = 7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          legend.position = "bottom") +
    labs(caption = "Data: Statens vegvesen og fylkeskommunene")
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
    dplyr::select(trp_id = point_id, area_name,
                  ukedag, ukenr = weekno, total_volume, daily_traffic, index) %>%
    dplyr::arrange(factor(ukedag, levels = ukedager))
}

calculate_daily_easter_point_index <- function(easter_to_compare, easter_to_compare_by) {

  trp_index <- easter_to_compare %>%
    dplyr::select(point_id, index_total_volume = total_volume,
                  easter_day, index_period = period) %>%
    dplyr::inner_join(easter_to_compare_by,
                     by = c(
                       "point_id" = "point_id",
                       "easter_day" = "easter_day"
                     )) %>%
    dplyr::mutate(index = round((index_total_volume / total_volume - 1) * 100,
                                digits = 1),
                  index = dplyr::na_if(index, "Inf"),
                  compared = stringr::str_c(index_period, period, sep = " mot ")) %>%
    dplyr::left_join(trps,
                     by = c(
                       "point_id" = "trp_id")) %>%
    dplyr::select(trp_id = point_id, area_name, name, road_reference, municipality_name,
                  easter_day, compared, index_total_volume, total_volume, index) %>%
    dplyr::arrange(area_name, factor(easter_day, levels = easter_days))
}

calculate_area_index_easter_per_day <- function(trp_index) {

  area_index <- trp_index %>%
    dplyr::select(trp_id, area_name, compared, easter_day, index_total_volume, total_volume) %>%
    dplyr::group_by(area_name, compared, easter_day) %>%
    dplyr::summarise(volume_this_year = sum(index_total_volume),
                     volume_to_compare_by = sum(total_volume),
                     n_points = n()) %>%
    dplyr::mutate(index = round((volume_this_year / volume_to_compare_by - 1) * 100,
                                digits = 1),
                  easter_day = factor(easter_day, levels = easter_days)) %>%
    dplyr::select(area_name, compared, n_points, easter_day, index) %>%
    dplyr::arrange(area_name, compared, easter_day)
}

calculate_area_index_easter <- function(trp_index) {

  area_index <- trp_index %>%
    dplyr::select(trp_id, area_name, compared, index_total_volume, total_volume) %>%
    dplyr::group_by(area_name, compared) %>%
    dplyr::summarise(volume_this_year = sum(index_total_volume),
                     volume_to_compare_by = sum(total_volume),
                     n_points = n() / 11) %>%
    dplyr::mutate(index = round((volume_this_year / volume_to_compare_by - 1) * 100,
                                digits = 1)) %>%
    dplyr::select(area_name, compared, n_points, index) %>%
    dplyr::arrange(area_name, compared)
}




