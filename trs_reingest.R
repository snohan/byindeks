# TRS and device history

# Get all trs with trp
trs_history <- get_trs_history()

# Remove bicycle trs!
trs_info <- get_trs_info()

trs_bike <- trs_info %>%
  filter(traffic_type == "BICYCLE")

trs_vehicle_periodic <- trs_info %>%
  filter(traffic_type == "VEHICLE",
         station_type == "PERIODIC")

# How many trs?
trs_vehicle_cont_distinct <- trs_history %>%
  select(1) %>%
  distinct() %>%
  filter(!(trs_id %in% trs_bike$trs_id)) %>%
  filter(!(trs_id %in% trs_vehicle_periodic$trs_id))

trs_lm <- trs_history %>%
  select(trs_id, deviceType) %>%
  filter(deviceType == "LOOP_MONITOR") %>%
  distinct() %>%
  filter(!(trs_id %in% trs_bike$trs_id))

trs_emu <- trs_history %>%
  select(trs_id, deviceType) %>%
  filter(deviceType == "EMU") %>%
  distinct() %>%
  filter(!(trs_id %in% trs_bike$trs_id)) %>%
  mutate(trs_id = as.numeric(trs_id)) %>%
  arrange(trs_id)

# LMs already reingested
trs_reingested <- read_csv("trs_ingested.txt") %>%
  mutate(trs_id = as.character(trs_id))

trs_lm_not_ingested <- trs_lm %>%
  select(trs_id) %>%
  filter(!(trs_id %in% trs_reingested$trs_id)) %>%
  mutate(trs_id = as.numeric(trs_id)) %>%
  arrange(trs_id)

# Write newline separated file so they can be copied to Trello
write_lines(trs_lm_not_ingested$trs_id, "lm_not_ingested.txt")
write_lines(trs_emu$trs_id, "emu_not_ingested.txt")
# Åpne i Notepad++
#cat(trs_lm_not_ingested$trs_id[1:3], sep = "\n")

