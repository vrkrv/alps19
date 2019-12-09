pacman::p_load(tidyverse, readxl, sp, rgdal, lubridate)
library(ggrepel)

wp <- readOGR("track/Alps2019_MGU_clean.gpx", layer = "track_points")
track_df <-
  wp %>%
  as.data.frame %>%
  as.tbl %>%
  mutate_at("time", ymd_hms) %>%
  mutate_at("time", `+`, 7800) %>%
  select(track_seg_point_id, time, ele, coords.x1, coords.x2) %>%
  mutate(day = as.integer(as_date(time) - ymd("20190713"))) %>%
  mutate(dist = c(0, spDists(wp, segments = T))) %>%
  # add speed km/h
  group_by(day) %>%
  mutate(speed = 3.6e3 * dist / as.integer(difftime(time, lag(time)))) %>%
  ungroup

tab_df <-
  track_df %>%
  group_by(day) %>%
  summarise(distance = sum(dist),
            ghv = difftime(last(time), first(time)),
            h_night = last(ele),
            h_max = max(ele)) #%>%
  # mutate_at("ghv", list(~stringr::str_sub(hms::as_hms(.), end = -4L)))

# stop time:
track_df2 %>%
  filter(day == 2) %>%
  # mutate(is.stop = speed < (mean(speed, na.rm = T) / 4)) %>%
  group_by(day) %>%
  mutate(is.stop = case_when(speed < (mean(speed, na.rm = T) / 4) ~ "stop",
                             speed > (mean(speed, na.rm = T) * 4) ~ "ride",
                             T ~ "walk")) %>%
  ggplot(aes(cumsum(dist), time)) +
  geom_line() +
  geom_point(aes(color = is.stop)) + facet_wrap(~day, scales = 'free')

plotly::ggplotly()

track_df %>%
  filter(day == 5) %>%
  # mutate(is.stop = speed < (mean(speed, na.rm = T) / 4)) %>%
  group_by(day) %>%
  mutate(is.stop = case_when(speed < (median(speed, na.rm = T) / 4) ~ "stop",
                             speed > (median(speed, na.rm = T) * 4) ~ "ride",
                             T ~ "walk")) %>%
  ggplot(aes(cumsum(dist), time)) +
  geom_line() +
  geom_point(aes(color = is.stop)) + facet_wrap(~day, scales = 'free')
  # plotly::ggplotly()

track_df2 <-
  track_df %>%
  slice(-c(1:2)) %>% # FIXME! First day problem
  # mutate(is.stop = speed < (mean(speed, na.rm = T) / 4)) %>%
  group_by(day) %>%
  mutate(is.stop = case_when(speed < (median(speed, na.rm = T) / 4) ~ "stop",
                             speed > (median(speed, na.rm = T) * 4) ~ "ride",
                             T ~ "walk")) %>%
  # fix first point -- is it was just switch on, without walking:
  mutate_at("is.stop", list(~ifelse(is.na(speed) & (lag(.) == "stop"), "stop", .))) %>%
  filter(is.stop != "stop") %>%
  # recalc speed again:
  mutate(speed = 3.6e3 * dist / as.integer(difftime(time, lag(time))))



stop_df <-
  track_df2 %>%
  group_by(day) %>%
  mutate(chv = difftime(time, lag(time))) %>%
  mutate(is.stop = case_when(
    speed < ((speed - mean(speed, na.rm = T)) / sd(speed, na.rm = T)) ~ "stop",
    # speed > 10 ~ "ride",
    T ~ "walk")) %>%
  # add stop point before:
  # mutate_at("is.stop", list(~ifelse(lead(.) & !., TRUE, .))) %>%
  # filter(is.stop == "stop") %>%
  group_by(day, is.stop) %>%
  summarise_at("chv", sum, na.rm = T) %>%
  spread(is.stop, chv) %>%
  mutate(ghv = stop + walk)
  # mutate_at("chv", list(~stringr::str_sub(hms::as_hms(.), end = -4L)))

# tab_df %>%
  # mutate(chistoe = ghv - chv) %>%
  # mutate_at(vars(ghv, chv, chistoe), list(~stringr::str_sub(hms::as.hms(.), end = -4L)))
stop_df %>%
  mutate_at(vars(ghv, stop, walk), list(~stringr::str_sub(hms::as.hms(.), end = -4L)))



# Clean original track ----------------------------------------------------

#' Remove rand - Zermatt
#' Remove Trockener Steg
#' Add breithorn summit
#' Add Becca trecaria
#' Add Grand moulin track
k1_df <- track_df %>% filter(day < 9)
k2_df <- track_df %>% filter(day >= 9)

k1_df %>%
  filter(day == 1) %>%
  mutate_at("dist", cumsum) %>%
  # head(40) %>%
  # tail(40) %>%
  ggplot(aes(dist, ele, color = speed > 7)) +
  # geom_text_repel(aes(label = track_seg_point_id)) +
  geom_line(size = 3)

# 231-232 -- passo Gr...
# 249-250 -- passo di Campo
# 287 -- mini pass before passo di Pontimia
# 314 -- passo di Pontimia
# 609 -- Zwischbergenpass
# 952 -- Mischabelhutte
# 1050 -- Windjoch
# 1069 -- Ulrichshorn
# 1219 -- Mittelbergpass
# 1390 -- unexpected rundgagn on Unter Europaweg




# Add missing cuts from files ---------------------------------------------


bh_wp <- readOGR("track/Weisshorn loop.gpx", layer = "track_points")

track_bh <-
  bh_wp %>%
  as.data.frame %>%
  as.tbl %>%
  select(x = coords.x1, y = coords.x2) %>%
  as.data.frame %>%
  elevatr::get_elev_point("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", "aws", z = 14)  %>%
  as.data.frame() %>%
  as.tbl

bh_wp$ele <- track_bh$elevation

# FIXME! add time points (uniformly)
bh_dist = sum(spDists(bh_wp, segments =  TRUE))

