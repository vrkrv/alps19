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

track_df2 <-
  track_df %>%
  slice(-c(1:2)) %>% # FIXME! First day problem
  # mutate(is.stop = speed < (mean(speed, na.rm = T) / 4)) %>%
  group_by(day) %>%
  mutate(is.stop = case_when(speed < (median(speed, na.rm = T) / 4) ~ "stop",
                             speed > (median(speed, na.rm = T) * 4) ~ "ride",
                             T ~ "walk")) %>%
  # fix first point -- is it was just switch on of navigator, without walking:
  mutate_at("is.stop", list(~ifelse(is.na(speed) & (lag(.) == "stop"), "stop", .))) %>%
  filter(is.stop != "stop") %>%
  # recalc speed again:
  mutate(speed = 3.6e3 * dist / as.integer(difftime(time, lag(time))))


# calc elevation and so on:
tab_df <-
  track_df2 %>%
  group_by(day) %>%
  summarise(distance = sum(dist),
            # ghv2 = difftime(last(time), first(time)),
            h_night = last(ele),
            h_max = max(ele))

# calc total elevation / descending:
ele_df <-
  track_df2 %>%
  group_by(day) %>%
  mutate(ele_diff = ele - lag(ele)) %>%
  summarise(ele_positive = sum(ele_diff * (ele_diff > 0), na.rm = T),
            ele_negative = sum(ele_diff * (ele_diff < 0), na.rm = T))

# calc stops:
stop_df <-
  track_df2 %>%
  group_by(day) %>%
  mutate(chv = difftime(time, lag(time))) %>%
  mutate(is.stop = case_when(speed < (mean(speed, na.rm = T) / 5) ~ "stop",
                             # speed > 10 ~ "ride",
                             T ~ "walk")) %>%
  group_by(day, is.stop) %>%
  summarise_at("chv", sum, na.rm = T) %>%
  spread(is.stop, chv) %>%
  mutate(ghv = stop + walk)
# mutate_at("chv", list(~stringr::str_sub(hms::as_hms(.), end = -4L)))

# tab_df %>%
# mutate(chistoe = ghv - chv) %>%
# mutate_at(vars(ghv, chv, chistoe), list(~stringr::str_sub(hms::as.hms(.), end = -4L)))
tab_df2 <-
  tab_df %>%
  left_join(stop_df) %>%
  mutate_at(vars(ghv, stop, walk), list(~stringr::str_sub(hms::as.hms(.), end = -4L))) %>%
  left_join(ele_df)


# stop time:
# track_df2 %>%
#   filter(day == 1) %>%
#   # mutate(is.stop = speed < (mean(speed, na.rm = T) / 4)) %>%
#   group_by(day) %>%
#   mutate(is.stop = case_when(speed < (mean(speed, na.rm = T) / 5) ~ "stop",
#                              speed > (mean(speed, na.rm = T) * 4) ~ "ride",
#                              T ~ "walk")) %>%
#   head(150) %>%
#   ggplot(aes(cumsum(dist), time)) +
#   geom_line() +
#   geom_point(aes(color = is.stop, alpha = .7)) +
#   facet_wrap(~day, scales = 'free')
#
# plotly::ggplotly()
#
# track_df2 %>%
#   filter(day == 4) %>%
#   # mutate(is.stop = speed < (mean(speed, na.rm = T) / 4)) %>%
#   group_by(day) %>%
#   mutate(is.stop = case_when(speed < (median(speed, na.rm = T) / 4) ~ "stop",
#                              speed > (median(speed, na.rm = T) * 4) ~ "ride",
#                              T ~ "walk")) %>%
#   ggplot(aes(cumsum(dist), time)) +
#   geom_line() +
#   geom_point(aes(color = is.stop)) + facet_wrap(~day, scales = 'free')
#   # plotly::ggplotly()





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

# 231-232 -- passo Groce di Valerio
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

add_elevation <- function(wp, zoom = 14) {
  track_df <-
    wp %>%
    as.data.frame %>%
    as.tbl %>%
    select(x = coords.x1, y = coords.x2) %>%
    as.data.frame %>%
    elevatr::get_elev_point("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", "aws", z = zoom)  %>%
    as.data.frame() %>%
    as.tbl

  wp$ele <- track_df$elevation
  return(wp)
}

bh_wp <- readOGR("track/Weisshorn loop.gpx", layer = "track_points") %>% add_elevation()
btr_wp <- readOGR("track/Becca_Trecare.gpx", layer = "track_points") %>% add_elevation()
moul_wp <- readOGR("track/Grand_Moullin.gpx", layer = "track_points") %>% add_elevation()


# add time:
add_time <- function(wp, start, finish) {
  # add uniform time:
  step = difftime(finish, start) / (length(wp) - 1)
  wp$time = seq(start, finish, step)
  return(wp)
}
bh_wp <- add_time(bh_wp,
                  start = ymd_hms("2019-07-23 05:40:16"),
                  finish = ymd_hms("2019-07-23 08:30:16"))
btr_wp <- add_time(btr_wp,
                   start = ymd_hms("2019-07-25 10:40:44"),
                   finish = ymd_hms("2019-07-25 11:50:44"))

moul_wp <- add_time(moul_wp,
                   start = ymd_hms("2019-07-26 08:30:00"),
                   finish = ymd_hms("2019-07-26 10:30:00"))

artificial_track <-
  (bh_wp + btr_wp + moul_wp) %>%
  as.data.frame %>%
  as.tbl %>%
  select(track_seg_point_id, time, ele, coords.x1, coords.x2) %>%
  mutate(day = as.integer(as_date(time) - ymd("20190713"))) %>%
  mutate(dist = c(0, spDists(bh_wp, segments = T),
                  0, spDists(btr_wp, segments = T),
                  0, spDists(moul_wp, segments = T))) %>%
  mutate(label = c(rep("breithorn", length(bh_wp)),
                   rep("becca trecare", length(btr_wp)),
                   rep("grand moulin", length(moul_wp)))) %>%
  # add speed km/h
  group_by(day) %>%
  mutate(speed = 3.6e3 * dist / as.integer(difftime(time, lag(time)))) %>%
  mutate(is.stop = "walk")
  ungroup

track_df3 <-
  track_df2 %>%
  # mutate(label = ifelse(day < 9, "k1", "k2")) %>%
  mutate(label = "clean") %>%
  bind_rows(artificial_track) %>%
  mutate(label2 = ifelse(day < 9, "k1", "k2")) %>%
  arrange(time) %>%
  mutate_at("speed", coalesce, 0) %>%
  ungroup

track_df3$track_seg_point_id <- 1:nrow(track_df3) - 1

wp3 <-
  SpatialPointsDataFrame(coords = track_df3 %>% select(coords.x1, coords.x2) %>% as.data.frame,
                         data = track_df3 %>% mutate_at("time", as.character) %>% as.data.frame,
                         proj4string = wp@proj4string)

wp3 %>% write_rds("data/track_extended.Rds", compress = "bz2")

library(leaflet)
m <-
  leaflet() %>%
  addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
  addLegend(position = 'bottomright',
            opacity = 0.4,
            colors = 'blue',
            labels = 'Пройденный маршрут',
            title = 'Поход 2 к.с. Пеннинские Альпы')

m %>%
  addPolylines(data = Line(wp),
               color = 'blue',
               group = 'orig')  %>%
  addPolylines(data = Line(wp3),
               color = 'green',
               group = 'corr')

  addMarkers(
    data = photo_day1 %>%
      mutate_at(vars(coords.x1, coords.x2), list(~ifelse(lag(.) == ., jitter(., factor = 100), .))) %>%
      mutate(lab = sprintf("%s<br/>N %s, E %s<br/>elevation: %s<br/>%s", time, round(coords.x2, 4), round(coords.x1, 4), ele, FileName)),
    # data = photo_day1,
    lng  = ~coords.x1,
    lat  = ~coords.x2,
    # label = ~sprintf("%s<br/>N %s, E %s; elevation: %s", time, coords.x1, coords.x2, ele)),
    label = ~lapply(lab, htmltools::HTML),
    group = "day1") %>%
  # addPopupImages(sprintf("../photo_day1$SourceFile, group = "day1", width = 480)
  addPopupImages(sprintf("../photo/preview/%s", photo_day1$FileName), group = "day1", width = 480)
# addPopupImages(sprintf("https://yadi.sk/d/ZibKr3f-ZVSunQ/%s", photo_day1$FileName), group = "day1", width = 640, height = 480)
# addPopupImages(sprintf(yadisk, photo_day1$FileName), group = "day1", width = 640, height = 480)
# addPopupImages(photo_day1 %>% transmute(FileName, tmp = glue::glue(yadisk)) %>% pull(tmp), group = "day1", width = 640, height = 480)


# Mini-track --------------------------------------------------------------

photo_df2 <- read_tsv("photo_tech_info2.tsv")
gpx_file <- "../track/Alps2019_MGU_clean.gpx"
wp <- readOGR(gpx_file, layer = "track_points")
track_df <- wp %>% as.data.frame %>% as.tbl %>% mutate_at("time", ymd_hms)

# day_n <- ymd('2019-07-25')
create_map <- function(day_n) {

  photo_day1 <-
    photo_df2 %>%
    filter(CreateDate > day_n,
           CreateDate < day_n + 1)

  track_day1 <-
    track_df %>%
    filter(time > day_n,
           time < day_n + 1) %>%
    mutate_at("time", `+`, 7800)

  sp_track <-  sp::Lines(list(sp::Line(track_day1 %>% select(coords.x1, coords.x2) %>% as.matrix)), "day1")

  m <-
    leaflet() %>%
    addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
    addLegend(position = 'bottomright',
              opacity = 0.4,
              colors = 'blue',
              # labels = sprintf('День %ы',
              title = 'Поход 2 к.с. Пеннинские Альпы') %>%
    addPolylines(
      data = sp_track,
      color = 'blue',
      group = 'day1')  %>%
    addMarkers(
      data = photo_day1 %>%
        mutate_at(vars(coords.x1, coords.x2), list(~ifelse(lag(.) == ., jitter(., factor = 100), .))) %>%
        mutate(lab = sprintf("%s<br/>N %s, E %s<br/>elevation: %s<br/>%s", time, round(coords.x2, 4), round(coords.x1, 4), ele, FileName)),
      # data = photo_day1,
      lng  = ~coords.x1,
      lat  = ~coords.x2,
      label = ~lapply(lab, htmltools::HTML),
      group = "day1") %>%
    addPopupImages(sprintf("photo/preview/%s", photo_day1$FileName), group = "day1", width = 480)
}


  # addPopupImages(sprintf("https://yadi.sk/d/ZibKr3f-ZVSunQ/%s", photo_day1$FileName), group = "day1", width = 640, height = 480)
  # addPopupImages(sprintf(yadisk, photo_day1$FileName), group = "day1", width = 640, height = 480)
  # addPopupImages(photo_day1 %>% transmute(FileName, tmp = glue::glue(yadisk)) %>% pull(tmp), group = "day1", width = 640, height = 480)

  track_day1 %>%
    ggplot(aes(time, ele)) +
    geom_point() +
    scale_x_datetime(date_breaks =  "1 hour", date_labels = "%H:%M")
