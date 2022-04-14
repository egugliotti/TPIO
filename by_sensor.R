# arranged by Orbit_Type2, Platform, `Date End of Life (EOL)`

by_sensor<- function(dat, sensor) {
  dat1<- dat %>% 
    filter(network_acronym == "CGMS") %>%
    filter(grepl("CGMS", platform_deployment_plans)) %>%
    mutate(platform_deployment_plans = sub(".*CGMS Information - ","",platform_deployment_plans)) %>%
    separate(platform_deployment_plans, sep = "[\\|]", c("dep_one","dep_two","dep_three","dep_four")) %>%
    pivot_longer(cols = starts_with("dep_"), names_to = "extra", values_to = "deployment_plans") %>% 
    select(-extra) %>%
    separate(deployment_plans, sep = ":", c("type", "value")) %>% 
    filter(type == "Sensors") %>% 
    mutate(value = trimws(value, "l")) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    separate_rows(Sensors, sep = "; ") %>% 
    filter(Sensors == sensor) %>%
    mutate(orbit_crossing_time = as.POSIXct(orbit_crossing_time, format = "%H:%M")) %>%
    mutate(orbit_crossing_time_simple = hour(orbit_crossing_time)) %>%
    mutate(orbit_crossing_category = ifelse(is.na(orbit_crossing_time_simple), orbit_crossing_time_simple, ifelse(
      orbit_crossing_time_simple<8, "Early AM", 
      ifelse(orbit_crossing_time_simple>=7 & orbit_crossing_time_simple<12, "Mid AM", "PM")))) %>%
    mutate(orbit = orbit_type) %>% 
    mutate(satellite_longitude_deg = as.numeric(satellite_longitude_deg)) %>%
    mutate(orbit = ifelse(orbit == "Geostationary" & satellite_longitude_deg < 0, paste0("GEO ",-satellite_longitude_deg," W"),
                          ifelse(orbit == "Geostationary" &satellite_longitude_deg > 0, paste0("GEO ",satellite_longitude_deg," E"),
                                 ifelse(orbit == "Geostationary" & satellite_longitude_deg == 0, paste0("GEO ", satellite_longitude_deg), orbit)))) %>%
    mutate(orbit = ifelse(orbit == "Sun-synchronous", paste0("Sun-synchronous ", orbit_crossing_category), orbit)) %>%
    # mutate(orbit = ifelse(is.na(orbit) ~ "TBD")) %>%
    mutate(orbit_type = ifelse(orbit_type == "Geostationary", satellite_longitude_deg, orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Sun-synchronous", orbit_crossing_category, orbit_type)) %>%
    mutate(orbit_type = ifelse(is.na(orbit_type), "TBD", orbit_type)) %>%
    select(platform_acronym, date_type, date,  orbit_type, orbit, orbit_crossing_category, satellite_longitude_deg, orbit_inclination_deg) %>% 
    distinct(platform_acronym, date_type,  .keep_all = TRUE) %>% 
    pivot_wider(names_from = date_type, values_from = date) %>%
    filter(!is.na(`Date Launched`)) %>%
    filter(!is.na(`Date End of Life (EOL)`)) %>%
    filter(!is.na(platform_acronym))
  
  dat_eol<- dat1 %>%
    select(platform_acronym, `Date End of Life (EOL)`) %>%
    rename(Platform = platform_acronym)
  
  M <- Map(seq, dat1$`Date Launched`, dat1$`Date End of Life (EOL)`, by = "year")
  
  dat2<-data.frame(
    Platform = rep.int(dat1$platform_acronym, vapply(M, length, 1L)),
    Orbit_Type = rep.int(dat1$orbit_type, vapply(M, length, 1L)),
    Orbit = rep.int(dat1$orbit, vapply(M, length, 1L)),
    Year = as.Date(do.call(c, M), format = "%m-%d-%Y"))
  
  # orbit_inclination_category = rep.int(dat1$orbit_inclination_category, vapply(M, length, 1L))
  # need to figure out how to reorder factor levels
  
  dat_wide<- dat2 %>%
    filter(Year >= "2022-01-01" & Year <= "2032-12-31") %>%
    arrange(Year) %>%
    mutate(Year = year(Year)) %>%
    mutate(Orbit_Type2 = Orbit_Type) %>%
    pivot_wider(names_from = Year, values_from = Orbit_Type) %>% 
    left_join(dat_eol, by = "Platform") %>%
    mutate(Orbit = as.factor(Orbit)) %>%
    mutate(Orbit_Type2 = as.factor(Orbit_Type2)) %>%
    mutate(Platform = as.factor(Platform)) %>%
    arrange(Orbit_Type2, Platform, `Date End of Life (EOL)`) %>% 
    select(-`Date End of Life (EOL)`, -Orbit_Type2) %>%
    as.data.frame()
  
  
  dat_wide
}