raw_chars <- c(LETTERS, letters, 0:9)
viridis_cols <- viridis::viridis(256)
usethis::use_data(viridis_cols,raw_chars, internal = TRUE)
