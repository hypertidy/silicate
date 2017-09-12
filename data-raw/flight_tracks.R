
flight_tracks <- silicate::PATH(sf::read_sf(system.file("extdata", "flight_tracks", "tracks.shp", package = "silicate")))
devtools::use_data(flight_tracks)
