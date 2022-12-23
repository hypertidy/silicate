context("test-trip")
skip_if_not_installed("trip")
library(trip)
#dput(trip::walrus818[c(1:4, 161:165), ])
tr <- new("trip", TOR.columns = c("DataDT", "Deployment"), data = structure(list(
  Deployment = c(353L, 353L, 353L, 353L, 354L, 354L, 354L,
                 354L, 354L), DataDT = structure(c(1252987200, 1252990800,
                                                   1252994400, 1252998000, 1253073600, 1253077200, 1253080800,
                                                   1253084400, 1253088000), class = c("POSIXct", "POSIXt"), tzone = "GMT"),
  Wet = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), Forage = c(0L,
                                                          0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)), row.names = c(1L, 2L, 3L,
                                                                                                          4L, 161L, 162L, 163L, 164L, 165L), class = "data.frame"), coords.nrs = 5:6,
  coords = structure(c(281017, 281399, 281209, 281376, 267900,
                       268048, 267623, 267198, 266773, 22532, 22392, 22218, 22175,
                       -9078, -9006, -9063, -9120, -9177), .Dim = c(9L, 2L), .Dimnames = list(
                         c("1", "2", "3", "4", "161", "162", "163", "164", "165"
                         ), c("X_AED170_70", "Y_AED170_70"))), bbox = structure(c(266773,
                                                                                  -9177, 281399, 22532), .Dim = c(2L, 2L), .Dimnames = list(
                                                                                    c("X_AED170_70", "Y_AED170_70"), c("min", "max"))), proj4string = new("CRS",
                                                                                                                                                          projargs = "+proj=aeqd +ellps=WGS84 +lon_0=-170 +lat_0=70"))
test_that("trip decomposition works", {
  expect_equal(sc_coord(tr), structure(list(x_ = c(281017, 281399, 281209, 281376, 267900,
                                                   268048, 267623, 267198, 266773), y_ = c(22532, 22392, 22218,
                                                                                           22175, -9078, -9006, -9063, -9120, -9177), t_ = structure(c(1252987200,
                                                                                                                                                       1252990800, 1252994400, 1252998000, 1253073600, 1253077200, 1253080800,
                                                                                                                                                       1253084400, 1253088000), class = c("POSIXct", "POSIXt"), tzone = "GMT"),
                                            Wet = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), Forage = c(0L,
                                                                                                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)), row.names = c(NA, -9L), class = c("tbl_df",
                                                                                                                                                                        "tbl", "data.frame")))


  expect_equal(sc_vertex(tr)[c("x_", "y_")],
               structure(list(x_ = c(266773, 267198, 267623, 267900, 268048,
                                                    281017, 281209, 281376, 281399),
                              y_ = c(-9177, -9120, -9063, -9078, -9006, 22532, 22218, 22175, 22392)),
                              row.names = c(NA, -9L), class = c("tbl_df", "tbl","data.frame")))

    expect_equal(sc_object(tr), structure(list(trip = 353:354), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame")))
    expect_equal(nrow(sc_edge(tr)), 7L)
    expect_warning(nrow(sc_arc(tr)), "not well-defined")
    expect_equal(nrow(sc_node(tr)), 4L)




    })

