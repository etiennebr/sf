context("sf: tibble & dplyr")

suppressMessages(library(tibble))
suppressMessages(library(dplyr))


nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE) %>% 
  as_tibble %>% 
  rename(geography = geometry)

test_that("respects the tibble guides", {
  expect_true(nchar(type_sum(nc$geography)) <= 6)
  expect_true(all(nchar(obj_sum(nc$geography)) <= 15))
})

test_that("can convert to tibble", {
  expect_true(inherits(nc, "tbl_df"))
  expect_true(inherits(nc$geography, "sfc"))
})

test_that("can mutate on geometry", {
  nc_mute <- nc %>% 
    mutate(geometry = st_transform(geography, 26917),
           buffer   = st_buffer(geometry, 10),
           area = st_area(geography))
  expect_true(inherits(nc_mute, "tbl_df"))
  expect_equal(names(nc_mute), c(names(nc), "geometry", "buffer", "area"))
})

test_that("can mutate without geometry", {
  nc_mute <- nc %>% 
    mutate(perimeter_ft = PERIMETER * 3.28084)
  expect_true(inherits(nc_mute, "tbl_df"))
  expect_equal(names(nc_mute), c(names(nc), "perimeter_ft"))
})

test_that("can select", {
  expect_true(nc %>% select(AREA) %>% inherits("tbl_df"))
  expect_true(nc %>% select(geography) %>% inherits("tbl_df"))
  expect_equal(nc %>% select(AREA) %>% names, "AREA")
  expect_equal(nc %>% select(geography) %>% names, "geography")
})

test_that("can filter", {
  expect_error(nc %>% filter(geography > 1), "not supported")
  expect_error(nc %>% filter(geography == 1), "not supported")
  expect_equal(nc %>% filter(AREA > quantile(AREA, 0.5)) %>% nrow, nrow(nc) / 2)
})

test_that("can summarise", {
  expect_error(nc %>% group_by(geography))
  expect_silent(nc %>% group_by(st_area(geography)))
  expect_silent(nc %>% group_by(SID79))
  
  expect_silent(nc %>% group_by(SID79) %>% summarise(total_area = sum(AREA)))
  expect_silent(nc %>% group_by(SID79) %>% 
                  summarise(total_area = sum(st_area(geography))))
  
  expect_silent(nc %>% group_by(SID79) %>% 
                  summarise(geog = st_union(geography), 
                            area_sum = sum(st_area(geography)),
                            area_tot = st_area(geog))
                # geog is a list of polygons, doesn't play well with st_area
  )
})
