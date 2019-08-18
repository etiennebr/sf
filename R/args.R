is_sfg <- function(x) {
	inherits(x, "sfg")
}

is_sfc <- function(x) {
	inherits(x, "sfc")
}

is_sf <- function(x) {
	inherits(x, "sf")
}

check_crs <- function(x) {
	if(is_sf(x) || is_sfc(x) || is_sfg(x) || is.na(x) || is.numeric(x) || is.character(x) || is_crs(x)) {
		return(x)
	}
	stop("Cannot create a crs from an object of class ", class(x), call. = FALSE)
}

is_crs <- function(x) {
	inherits(x, "sf_crs")
}
