library(magick)
library(tidyverse)
library(image.libfacedetection)
library(fs)

find_face_center <- function(image) {
  detections <- image.libfacedetection::image_detect_faces(image)$detections
  dims <- as.list(detections[1, c("x", "y", "width", "height")])
  list(
    x = dims$x + dims$width / 2,
    y = dims$y + dims$height / 2
  )
}

resize_consistent <- function(image, size = 800) {
  info <- image_info(image)
  image_resize(
    image,
    geometry_size_pixels(
      height = if (info$width >= info$height) size,
      width = if (info$height > info$width) size
    )
  )
}

clip_offset <- function(point, range, width) {
  #        point
  # |--[=====*=====]-----|
  #    ^~~ width ~~^
  # ^....................^ range

  if ((point - width / 2) < 0) {
    # must start at left edge
    return(0)
  }
  if ((point + width / 2) > range) {
    # must start at right edge
    return(range - width)
  }
  # enough space on both sides to center width in range
  point - width / 2
}

clip_to_face <- function(image, size = 800) {
  info <- image_info(image)
  size <- min(size, info$width, info$height)

  image <- resize_consistent(image, size)

  info <- image_info(image)
  if (info$width == info$height && info$width == size) {
    return(image)
  }

  face <- find_face_center(image)

  offset <- if (info$width == size) {
    glue::glue("+0+{offset}", offset = clip_offset(face$y, info$height, size))
  } else {
    glue::glue("+{offset}+0", offset = clip_offset(face$x, info$width, size))
  }

  image_crop(
    image,
    geometry = glue::glue(
      "{size}x{size}{offset}"
    )
  )
}

process_image <- function(path, size = 500) {
  cli::cli_process_start("{.path {path}}")
  path |>
    image_read() |>
    clip_to_face(size) |>
    image_write(path("..", path))
}


# Process Photos ----------------------------------------------------------

photos <- dir_ls(regexp = "jpe?g|png$")

# These required manual cropping
manual <- c(
  "academy.png",
  "colin-rundel.png"
)

photos <- setdiff(photos, manual)

# auto-process all the photos
walk(photos, process_image)
