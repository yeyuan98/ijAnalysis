.ROIsavePoint_py <- "
# reticulate, saving one ROI point with roifile

from roifile import *
import numpy

def saveRoiPoint(pointCoord, nchannel, path):
  x = int(pointCoord[0])
  fx = pointCoord[0]
  y = int(pointCoord[1])
  fy = pointCoord[1]
  z = int(pointCoord[2])
  cz = int(z*nchannel)
  ROIname = str(cz).zfill(4)+'-'+str(y).zfill(4)+'-'+str(x).zfill(4)
  roi = ImagejRoi(
    roitype=ROI_TYPE.POINT,
    options=ROI_OPTIONS.SHOW_LABELS | ROI_OPTIONS.SUB_PIXEL_RESOLUTION,
    name=ROIname, version=228,
    top=y,
    left=x,
    bottom=y+1,
    right=x+1,
    n_coordinates=1,
    stroke_width=1,
    z_position=z,
    t_position=1,
    counters=numpy.array([0], dtype=numpy.uint8),
    counter_positions=numpy.array([cz], dtype=numpy.uint32),
    integer_coordinates=numpy.array([
      [0, 0]], dtype=numpy.int32),
    subpixel_coordinates=numpy.array([
      [fx, fy]], dtype='>f4'),
  )
  roi.tofile(path)
"

#' Initialize an ImageJ ROI point set saver function
#'
#' @param use_python Path to the Python binary to use.
#'
#' @return A ROI saver function.
#' @export
#'
#' @examples
#' # TODO
ij_RoiPointsSaver <-
  function(use_python){

    # Initialize reticulate
    reticulate::use_python(use_python)
    # Write the python script to be sourced
    py_file <- tempfile()
    sink(py_file)
    cat(.ROIsavePoint_py)
    cat("\n")
    sink()
    # Source the script
    reticulate::source_python(py_file)


    # Helper function - feed each point to saveRoiPoint
    getPoint <- function(points, idx){
      px <- vctrs::field(points, "x")
      py <- vctrs::field(points, "y")
      pz <- vctrs::field(points, "z")
      return(c(px[idx], py[idx], pz[idx]))
    }

    # Helper function - save ROIs to a directory
    dirSaver <- function(points, dir.path){

      # Check that points has correct data
      if (!inherits(points, "y3628_metaRcrd"))
        rlang::abort("points must be a metaRcrd")
      pf <- vctrs::fields(points)
      if (!all("x" %in% pf, "y" %in% pf, "z" %in% pf))
        rlang::abort("points must contain x,y,z fields")

      # Save points
      for (i in seq_along(points)){
        point.path <-
          file.path(dir.path, paste0(sprintf("%04d", i), ".roi"))
        saveRoiPoint(getPoint(points, i), 3, point.path)
      }
    }

    # Return the saver function
    function(points, zip.path){
      # Check zip.path is correct
      if (tools::file_ext(zip.path) != "zip")
        rlang::abort("zip.path must have zip extension.")
      if (!dir.exists(dirname(zip.path)))
        rlang::abort("zip.path parent folder does not exist.")

      # Save each to a temporary directory
      td <- tempdir(check = TRUE)
      #   Clear the temporary directory
      unlink(list.files(td, full.names = TRUE), recursive = TRUE)
      dirSaver(points, td)

      # Zip and save
      roi.files <- list.files(td, full.names = TRUE)
      utils::zip(zip.path, roi.files, flags = "-r9Xjq") # quiet, junk
    }
  }


# # Demo
# saver <- RoiPointsSaver("/Users/yeyuan/mambaforge/envs/binfo/bin/python")
#
# pts <- df.nnd$spotIn.x[[1]]
# saver(pts, "testROI.zip")
