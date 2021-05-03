#' Image Transparency
#'
#' This function allows you to set image transparency
#' for the geom_image addition to ggplot. This function has
#' an alpha value hard-coded in. To change the alpha value,
#' replace the decimal in 'expression = X.X*a'.
#'
#' @param img 
#' @return an image with adjusted transparency according to "0.8*a"
#' @examples
#' g + geom_image(aes(x=x, y=y, image=img_filename), image_fun=transparent, size=1, asp=1)
#' @export
transparent <- function(img) {
magick::image_fx(img, expression = "0.8*a", channel = "alpha")
}
