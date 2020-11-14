#' Example DataVolley files provided as part of the volleysim package
#'
#' @references The example data files came from \url{https://www.volleynet.at/dvdownload/information/f-Damen/}
#' @param choice numeric: which data file to return?
#' \itemize{
#'   \item{1 - the 2020 Austrian Women's Volley Cup played between Hartberg and UVC Graz}
#' }
#' @return path to the file
#'
#' @seealso \code{\link[datavolley]{dv_read}}
#'
#' @examples
#' \dontrun{
#'   myfile <- vs_example_file()
#'   x <- datavolley::dv_read(myfile)
#'   summary(x)
#' }
#'
#' @export
vs_example_file <- function(choice = 1) {
    assert_that(is.numeric(choice))
    switch(as.character(choice),
           "1" = system.file("extdata/demo/&DCup-7.dvw", package = "volleysim"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}
