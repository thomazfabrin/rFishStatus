#' Mock Function
#'
#' This function takes an input, prints a message indicating the input received,
#' and returns a mock output.
#'
#' @param input A value of any type that will be processed by the function.
#'
#' @return A character string: "This is a mock output".
#'
#' @examples
#' rfs_mock_function("example input")
#' rfs_mock_function(42)
#'
#' @export
rfs_mock_function <- function(input) {
    print(paste("Input received:", input))

    return("This is a mock output")
}
