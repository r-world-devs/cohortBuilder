#' Sample of library database
#'
#' A list containing four data frames reflecting library management database.
#'
#' @format A list of four data frames:
#'
#' \strong{books} - books on store
#' \describe{
#'   \item{\code{isbn}}{book ISBN number}
#'   \item{\code{title}}{book title}
#'   \item{\code{genre}}{comma separated book genre}
#'   \item{\code{publisher}}{name of book publisher}
#'   \item{\code{author}}{name of book author}
#'   \item{\code{copies}}{total number of book copies on store}
#' }
#' \strong{borrowers} - registered library members
#' \describe{
#'   \item{\code{id}}{member unique id}
#'   \item{\code{registered}}{date the member joined library}
#'   \item{\code{address}}{member address}
#'   \item{\code{name}}{full member name}
#'   \item{\code{phone_number}}{member phone number}
#'   \item{\code{program}}{membership program type (standard, premium or vip)}
#' }
#' \strong{issues} - borrowed books events
#' \describe{
#'   \item{\code{id}}{unique event id}
#'   \item{\code{borrower_id}}{id of the member that borrowed the book}
#'   \item{\code{isbn}}{is of the borrowed book}
#'   \item{\code{date}}{date of borrow event}
#' }
#' \strong{returns} - returned books events
#' \describe{
#'   \item{\code{id}}{event id equal to borrow issue id}
#'   \item{\code{date}}{date of return event}
#' }
"librarian"
