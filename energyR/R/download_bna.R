#' Download a data file from the Brave New Algorithm GitHub repository
#'
#' Fetches a file from the \code{data/} directory of the
#' \href{https://github.com/JJ/brave-new-green-algorithm}{Brave New Algorithm}
#' GitHub repository and caches it locally.  If the destination file already
#' exists it is returned immediately without a network request.
#'
#' This helper is intended for use in vignettes and reproducible reports that
#' need to access the experimental data without bundling it in the package.
#'
#' @param filename Character.  Name of the file to download (just the
#'   filename, not the full path).  Must be a file present under the
#'   \code{data/} subdirectory of the repository.
#' @param destdir Character.  Local directory in which to cache the
#'   downloaded file.  Defaults to \code{tempdir()}.
#' @param repo_url Character.  Base raw-content URL of the repository's
#'   \code{data/} folder.  Override only if using a fork or a different
#'   branch.  Default: the \code{main} branch of the canonical repo.
#'
#' @return The full local path to the cached file (character), or \code{NULL}
#'   (invisibly) if the download failed.
#'
#' @references
#' Merelo Guervos, Juan Julian and Merelo-Molina, Cecilia (2025). "Analyzing
#' how the exploration/exploitation trade off in biologically-inspired
#' algorithms affects energy consumption." University of Granada.
#' \url{https://hdl.handle.net/10481/107864}
#'
#' @examples
#' \dontrun{
#' dest <- download_bna("ola-base-ola-baseline-14-Dec-12-06-42.csv")
#' baseline <- load_bna_csv(dest)
#' }
#'
#' @export
download_bna <- function(
    filename,
    destdir  = tempdir(),
    repo_url = "https://raw.githubusercontent.com/JJ/brave-new-green-algorithm/main/data/") {
  dest <- file.path(destdir, filename)
  if (file.exists(dest)) return(dest)
  url <- paste0(repo_url, filename)
  success <- tryCatch({
    utils::download.file(url, destfile = dest, quiet = TRUE)
    TRUE
  }, error = function(e) {
    warning("Could not download '", filename, "': ", conditionMessage(e))
    FALSE
  })
  if (success) dest else invisible(NULL)
}
