#' Get Stylesheets Location
#'
#' Locates the directory containing QSS stylesheets within the package.
#' This function is not supported on Mac systems as QSS stylesheets are not used there.
#'
#' @return A character string with the path to the stylesheets directory
#' @export
#' @examples
#' \dontrun{
#' get_stylesheets_location()
#' }
get_stylesheets_location <- function() {
  # Check if running on Mac - QSS stylesheets are not used on Mac
  if (host_os_is_mac()) {
    stop("QSS Stylesheets are not supported on Mac OS", call. = FALSE)
  }

  # Get the current package name
  pkg_name <- "rscodeio"

  # If packageName() returns NULL, try to get it from the environment
  if (is.null(pkg_name)) {
    pkg_name <- environmentName(parent.env(environment()))
    if (is.null(pkg_name) || pkg_name == "") {
      stop("Could not determine package name. Please specify package explicitly.",
           call. = FALSE)
    }
  }

  # Locate the stylesheets directory within the package
  stylesheet_dir <- system.file("resources", "stylesheets", package = pkg_name)

  # Validate that the directory exists
  if (stylesheet_dir == "" || !dir.exists(stylesheet_dir)) {
    stop("Stylesheets directory not found in package '", pkg_name,
         "'. Please ensure the package is properly installed with the required resources.",
         call. = FALSE)
  }

  return(stylesheet_dir)
}

#' Get Gnome Dark Theme Path
#'
#' Returns the full path to the Gnome dark theme QSS file.
#'
#' @return A character string with the path to the Gnome dark theme file
#' @export
#' @examples
#' \dontrun{
#' gnome_theme_dark()
#' }
gnome_theme_dark <- function() {
  theme_path <- file.path(get_stylesheets_location(), "rstudio-gnome-dark.qss")

  # Validate that the file exists
  if (!file.exists(theme_path)) {
    warning("Gnome dark theme file not found at: ", theme_path, call. = FALSE)
  }

  return(theme_path)
}

#' Get Gnome Dark Theme Backup Path
#'
#' Returns the full path to the Gnome dark theme backup QSS file.
#'
#' @return A character string with the path to the Gnome dark theme backup file
#' @export
#' @examples
#' \dontrun{
#' gnome_theme_dark_backup()
#' }
gnome_theme_dark_backup <- function() {
  backup_path <- file.path(get_stylesheets_location(),
                           "rstudio-gnome-dark-rscodeio-backup.qss")

  # Validate that the file exists
  if (!file.exists(backup_path)) {
    warning("Gnome dark theme backup file not found at: ", backup_path, call. = FALSE)
  }

  return(backup_path)
}

#' Get Windows Dark Theme Path
#'
#' Returns the full path to the Windows dark theme QSS file.
#'
#' @return A character string with the path to the Windows dark theme file
#' @export
#' @examples
#' \dontrun{
#' windows_theme_dark()
#' }
windows_theme_dark <- function() {
  theme_path <- file.path(get_stylesheets_location(), "rstudio-windows-dark.qss")

  # Validate that the file exists
  if (!file.exists(theme_path)) {
    warning("Windows dark theme file not found at: ", theme_path, call. = FALSE)
  }

  return(theme_path)
}

#' Get Windows Dark Theme Backup Path
#'
#' Returns the full path to the Windows dark theme backup QSS file.
#'
#' @return A character string with the path to the Windows dark theme backup file
#' @export
#' @examples
#' \dontrun{
#' windows_theme_dark_backup()
#' }
windows_theme_dark_backup <- function() {
  backup_path <- file.path(get_stylesheets_location(),
                           "rstudio-windows-dark-rscodeio-backup.qss")

  # Validate that the file exists
  if (!file.exists(backup_path)) {
    warning("Windows dark theme backup file not found at: ", backup_path, call. = FALSE)
  }

  return(backup_path)
}

#' Check if Host OS is Mac
#'
#' Determines if the current operating system is macOS (Darwin).
#'
#' @return A logical value: TRUE if running on Mac, FALSE otherwise
#' @export
#' @examples
#' host_os_is_mac()
host_os_is_mac <- function() {
  Sys.info()[["sysname"]] == "Darwin"
}

#' Check if Running RStudio Server
#'
#' Determines if the current RStudio session is running in server mode.
#'
#' @return A logical value: TRUE if running RStudio Server, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' is_rstudio_server()
#' }
is_rstudio_server <- function() {
  # Check if rstudioapi is available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    warning("rstudioapi package not available. Cannot determine RStudio mode.",
            call. = FALSE)
    return(FALSE)
  }

  # Check if running in RStudio
  if (!rstudioapi::isAvailable()) {
    return(FALSE)
  }

  # Get version info and check mode
  tryCatch({
    version_info <- rstudioapi::versionInfo()
    return(version_info$mode == "server")
  }, error = function(e) {
    warning("Could not determine RStudio mode: ", e$message, call. = FALSE)
    return(FALSE)
  })
}

#' Check if RSCodeIO Theme is Installed
#'
#' Determines if the RSCodeIO theme or "Tomorrow Night Bright (rscodeio)" theme
#' is available in the current RStudio installation.
#'
#' @return A logical value: TRUE if RSCodeIO theme is installed, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' rscodeio_installed()
#' }
rscodeio_installed <- function() {
  # Check if rstudioapi is available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    warning("rstudioapi package not available. Cannot check for RSCodeIO theme.",
            call. = FALSE)
    return(FALSE)
  }

  # Check if running in RStudio
  if (!rstudioapi::isAvailable()) {
    return(FALSE)
  }

  # Get available themes and check for RSCodeIO variants
  tryCatch({
    themes <- rstudioapi::getThemes()
    theme_names <- names(themes)

    # Check for both possible theme names
    rscodeio_present <- "rscodeio" %in% theme_names
    tomorrow_night_present <- "tomorrow night bright (rscodeio)" %in% theme_names

    return(rscodeio_present || tomorrow_night_present)
  }, error = function(e) {
    warning("Could not retrieve RStudio themes: ", e$message, call. = FALSE)
    return(FALSE)
  })
}

#' Validate Stylesheet File
#'
#' Helper function to validate that a stylesheet file exists and is readable.
#'
#' @param file_path Character string with the path to validate
#' @param file_description Character string describing the file for error messages
#' @return Logical value indicating if the file is valid
#' @keywords internal
validate_stylesheet_file <- function(file_path, file_description = "Stylesheet file") {
  if (!file.exists(file_path)) {
    warning(file_description, " not found at: ", file_path, call. = FALSE)
    return(FALSE)
  }

  if (!file.access(file_path, mode = 4) == 0) {
    warning(file_description, " exists but is not readable: ", file_path, call. = FALSE)
    return(FALSE)
  }

  return(TRUE)
}

#' Get All Available Theme Paths
#'
#' Returns a named list with paths to all available theme files.
#'
#' @return A named list containing paths to theme files
#' @export
#' @examples
#' \dontrun{
#' get_all_theme_paths()
#' }
get_all_theme_paths <- function() {
  if (host_os_is_mac()) {
    message("Theme paths not applicable on Mac OS")
    return(list())
  }

  tryCatch({
    theme_paths <- list(
      gnome_dark = gnome_theme_dark(),
      gnome_dark_backup = gnome_theme_dark_backup(),
      windows_dark = windows_theme_dark(),
      windows_dark_backup = windows_theme_dark_backup()
    )

    # Filter out paths where files don't exist
    existing_paths <- Filter(function(x) file.exists(x), theme_paths)

    if (length(existing_paths) == 0) {
      warning("No theme files found in stylesheets directory", call. = FALSE)
    }

    return(existing_paths)
  }, error = function(e) {
    warning("Error getting theme paths: ", e$message, call. = FALSE)
    return(list())
  })
}
