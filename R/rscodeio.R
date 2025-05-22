#' Install the RSCodeIO Theme
#'
#' Installs the RSCodeIO theme for RStudio. Requires RStudio 1.2.0 or higher.
#' On Windows or Linux, you may need to run RStudio as Administrator to install
#' the menu theme files (only required for installation).
#'
#' @param menus Logical. If FALSE, do not install the RStudio menu theme QSS files.
#'   Default is TRUE.
#' @param force Logical. If TRUE, reinstall even if already installed. Default is FALSE.
#' @return Logical. TRUE if installation was successful, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Install with menu themes
#' install_theme()
#'
#' # Install without menu themes
#' install_theme(menus = FALSE)
#'
#' # Force reinstallation
#' install_theme(force = TRUE)
#' }
install_theme <- function(menus = TRUE, force = FALSE) {
  # Validate RStudio environment
  if (!validate_rstudio_environment()) {
    return(FALSE)
  }

  # Check if already installed and handle accordingly
  if (rscodeio_installed() && !force) {
    message("RSCodeIO theme is already installed. Use force = TRUE to reinstall.")
    return(TRUE)
  }

  # Uninstall existing theme if present
  if (rscodeio_installed()) {
    message("Uninstalling existing RSCodeIO theme...")
    if (!uninstall_theme()) {
      warning("Failed to uninstall existing theme. Installation may fail.", call. = FALSE)
    }
  }

  # Install theme files
  tryCatch({
    # Get theme file paths
    default_theme_path <- system.file("resources", "rscodeio.rstheme",
                                      package = utils::packageName())
    bright_theme_path <- system.file("resources", "rscodeio_tomorrow_night_bright.rstheme",
                                     package = utils::packageName())

    # Validate theme files exist
    if (!file.exists(default_theme_path)) {
      stop("Default theme file not found: ", default_theme_path, call. = FALSE)
    }
    if (!file.exists(bright_theme_path)) {
      stop("Bright theme file not found: ", bright_theme_path, call. = FALSE)
    }

    # Add themes to RStudio
    message("Installing RSCodeIO themes...")
    default_theme <- rstudioapi::addTheme(default_theme_path)
    rstudioapi::addTheme(bright_theme_path)

    # Install menu themes if requested
    if (menus) {
      if (!activate_menu_theme()) {
        warning("Theme installed but menu theme activation failed.", call. = FALSE)
      }
    }

    # Apply the default theme
    message("Applying RSCodeIO theme...")
    rstudioapi::applyTheme(default_theme)

    message("RSCodeIO theme installed successfully!")
    return(TRUE)

  }, error = function(e) {
    stop("Failed to install RSCodeIO theme: ", e$message, call. = FALSE)
  })
}

#' Uninstall the RSCodeIO Theme
#'
#' Removes all RSCodeIO themes from RStudio and deactivates menu themes.
#'
#' @param silent Logical. If TRUE, suppress messages. Default is FALSE.
#' @return Logical. TRUE if uninstallation was successful, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' uninstall_theme()
#' uninstall_theme(silent = TRUE)
#' }
uninstall_theme <- function(silent = FALSE) {
  if (!validate_rstudio_environment(silent = silent)) {
    return(FALSE)
  }

  # Check if theme is installed
  if (!rscodeio_installed()) {
    if (!silent) message("RSCodeIO theme is not currently installed.")
    return(TRUE)
  }

  tryCatch({
    # Deactivate menu theme first
    if (!silent) message("Deactivating menu themes...")
    deactivate_menu_theme(silent = silent)

    # Get all installed themes
    all_themes <- rstudioapi::getThemes()

    # Find RSCodeIO themes
    rscodeio_themes <- Filter(function(theme) {
      theme_name <- theme$name
      grepl("rscodeio", theme_name, ignore.case = TRUE) ||
        grepl("tomorrow night bright \\(rscodeio\\)", theme_name, ignore.case = TRUE)
    }, all_themes)

    # Remove each RSCodeIO theme
    if (length(rscodeio_themes) > 0) {
      if (!silent) message("Removing RSCodeIO themes...")
      for (theme in rscodeio_themes) {
        rstudioapi::removeTheme(theme$name)
        if (!silent) message("Removed theme: ", theme$name)
      }
    }

    if (!silent) message("RSCodeIO theme uninstalled successfully!")
    return(TRUE)

  }, error = function(e) {
    warning("Error during uninstallation: ", e$message, call. = FALSE)
    return(FALSE)
  })
}

#' Activate RSCodeIO Menu Theme
#'
#' Activates RSCodeIO styling in RStudio's file menus by replacing QSS files.
#' Not supported on Mac or RStudio Server.
#'
#' @param backup Logical. If TRUE, create backup of original files. Default is TRUE.
#' @param silent Logical. If TRUE, suppress messages. Default is FALSE.
#' @return Logical. TRUE if activation was successful, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' activate_menu_theme()
#' activate_menu_theme(backup = FALSE)
#' }
activate_menu_theme <- function(backup = TRUE, silent = FALSE) {
  # Check if menu themes are supported
  if (host_os_is_mac()) {
    if (!silent) message("Menu themes are not supported on Mac OS")
    return(FALSE)
  }

  if (is_rstudio_server()) {
    if (!silent) message("Menu themes are not supported on RStudio Server")
    return(FALSE)
  }

  # Check if already activated
  if (is_menu_theme_active()) {
    if (!silent) message("RSCodeIO menu theme is already activated")
    return(TRUE)
  }

  tryCatch({
    # Get stylesheet paths
    gnome_original <- gnome_theme_dark()
    gnome_backup <- gnome_theme_dark_backup()
    windows_original <- windows_theme_dark()
    windows_backup <- windows_theme_dark_backup()

    # Get RSCodeIO stylesheet paths
    pkg_name <- utils::packageName()
    gnome_rscodeio <- system.file("resources", "stylesheets", "rstudio-gnome-dark.qss",
                                  package = pkg_name)
    windows_rscodeio <- system.file("resources", "stylesheets", "rstudio-windows-dark.qss",
                                    package = pkg_name)

    # Validate RSCodeIO stylesheets exist
    if (!file.exists(gnome_rscodeio)) {
      stop("RSCodeIO Gnome stylesheet not found: ", gnome_rscodeio, call. = FALSE)
    }
    if (!file.exists(windows_rscodeio)) {
      stop("RSCodeIO Windows stylesheet not found: ", windows_rscodeio, call. = FALSE)
    }

    # Create backups if requested and originals exist
    if (backup) {
      if (file.exists(gnome_original)) {
        if (!file.copy(gnome_original, gnome_backup, overwrite = TRUE)) {
          stop("Failed to backup Gnome theme file", call. = FALSE)
        }
        if (!silent) message("Created backup: ", basename(gnome_backup))
      }

      if (file.exists(windows_original)) {
        if (!file.copy(windows_original, windows_backup, overwrite = TRUE)) {
          stop("Failed to backup Windows theme file", call. = FALSE)
        }
        if (!silent) message("Created backup: ", basename(windows_backup))
      }
    }

    # Copy RSCodeIO stylesheets to RStudio location
    success_gnome <- file.copy(gnome_rscodeio, gnome_original, overwrite = TRUE)
    success_windows <- file.copy(windows_rscodeio, windows_original, overwrite = TRUE)

    if (!success_gnome || !success_windows) {
      stop("Failed to copy RSCodeIO stylesheets. You may need administrator privileges.",
           call. = FALSE)
    }

    if (!silent) message("RSCodeIO menu theme activated successfully!")
    return(TRUE)

  }, error = function(e) {
    warning("Failed to activate menu theme: ", e$message, call. = FALSE)
    return(FALSE)
  })
}

#' Deactivate RSCodeIO Menu Theme
#'
#' Restores original RStudio menu styling by restoring backed-up QSS files.
#'
#' @param silent Logical. If TRUE, suppress messages. Default is FALSE.
#' @return Logical. TRUE if deactivation was successful, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' deactivate_menu_theme()
#' }
deactivate_menu_theme <- function(silent = FALSE) {
  # Check if menu themes are supported
  if (host_os_is_mac()) {
    if (!silent) message("Menu themes are not applicable on Mac OS")
    return(TRUE)
  }

  # Check if menu theme is currently active
  if (!is_menu_theme_active()) {
    if (!silent) message("RSCodeIO menu theme is not currently active")
    return(TRUE)
  }

  tryCatch({
    # Get file paths
    gnome_original <- gnome_theme_dark()
    gnome_backup <- gnome_theme_dark_backup()
    windows_original <- windows_theme_dark()
    windows_backup <- windows_theme_dark_backup()

    # Restore from backups
    success_gnome <- TRUE
    success_windows <- TRUE

    if (file.exists(gnome_backup)) {
      success_gnome <- file.copy(gnome_backup, gnome_original, overwrite = TRUE)
      if (success_gnome) {
        unlink(gnome_backup)
        if (!silent) message("Restored Gnome theme from backup")
      }
    }

    if (file.exists(windows_backup)) {
      success_windows <- file.copy(windows_backup, windows_original, overwrite = TRUE)
      if (success_windows) {
        unlink(windows_backup)
        if (!silent) message("Restored Windows theme from backup")
      }
    }

    if (!success_gnome || !success_windows) {
      warning("Some theme files could not be restored. You may need administrator privileges.",
              call. = FALSE)
      return(FALSE)
    }

    if (!silent) message("RSCodeIO menu theme deactivated successfully!")
    return(TRUE)

  }, error = function(e) {
    warning("Failed to deactivate menu theme: ", e$message, call. = FALSE)
    return(FALSE)
  })
}

#' Check if Menu Theme is Active
#'
#' Determines if the RSCodeIO menu theme is currently active by checking
#' for the presence of backup files.
#'
#' @return Logical. TRUE if menu theme is active, FALSE otherwise.
#' @export
#' @examples
#' \dontrun{
#' is_menu_theme_active()
#' }
is_menu_theme_active <- function() {
  if (host_os_is_mac()) {
    return(FALSE)
  }

  gnome_backup_exists <- file.exists(gnome_theme_dark_backup())
  windows_backup_exists <- file.exists(windows_theme_dark_backup())

  return(gnome_backup_exists || windows_backup_exists)
}

#' Validate RStudio Environment
#'
#' Helper function to validate that RStudio is available and supports themes.
#'
#' @param silent Logical. If TRUE, suppress messages. Default is FALSE.
#' @return Logical. TRUE if environment is valid, FALSE otherwise.
#' @keywords internal
validate_rstudio_environment <- function(silent = FALSE) {
  # Check if rstudioapi is available
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    if (!silent) stop("rstudioapi package is required but not available", call. = FALSE)
    return(FALSE)
  }

  # Check if running in RStudio
  if (!rstudioapi::isAvailable()) {
    if (!silent) stop("RSCodeIO must be installed from within RStudio", call. = FALSE)
    return(FALSE)
  }

  # Check RStudio version supports themes
  rstudio_version <- rstudioapi::versionInfo()$version
  if (utils::compareVersion(as.character(rstudio_version), "1.2.0") < 0) {
    if (!silent) stop("RStudio 1.2.0 or higher is required for theme support. Current version: ",
                      rstudio_version, call. = FALSE)
    return(FALSE)
  }

  return(TRUE)
}

#' Get Theme Installation Status
#'
#' Returns detailed information about the current RSCodeIO theme installation status.
#'
#' @return A named list with installation status information.
#' @export
#' @examples
#' \dontrun{
#' get_theme_status()
#' }
get_theme_status <- function() {
  status <- list(
    rstudio_available = FALSE,
    rstudio_version = NULL,
    themes_supported = FALSE,
    rscodeio_installed = FALSE,
    menu_theme_supported = FALSE,
    menu_theme_active = FALSE,
    current_theme = NULL
  )

  # Check RStudio availability
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    status$rstudio_available <- TRUE
    status$rstudio_version <- as.character(rstudioapi::versionInfo()$version)
    status$themes_supported <- utils::compareVersion(status$rstudio_version, "1.2.0") >= 0

    if (status$themes_supported) {
      status$rscodeio_installed <- rscodeio_installed()
      status$menu_theme_supported <- !host_os_is_mac() && !is_rstudio_server()
      status$menu_theme_active <- is_menu_theme_active()

      tryCatch({
        current_theme_info <- rstudioapi::getThemeInfo()
        status$current_theme <- current_theme_info$name
      }, error = function(e) {
        status$current_theme <- "Unknown"
      })
    }
  }

  return(status)
}

#' Print Theme Status
#'
#' Prints a human-readable summary of the RSCodeIO theme installation status.
#'
#' @return Nothing (called for side effects).
#' @export
#' @examples
#' \dontrun{
#' print_theme_status()
#' }
print_theme_status <- function() {
  status <- get_theme_status()

  cat("RSCodeIO Theme Status:\n")
  cat("=====================\n")
  cat("RStudio Available:", if (status$rstudio_available) "Yes" else "No", "\n")

  if (status$rstudio_available) {
    cat("RStudio Version:", status$rstudio_version, "\n")
    cat("Themes Supported:", if (status$themes_supported) "Yes" else "No", "\n")

    if (status$themes_supported) {
      cat("RSCodeIO Installed:", if (status$rscodeio_installed) "Yes" else "No", "\n")
      cat("Menu Theme Supported:", if (status$menu_theme_supported) "Yes" else "No", "\n")
      cat("Menu Theme Active:", if (status$menu_theme_active) "Yes" else "No", "\n")
      cat("Current Theme:", status$current_theme, "\n")
    }
  }
}
