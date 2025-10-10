#' Internal logging utility
#'
#' This function provides a simple internal logger that prints timestamped messages
#' to the console and appends them to a session log file.
#'
#' The log file name includes the time when the session started, ensuring that
#' each R session produces its own log file. Log messages are printed with
#' a timestamp, a log level (INFO, WARN, or ERROR), and a custom message.
#'
#' @param msg Character string. The message to log.
#' @param level Character string indicating log level. One of `"INFO"`, `"WARN"`, or `"ERROR"`.
#'
#' @details
#' This function is designed for internal use only and should not be directly
#' exposed to end-users. It can be safely called from within package functions
#' to output informative runtime logs or warnings.
#'
#' @return Invisibly returns `NULL`. Side effects: prints to console and writes to a log file.
#'
#' @examples
#' \dontrun{
#' log_message("Starting data import")
#' log_message("Missing values detected", "WARN")
#' log_message("File not found, skipping site", "ERROR")
#' }
#'
#' @keywords internal
#' @noRd
log_message <- (function() {
  log_file <- sprintf("log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S"))

  function(msg, level = c("INFO", "WARN", "ERROR")) {
    level <- toupper(match.arg(level))

    log_line <- sprintf("%s\t%s\t%s",
                        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                        level,
                        msg)

    # Print to console (optional: different style per level)
    switch(level,
           "INFO"  = message(log_line),
           "WARN"  = warning(log_line, call. = FALSE, immediate. = TRUE),
           "ERROR" = message(log_line))

    # Append to log file
    cat(log_line, "\n", file = log_file, append = TRUE)
  }
})()
