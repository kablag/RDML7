# Structured RDML conditions ------------------------------------------------

.rdmlCondition <- function(
    message,
    class,
    code,
    ..., 
    call = NULL) {

  fields <- c(
    list(
      message = as.character(message),
      call = call,
      code = as.character(code)
    ),
    list(...)
  )

  structure(
    fields,
    class = unique(c(
      class,
      "rdmlCondition",
      "condition"
    ))
  )
}


.rdmlAbort <- function(
    code,
    message,
    ...,
    call = NULL) {

  condition <- .rdmlCondition(
    message = message,
    class = c(
      paste0("rdmlError_", code),
      "rdmlError",
      "error"
    ),
    code = code,
    ...,
    call = call
  )

  stop(condition)
}


.rdmlWarn <- function(
    code,
    message,
    ...,
    call = NULL) {

  condition <- .rdmlCondition(
    message = message,
    class = c(
      paste0("rdmlWarning_", code),
      "rdmlWarning",
      "warning"
    ),
    code = code,
    ...,
    call = call
  )

  warning(condition)
  invisible(condition)
}


#' Create a structured lossy-conversion record
#'
#' Importers/exporters use these records for information that cannot be
#' represented exactly in the destination model.
#'
#' @param code Machine-readable loss code.
#' @param message Human-readable description.
#' @param path Optional RDML object path.
#' @param details Additional structured metadata.
#' @return An `rdmlLossRecord`.
#' @seealso `readRDML`, `writeRDML`, `rdmlImportData`
#' @export
rdmlLossRecord <- function(
    code,
    message,
    path = NA_character_,
    details = list()) {

  checkmate::assertString(code)
  checkmate::assertString(message)

  if (!is.list(details)) {
    stop("`details` must be a list", call. = FALSE)
  }

  structure(
    list(
      code = code,
      message = message,
      path = path,
      details = details
    ),
    class = "rdmlLossRecord"
  )
}


.rdmlSignalLoss <- function(
    loss = c("warn", "error", "allow"),
    code,
    message,
    path = NA_character_,
    details = list()) {

  loss <- match.arg(loss)

  if (identical(loss, "allow")) {
    return(invisible(FALSE))
  }

  baseClass <- if (identical(loss, "error")) {
    c("rdmlLossError", "rdmlError", "error")
  } else {
    c("rdmlLossWarning", "rdmlWarning", "warning")
  }

  condition <- .rdmlCondition(
    message = message,
    class = c(
      paste0("rdmlLoss_", code),
      baseClass
    ),
    code = code,
    path = path,
    details = details,
    call = NULL
  )

  if (identical(loss, "error")) {
    stop(condition)
  }

  warning(condition)
  invisible(TRUE)
}


.rdmlSignalLosses <- function(
    losses,
    loss = c("warn", "error", "allow")) {

  loss <- match.arg(loss)

  if (!length(losses) || identical(loss, "allow")) {
    return(invisible(FALSE))
  }

  for (record in losses) {
    if (inherits(record, "rdmlLossRecord")) {
      .rdmlSignalLoss(
        loss = loss,
        code = record$code,
        message = record$message,
        path = record$path,
        details = record$details
      )
    } else if (is.list(record) && !is.null(record$code) && !is.null(record$message)) {
      .rdmlSignalLoss(
        loss = loss,
        code = record$code,
        message = record$message,
        path = if (is.null(record$path)) NA_character_ else record$path,
        details = if (is.null(record$details)) list() else record$details
      )
    } else {
      .rdmlSignalLoss(
        loss = loss,
        code = "unspecified",
        message = as.character(record)
      )
    }
  }

  invisible(TRUE)
}
