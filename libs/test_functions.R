library(testthat)
library(rlang)

# generic information message
msg <- function(q, ...) paste0(sprintf("Problem in question %s: ", q), ..., ".")

# expect specific numeric or text value
expect_value <- function(q, var, value, dec = 3) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, sprintf("'%s' object should exist but does not", var_name))
  )
  var <- eval_tidy(var_quo)
  if (is.numeric(value)) {
    var <- as.numeric(round(var, dec))
    value <- round(value, dec)
  }
  expect_equal(
    var, value,
    info = msg(q, sprintf("'%s' does not have the correct value", var_name))
  )
}

# expect columns to be present
expect_df_columns <- function(q, var, cols) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, sprintf("'%s' object should exist but does not", var_name))
  )
  var_cols <- names(eval_tidy(var_quo))
  expect_true(
    all(cols %in% var_cols),
    info = msg(q, sprintf("'%s' does not have all expected columns: %s", 
                          var_name, paste(cols, collapse=', ')))
  )
}

# expect a specific class
expect_class <- function(q, var, class) {
  var_quo <- enquo(var)
  var_name <- quo_text(var_quo)
  expect_true(
    exists(var_name),
    info = msg(q, sprintf("'%s' object should exist but does not", var_name))
  )
  var <- eval_tidy(var_quo)
  expect_is(
    var, class,
    info = msg(q, sprintf("'%s' does not have the expected class: %s", var_name, class))
  )
}