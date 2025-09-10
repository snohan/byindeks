# Show API error message if any
if (!is.null(response$errors)) {
  base::print(
    paste0(
      "GraphQL errors encountered: ",
      jsonlite::toJSON(response$errors$message)
    )
  )
}