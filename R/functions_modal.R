guideline_modal <- function(x = "There is insufficient data to calculate the water quality guideline.
                            You could try to get a modelled estimate, change the term, or set the guideline manually.") {
  x <- gsub("Error :", "", x)
  modalDialog(
    title = "",
    footer = modalButton(label = "Got it"),
    tagList(
      p(x)
    )
  )
}

sitediff_modal <- function(sitediff) {
  modalDialog(
    title = "",
    footer = modalButton(label = "Got it"),
    tagList(
      p("There are no data available for the selected variable, date range and the following sites:"),
      p(cc(sitediff, conj = " and ", sep = ", ", ellipsis = 100L)),
      br(),
      help_text("This is not an error, just a note")
    )
  )
}

