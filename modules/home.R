# Home Tab UI
homeTabUI <- function(id) {
  ns <- NS(id)  # Namespace for unique IDs
  tabPanel(
    "Home",
    fluidPage(
      h2(HTML("<b>Welcome to DecoNFlow Benchmarking</b>")),
      h3("Results"),
      h3("Datasets"),
      h3("Methods"),
      h3("Usability"),
      h3("Links"),
      h3("Citation")
    )
  )
}

# Home Tab Server
homeTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Add server-side logic for Home tab if needed (currently none)
  })
}
