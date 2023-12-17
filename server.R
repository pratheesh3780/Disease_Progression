################ server
server <- function(input, output, session) {
  mpsle_server(input, output, session)
  lpsle_server(input, output, session)
  lpsfe_server(input, output, session)
  lptfe_server(input, output, session)
}