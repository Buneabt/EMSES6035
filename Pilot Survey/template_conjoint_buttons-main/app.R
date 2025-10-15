# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak(c(
#   'surveydown-dev/surveydown', # Development version from GitHub
#   'here',
#   'glue',
#   'readr',
#   'dplyr'
# ))

# Load packages
library(surveydown)
library(here)
library(dplyr)
library(glue)
library(readr)

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(ignore = TRUE)

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)
  sd_store_value(completion_code)

  # Read in the full survey design file
  design <- read_csv(here("data", "choice_questions1.csv"))

  # Sample a random respondentID and store it directly as "respID"
  respondentID <- sample(design$respID, 1)
  sd_store_value(respondentID, "respID")

  # Filter for the rows for the chosen respondentID
  df <- design |>
    filter(respID == respondentID)

  # Function to create the question labels based on design values
  make_cbc_options <- function(df) {
    alt1 <- df |> filter(altID == 1)
    alt2 <- df |> filter(altID == 2)
    alt3 <- df |> filter(altID == 3)
    alt4 <- df |> filter(altID == 3)
    alt5 <- df |> filter(altID == 3)
    alt6 <- df |> filter(altID == 3)

    options <- c("option_1", "option_2", "option_3")

    #type = c("Implantable", "Ring", "Bracelet", "Card"),
    #price = c(25, 50, 100),
    #compatability = c("iOS", "Android", "Both"),
    #capacity = c(1,3,5), #No of cards
    #range = c(0.1,0.5,1) #feet

    names(options) <- c(
      glue("
        **Option 1**<br>
        **Type**:  {alt1$type}<br>
        **Price**: $ {alt1$price} $ <br>
        **Compatability**:  {alt1$compatability}<br>
        **Capcacity**: {alt1$capacity} tag(s)<br>
        **Range**: {alt1$range} ft<br>
      "),
      glue("
        **Option 2**<br>
        **Type**:  {alt2$type}<br>
        **Price**: ${alt2$price} <br>
        **Compatability**:  {alt2$compatability}<br>
        **Capcacity**: {alt2$capacity} tag(s) <br>
        **Range**: {alt2$range} ft<br>
      "),
      glue("
        **Option 3**<br>
        **Type**:  {alt3$type}<br>
        **Price**: $ {alt3$price} <br>
        **Compatability**:  {alt3$compatability}<br>
        **Capcacity**: {alt3$capacity} tag(s) <br>
        **Range**: {alt3$range} ft<br>
      ")
    )
    return(options)
  }

  # Create the options for each choice question
  cbc1_options <- make_cbc_options(df |> filter(qID == 1))
  cbc2_options <- make_cbc_options(df |> filter(qID == 2))
  cbc3_options <- make_cbc_options(df |> filter(qID == 3))
  cbc4_options <- make_cbc_options(df |> filter(qID == 4))
  cbc5_options <- make_cbc_options(df |> filter(qID == 5))
  cbc6_options <- make_cbc_options(df |> filter(qID == 6))

  # Create each choice question - display these in your survey using sd_output()
  # Example: sd_output('cbc_q1', type = 'question')

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q1',
    label = "(1 of 6) If these were your only options, which would you choose?",
    option = cbc1_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q2',
    label = "(2 of 6) If these were your only options, which would you choose?",
    option = cbc2_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q3',
    label = "(3 of 6) If these were your only options, which would you choose?",
    option = cbc3_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q4',
    label = "(4 of 6) If these were your only options, which would you choose?",
    option = cbc4_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q5',
    label = "(5 of 6) If these were your only options, which would you choose?",
    option = cbc5_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q6',
    label = "(6 of 6) If these were your only options, which would you choose?",
    option = cbc6_options
  )

  # Define conditional skip logic (skip to page if a condition is true)
  sd_skip_if(
    input$screenout == "blue" ~ "end_screenout",
    input$consent_age == "no" ~ "end_consent",
    input$consent_understand == "no" ~ "end_consent"
  )


  # Run surveydown server and define database
  sd_server(db = db,
            all_questions_required = TRUE)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
