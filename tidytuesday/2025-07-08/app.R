library(shiny)
library(bslib)
library(tidytuesdayR)
library(grDevices)
library(tidyverse)
library(plotly)
library(colorspace)

# Load and prepare data
tuesdata <- tidytuesdayR::tt_load("2025-07-08")
answers0 <- tuesdata$answers |>
  mutate(hex = tolower(hex))
color_ranks <- tuesdata$color_ranks |>
  mutate(hex = tolower(hex)) |>
  rename(most_common_hex = hex)
users0 <- tuesdata$user
users <- users0 |>
  filter(
    colorblind == 0,
    spam_prob < 1
  )
answers <- answers0 |>
  filter(user_id %in% unique(users$user_id))

# Join data and add RGB values
d <- left_join(answers, color_ranks)
df_rgb <- d |>
  mutate(
    r = col2rgb(hex)[1, ],
    g = col2rgb(hex)[2, ],
    b = col2rgb(hex)[3, ]
  ) |>
  mutate(color_paste = paste0("rgb(", r, ", ", g, ", ", b, ")"))

# Convert to HSV
rgb_colors <- sRGB(df_rgb$r / 255, df_rgb$g / 255, df_rgb$b / 255)
hsv_colors <- as(rgb_colors, "HSV")

hsv_vals <- coords(hsv_colors)
df_rgb <- df_rgb |>
  mutate(
    hue = hsv_vals[, 1],
    saturation = hsv_vals[, 2],
    value = hsv_vals[, 3]
  )

# Get unique colors for selection
colors <- unique(d$color)

ui <- page_sidebar(
  title = "xkcd color space explorer",
  # HTML('<p>interact with the results of the <a href="https://blog.xkcd.com/2010/05/03/color-survey-results/" target="_blank">2010 xkcd color survey</a> in 3d space. select and deselect color names to see just how much variability there really is in color classification.</p>'),
  HTML('<p>what would you call <span style="color: #454af3;">this color</span>? the <a href="https://blog.xkcd.com/2010/05/03/color-survey-results/" target="_blank">2010 xkcd color survey</a> asked 200,000+ people the same question—and got wildly different answers. explore their responses in 3d space and see just how variable color perception really is.</p>'),
  theme = bs_theme(brand = "brand.yml"),
  sidebar = sidebar(
    width = 300,
    card(
      card_header("data controls"),
      checkboxGroupInput("selected_colors",
        "what users most often called:",
        choices = colors,
        selected = colors
      ),
      div(
        style = "margin-bottom: 15px;",
        actionButton("check_all", "Check All", class = "btn-outline-primary btn-sm", style = "margin-right: 5px;"),
        actionButton("uncheck_all", "Uncheck All", class = "btn-outline-secondary btn-sm")
      ),
      sliderInput("opacity",
        "point opacity:",
        min = 0.1,
        max = 1,
        value = 0.7,
        step = 0.1
      ),
      sliderInput("point_size",
        "point size:",
        min = 1,
        max = 10,
        value = 3,
        step = 1
      ),
      numericInput("sample_size",
        "# random samples from the data:",
        value = 5000,
        min = 1000,
        max = min(50000, nrow(df_rgb)),
        step = 1000
      ),
      actionButton("resample", "resample data", class = "btn-primary"),
      div(
        style = "text-align: center; margin-top: 20px;",
        a(
          href = "https://github.com/yourusername/your-repo-name",
          target = "_blank",
          class = "btn btn-outline-dark btn-sm",
          icon("github", lib = "font-awesome"),
          " source code"
        )
      ),
      HTML('<p>dataset info @ <a href="https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-07-08", target="_blank">tidytuesday</a>. only responses from non-colorblind users are used in this app.</p>')
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("red, green, blue color space"),
      plotlyOutput("color_plot", height = "700px")
    ),
    card(
      full_screen = TRUE,
      card_header("hue, saturation, value color sapce"),
      plotlyOutput("hsv_plot", height = "700px")
    )
  ),
  tags$footer(
    style = "
    bottom: 0;
    width: 100%;
    padding: 20px;
    background-color: #f8f9fa;
    text-align: center;
    font-size: 0.75em;
    color: #555;
    border-top: 1px solid #ddd;
    font-family: 'Rubik', sans-serif;
  ",
    HTML("made with love, curiosity, and confusion using <strong>shiny</strong> by jess graves — july 15, 2025"),
    HTML('<p><a href="https://bsky.app/profile/jessgraves.bsky.social", target="_blank">@jessgraves.bsky.social</a> & <a href="https://jesslgraves.github.io" target="_blank">jesslgraves.github.io</a></p>'),
  )
)

server <- function(input, output, session) {
  # Reactive data sampling
  sampled_data <- reactiveVal()

  # Initialize with sample data
  observe({
    set.seed(123)
    initial_sample <- df_rgb[sample(nrow(df_rgb), min(5000, nrow(df_rgb))), ]
    sampled_data(initial_sample)
  })

  # Update sample when button is clicked or sample size changes
  observeEvent(c(input$resample, input$sample_size), {
    set.seed(sample(1:1000, 1)) # Random seed for resampling
    new_sample <- df_rgb[sample(nrow(df_rgb), min(input$sample_size, nrow(df_rgb))), ]
    sampled_data(new_sample)
  })

  # Check all colors
  observeEvent(input$check_all, {
    updateCheckboxGroupInput(session, "selected_colors", selected = colors)
  })

  # Uncheck all colors
  observeEvent(input$uncheck_all, {
    updateCheckboxGroupInput(session, "selected_colors", selected = character(0))
  })

  # Filtered data based on selected colors
  filtered_data <- reactive({
    req(sampled_data())

    # Handle case where no colors are selected
    if (is.null(input$selected_colors) || length(input$selected_colors) == 0) {
      return(data.frame())
    }

    sampled_data() |>
      filter(color %in% input$selected_colors)
  })

  # Create the 3D plot
  output$color_plot <- renderPlotly({
    req(sampled_data())

    data <- filtered_data()

    if (nrow(data) == 0) {
      return(plotly_empty() %>%
        layout(title = "No data available - please select colors"))
    }

    p <- plot_ly(data,
      x = ~r,
      y = ~g,
      z = ~b,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = input$point_size,
        opacity = input$opacity,
        color = data$hex
      ),
      text = ~ paste(
        "user:", user_id, "<br>",
        "color:", color, "<br>",
        "hex:", hex, "<br>",
        "rGB:", r, ",", g, ",", b, "<br>",
        "rank:", rank
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "red (r)", range = c(0, 255)),
          yaxis = list(title = "green (g)", range = c(0, 255)),
          zaxis = list(title = "blue (b)", range = c(0, 255)),
          camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5)),
          aspectmode = "cube" # or "manual" for more control
        ),
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
        # Remove any title padding
        title = list(pad = list(t = 0, b = 0)),
        # Make plot fill container
        autosize = TRUE
      )

    p
  })

  # Create the 3D plot
  output$hsv_plot <- renderPlotly({
    req(sampled_data())

    data <- filtered_data()

    if (nrow(data) == 0) {
      return(plotly_empty() %>%
        layout(title = "no data available - please select colors"))
    }

    p2 <- plot_ly(data,
      x = ~hue,
      y = ~saturation,
      z = ~value,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = input$point_size,
        opacity = input$opacity,
        color = data$hex
      ),
      text = ~ paste(
        "user:", user_id, "<br>",
        "color:", color, "<br>",
        "hex:", hex, "<br>",
        "hsv:", hue, ",", saturation, ",", value, "<br>",
        "rank:", rank
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "hue (h)", range = c(0, 365)),
          yaxis = list(title = "saturation (s)", range = c(0, 1)),
          zaxis = list(title = "value (v)", range = c(0, 1)),
          camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5)),
          aspectmode = "cube" # or "manual" for more control
        ),
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
        # Remove any title padding
        title = list(pad = list(t = 0, b = 0)),
        # Make plot fill container
        autosize = TRUE
      )

    p2
  })
}

shinyApp(ui = ui, server = server)
