# MIT License
#
# Copyright (c) Stefano M. Iacus
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(shiny)
library(data.table)
library(arrow)
library(sf)
library(leaflet)
library(viridis)
library(bslib)
library(htmlwidgets)
library(shinyWidgets)
library(shinyjs)
library(webshot)
library(ggplot2)
library(zoo)
library(scales)
library(plotly)
library(lubridate)

# Load data
state_data <- as.data.table(read_parquet("https://huggingface.co/datasets/siacus/flourishing/resolve/main/flourishingStateYear.parquet"))
state_data <- state_data[year >= 2013]
county_data <- as.data.table(read_parquet("https://huggingface.co/datasets/siacus/flourishing/resolve/main/flourishingCountyYear.parquet"))
county_data <- county_data[year >= 2013]
all_vars <- unique(state_data$variable)
all_vars <- all_vars[!grepl("^q", all_vars)]
all_years <- sort(unique(state_data$year))

monthly_state_data <- as.data.table(read_parquet("https://huggingface.co/datasets/siacus/flourishing/resolve/main/flourishingStateMonth.parquet"))
monthly_state_data <- monthly_state_data[year >= 2013]
monthly_state_data[, date := ceiling_date(ymd(paste(year, month, "01", sep = "-")), "month") - days(1)]


plot_variable_timeseries_plotly <- function(dt, varname, presidents = FALSE, covid = FALSE) {
  single_var <- dt[variable == varname]
  single_var[, date := as.Date(date)]
  
  # Fill missing dates
  full_dates <- data.table(date = seq(min(single_var$date), max(single_var$date), by = "month"))
  interpolated <- merge(full_dates, single_var, by = "date", all.x = TRUE)
  interpolated[, variable := varname]
  
  # Interpolation
  interpolated[, salience := na.approx(salience, date, na.rm = FALSE)]
  interpolated[, total_ntweets := na.approx(ntweets, date, na.rm = FALSE)]
  interpolated[, ntweets := na.approx(ntweets, date, na.rm = FALSE)]
  interpolated[, stat := na.approx(stat, date, na.rm = FALSE)]
  
  # Reshape for plotting
  plot_data <- melt(
    interpolated,
    id.vars = "date",
    measure.vars = c("stat", "salience"),
    variable.name = "measure",
    value.name = "value"
  )
  
  min_date <- min(plot_data$date, na.rm = TRUE)
  max_date <- max(plot_data$date, na.rm = TRUE)
  label_y <- plot_data[, .(y = max(value, na.rm = TRUE)), by = measure]
  label_y <- label_y[!is.infinite(y)]  # protect against empty or all NA
  
  p <- ggplot(plot_data, aes(x = date, y = value)) +
    geom_line(color = "steelblue", size = 1) +
    facet_wrap(~ measure, scales = "free_y", ncol = 1) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste("Monthly Trends for", shQuote(varname), "(Interpolated)"),
      x = "Year",
      y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # --- Add presidential annotations
  if (presidents) {
    pres_labels <- data.table(
      label = c("Obama", "Trump", "Biden"),
      start = as.Date(c("2009-01-20", "2017-01-20", "2021-01-20")),
      end   = as.Date(c("2017-01-19", "2021-01-19", "2025-01-20"))
    )
    pres_labels <- pres_labels[start <= max_date & end >= min_date]
    pres_labels[, start := pmax(start, min_date)]
    pres_labels[, end := pmin(end, max_date)]
    pres_labels[, mid := start + (end - start) / 2]
    
    if (nrow(pres_labels) > 0 && nrow(label_y) > 0) {
      pres_text <- pres_labels[, .(mid, label)][
        , .SD[rep(1:.N, each = nrow(label_y))]][
          , measure := rep(label_y$measure, times = nrow(pres_labels))][
            , y := rep(label_y$y, times = nrow(pres_labels))]
      p <- p +
        geom_vline(xintercept = as.numeric(pres_labels$start), linetype = "dashed", color = "red") +
        geom_text(
          data = pres_text,
          aes(x = mid, y = y * 0.95, label = label),
          inherit.aes = FALSE,
          size = 3,
          color = "red"
        )
    }
  }
  
  # --- Add COVID annotation
  if (covid) {
    covid_start <- as.Date("2020-03-01")
    covid_end <- as.Date("2022-02-28")  # You can adjust this if needed
    
    if (covid_start <= max_date && covid_end >= min_date) {
      covid_start <- pmax(covid_start, min_date)
      covid_end <- pmin(covid_end, max_date)
      
      mid1 <- min_date + (covid_start - min_date) / 2
      mid2 <- covid_start + (covid_end - covid_start) / 2
      mid3 <- covid_end + (max_date - covid_end) / 2
      
      covid_text <- data.table(
        label = c("Pre-COVID-19", "COVID-19", "Post-COVID-19"),
        x = c(mid1, mid2, mid3)
      )
    }
    
      if (nrow(label_y) > 0) {
        # Expand covid_text over all measures
        covid_text <- covid_text[!is.na(label)]
        
        covid_text <- covid_text[
          , .SD[rep(1:.N, each = nrow(label_y))]][
            , measure := rep(label_y$measure, times = nrow(covid_text))][
              , y := rep(label_y$y, times = nrow(covid_text))]
        p <- p +
          geom_vline(xintercept = as.numeric(covid_start), linetype = "dotted", color = "blue") +
          geom_vline(xintercept = as.numeric(covid_end), linetype = "dotted", color = "blue")
        
        p <- p + geom_text(
            data = covid_text,
            aes(x = x, y = y * 0.85, label = label),
            inherit.aes = FALSE,
            size = 3,
            color = "blue"
          )
      }
    }
  
  
  ggplotly(p)
}



plot_variable_timeseries <- function(dt, varname) {
  # Step 1: Filter data
  single_var <- dt[variable == varname]
  single_var[, date := as.Date(date)]
  
  # Step 2: Create full sequence of monthly dates
  full_dates <- data.table(date = seq(min(single_var$date), max(single_var$date), by = "month"))
  interpolated <- merge(full_dates, single_var, by = "date", all.x = TRUE)
  interpolated[, variable := varname]
  
  # Step 3: Interpolate missing numeric values
  interpolated[, weighted_salience := na.approx(weighted_salience, date, na.rm = FALSE)]
  interpolated[, total_ntweets := na.approx(total_ntweets, date, na.rm = FALSE)]
  interpolated[, ntweets := na.approx(ntweets, date, na.rm = FALSE)]
  interpolated[, stat := na.approx(stat, date, na.rm = FALSE)]
  
  # Step 4: Reshape to long format for plotting
  plot_data <- melt(
    interpolated,
    id.vars = "date",
    measure.vars = c("stat","ntweets","weighted_salience", "total_ntweets"),
    variable.name = "measure",
    value.name = "value"
  )
  
  # Step 5: Plot
  ggplot(plot_data, aes(x = date, y = value)) +
    geom_line(color = "steelblue", size = 1) +
    facet_wrap(~ measure, scales = "free_y", ncol = 1) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste("Monthly Trends for", shQuote(varname), "(Interpolated)"),
      x = "Year",
      y = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


var_info <- data.table(
  variable = c("migmood", "corruption", 
               "happiness", "resilience", "selfesteem", "lifesat", "fearfuture", "vitality",
               "energy", "posfunct", "jobsat", "optimism", "innerpeace", "purpose",
               "depression", "anxiety", "suffering", "pain", "altruism", "loneliness",
               "relationships", "belonging", "gratitude", "trust", "trusted", "balance",
               "mastery", "discrim", "lovedgod", "believegod", "relcrit", "spiritpun",
               "relcomfort", "finworry", "afterlife", "volunteer", "charity", "forgive",
               "polvoice", "govapprove", "hope", "goodpromo", "delaygrat", "ptsd",
               "smokehealth", "drinkhealth", "healthlim", "empathy"),
  
  label = c(
    "Mood Towards Migration or Migrants", "Perception of Corruption",
    "Happiness",  "Resilience",  "Self-esteem",  "Life satisfaction",
    "Fear of Future",  "Vitality",  "Having Energy",  "Positive Functioning",
    "Expressing Job Satisfaction",  "Expressing Optimism",  "Peace With Thoughts and Feelings",
    "Purpose in Life",  "Depression",  "Anxiety",  "Suffering",  "Feeling Pain",
    "Expressing Altruism",  "Loneliness",  "Quality of Relationships",  "Belonging to Society",
    "Expressing Gratitude",  "Expressing Trust",  "Feeling Trusted",
    "Balance in The Various Aspects of Own Life",
    "Mastery (Ability or Capability)",  "Perceiving Discrimination",  "Feeling Loved by God",
    "Belief in God",  "Religious criticism", "Spiritual Punishment",
    "Feeling Religious Comfort",  "Financial or Material Worry",  "Life After Death Belief",
    "Volunteering",  "Charitable Giving or Helping", "Seeking for Forgiveness",
    "Feeling Having a Political Voice",  "Expressing Government Approval",
    "Having Hope",  "Promoting Good","Expressing Delayed Gratification",
    "PSTD (Post-Traumatic Stress Disorder)", "Describing Smoking Related Health Issues",
    "Describing Drinking Related Health Issues", "Describing Health Limitations",
    "Expressing Empathy"),
  
  description = c(
    "General sentiment towards migration and/or migrants.<p>Scale: 1 = positive, -1 = negative.",
    "Corruption is mentioned in the discourse.<p>Scale: 0 = no mention, 1 = many mentions.",
    "Expressions of some level of happiness or sadness.<p>Scale: 1 = high happiness, -1 = sadness.",
    "Expressing capability of withstanding or recovering from difficulties.<p>Salce:  1 = capable, -1 = unable.",
    "Expressing some level of confidence in one's worth or abilities.<p>Scale: 1 = high confidence, -1 low confidence.",
    "Expressing satisfaction with one's life as a whole.<p>Scale: 1 = high satisfaction, -1 low satisfaction.",
    "Expressing worry about one's condition in the next years.<p>Scale: 1 = worrines, -1 = serenity.",
    "Expressing feelings of strength and activity.<p>Scale: 1 = strenght, -1 = weakness.",
    "Expressing that one feels full of energy.<p>Scale: 1 = full, -1 exhausted.",
    "Expressing the fact that one feels capable to do many things.<p>Scale: 1 = very capable, -1 = uncapable.",
    "Expressing satisfaction with one's present job, all thing considered.<p>Scale: 1 = very satisfied, -1 = unsatisfied.",
    "Expressing optimism about one's condition or in general in the medium-run future.<p>Scale: 1 = optimism, -1 = skepticisms.",
    "Expressing a general feeling of peace with one's thoughts and feelings.<p>Scale: 1 peacefulness, -1 = distress.",
    "Expressing understanding of one's purpose in life. In other terms, it expresses the feeling that the things one is doing in his/her life are worthwhile.<p>Scale: 1 = worthwhile, -1 = worthless.",
    "Expressing that one is bothered by the following problems: Little interest or pleasure in doing things; Feeling down, depressed or hopeless.<p>Scale: 1 = depressed, -1 = emotionally well.",
    "Expressing that one is bothered by the following problems: Feeling nervous, anxious or on edge; Not being able to stop or control worrying.<p>Scale: 1 = anxious, -1 = serene.",
    "Expressing the experience of any type of physical or mental suffering.<p>Scale: 1 = suffering, -1 = pain-free.",
    "Expressing the experience of bodily pain currently or in the recent past.<p>Scale: 1 = feel pain, -1 = no discomfort.",
    "Expressing willingness to do things that bring advantages to others, even if it results in disadvantage for him/herself.<p>Scale: 1 = altruist, -1 = selfish.",
    "Expressing feelings of loneliness.<p>Scale: 1 = loneliness, -1 = connectedness.",
    "Expressing satisfaction about one's relationships.<p>Scale: 1 = satisfactory, -1 = unsatisfactory.",
    "Expressing a sense of belonging in one's community.<p>Scale: 1 = belonging, -1 = disconnection.",
    "Expressing one's feelings of gratitude for many reasons.<p>Scale: 1 = gratitude, -1 = ingratitude.",
    "Expressing feeling of trust towards people in one's community?<p>Scale: 1 = trust, -1 = distrust.",
    "Expressing that people in one's community trust one another.<p>Scale: 1 = feeling trusted, -1 = feeling mistrusted.",
    "Indication that the various aspects of one's life are, in general, well balanced.<p>Scale: 1 = balance, -1 = unbalance.",
    "Expression of one's feeling of being very capable in most things one does in life.<p>Scale: 1 = self-efficacy, -1 = helplessness.",
    "Expressing the feeling of being discriminated against because of one's belonging to any group.<p>Scale: 1 = feeling discriminated, -1 = feeling accepted.",
    "Expressing one's feeling of being loved or cared for by God, the main god worshipped, or the spiritual force that guides one's life.<p>Scale: 1 = spiritually loved, -1 = spiritually abandoned.",
    "Expressing believe in one God, or more than one god, or an impersonal spiritual force.<p>Scale: Scale: 1 = strong belief, -1 = no belief.",
    "Expressing that people in one's religious community are critical of one's person or one's lifestyle.<p>Scale:  1 = feels judged, -1 = feels accepted.",
    "Expressing the feeling of God, a god, or a spiritual force as a punishing entity.<p>Scale: 1 = sees God/spiritual force as punishing, -1 = sees God/spiritual force as loving.",
    "Expressions of finding strength or comfort in one's religion or spirituality.<p>Scale: 1 = finds strength in religion/spirituality, -1 = finds no strength in religion/spirituality.",
    "Expressing one's worry about being able to meet normal monthly living expenses.<p>Scale: 1 = financially worried, -1 = financially secure.",
    "Expressing one's belief in life after death.<p>Scale: 1 = strong belief in life after death, -1 = no belief in life after death.",
    "Expressing one's habit of volunteering one's time to an organization.<p>Scale: 1 = engaged in volunteering, -1 = not engaged in volunteering.",
    "Expressing one's habit of donating money to a charity.<p>Scale:1 = frequently donates, -1 = never donates.",
    "Expressing propensity to forgive those who have hurt us.<p>Scale: 1 = highly forgiving, -1 = unwilling to forgive.",
    "Expressing the feeling of having a say about what the government does.<p>Scale: 1 = feeling heard by government, -1 = feeling ignored by government.",
    "Expressing approval of the job performance of the national government of one's country.<p>Scale: 1 = strong approval, -1 = strong disapproval.",
    "Expressing feelings of hope about the future, despite challenges.<p>Scale: 1 = hopeful, -1 = hopeless.",
    "Showing the propensity of acting to promote good in all circumstances, even in difficult and challenging situations.<p>Scale: 1 = consistently acts for good (or morally resistent), -1 = rarely acts for good (or morally disengaged.",
    "Expressing the ability to give up some happiness now for greater happiness later.<p>Scale: 1 = expresses delayed gratification, -1 = seeks immediate gratification.",
    "PTSD (Post-traumatic stress disorder) Expressing the tendency to be frequently bothered by the big threats to life one has witnessed or personally experienced during one's life.<p>Scale: 1 = expresses PTSD symptoms, -1 = no signs of distress from past trauma.",
    "Describing smoking related health issues or expressing the habit of smoking many cigarettes every day.<p>Scale: 1 = smokes heavily every day, -1 = does not smoke.",
    "Describing drinking related health issues or expressing the habit of frequently drinking full drinks of any kind of alcoholic beverage.<p>Scale: 1 = frequently drinks alcohol (high health risk), -1 = does not drink alcohol (no health risk).",
    "Describing any health problems that prevent one from doing any of the things people that age normally can do.<p>Scale: 1 = severely limited by health problems, -1 = no health-related limitations.",
    "Expressing ability to share other people's feelings or experiences by imagining what it would be like to be in their own situation.<p>Scale: 1 = highly empathetic, -1 = lacks empathy"
    )
)

library(data.table)

# Step 1: Log transform based on variable
state_data[, stat_log := ifelse(variable == "corruption", log(stat + 1), log(stat + 2))]
county_data[, stat_log := ifelse(variable == "corruption", log(stat + 1), log(stat + 2))]

# Step 2: Rescale per variable and year
state_data[, stat_rescaled := {
  if (unique(variable) == "corruption") {
    min_val <- min(stat_log, na.rm = TRUE)
    max_val <- max(stat_log, na.rm = TRUE)
    if (max_val == min_val) {
      rep(0, .N)
    } else {
      (stat_log - min_val) / (max_val - min_val)
    }
  } else {
    centered <- stat_log - log(2)
    min_val <- min(centered, na.rm = TRUE)
    max_val <- max(centered, na.rm = TRUE)
    if (max_val == min_val) {
      rep(0, .N)
    } else {
      2 * ((centered - min_val) / (max_val - min_val)) - 1
    }
  }
}, by = .(variable, year)]


county_data[, stat_rescaled := {
  if (unique(variable) == "corruption") {
    min_val <- min(stat_log, na.rm = TRUE)
    max_val <- max(stat_log, na.rm = TRUE)
    if (max_val == min_val) {
      rep(0, .N)
    } else {
      (stat_log - min_val) / (max_val - min_val)
    }
  } else {
    centered <- stat_log - log(2)
    min_val <- min(centered, na.rm = TRUE)
    max_val <- max(centered, na.rm = TRUE)
    if (max_val == min_val) {
      rep(0, .N)
    } else {
      2 * ((centered - min_val) / (max_val - min_val)) - 1
    }
  }
}, by = .(variable, year)]

# Not sure about the scale problem. I keep this code here for future reference

# # Step 1: Compute mean(stat) by FIPS, variable, year
# state_means <- state_data[, .(stat_mean = mean(stat, na.rm = TRUE)), by = .(FIPS, variable, year)]
# county_means <- county_data[, .(stat_mean = mean(stat, na.rm = TRUE)), by = .(StateCounty, variable, year)]
# 
# # Step 2: Conditional log transform
# state_means[, stat_log := ifelse(variable == "corruption", log(stat_mean + 1), log(stat_mean + 2))]
# county_means[, stat_log := ifelse(variable == "corruption", log(stat_mean + 1), log(stat_mean + 2))]
# 
# state_means[, stat_rescaled := {
#   if (unique(variable) == "corruption") {
#     min_val <- min(stat_log, na.rm = TRUE)
#     max_val <- max(stat_log, na.rm = TRUE)
#     if (max_val == min_val) {
#       rep(0, .N)
#     } else {
#       (stat_log - min_val) / (max_val - min_val)
#     }
#   } else {
#     centered <- stat_log - log(2)
#     min_val <- min(centered, na.rm = TRUE)
#     max_val <- max(centered, na.rm = TRUE)
#     if (max_val == min_val) {
#       rep(0, .N)
#     } else {
#       2 * ((centered - min_val) / (max_val - min_val)) - 1
#     }
#   }
# }, by = .(variable, year)]
# 
# 
# 
# county_means[, stat_rescaled := {
#   if (unique(variable) == "corruption") {
#     min_val <- min(stat_log, na.rm = TRUE)
#     max_val <- max(stat_log, na.rm = TRUE)
#     if (max_val == min_val) {
#       rep(0, .N)
#     } else {
#       (stat_log - min_val) / (max_val - min_val)
#     }
#   } else {
#     centered <- stat_log - log(2)  # log(2)
#     min_val <- min(centered, na.rm = TRUE)
#     max_val <- max(centered, na.rm = TRUE)
#     if (max_val == min_val) {
#       rep(0, .N)
#     } else {
#       2 * ((centered - min_val) / (max_val - min_val)) - 1
#     }
#   }
# }, by = .(variable, year)]


# → Now:
# - stat_mean = 0 → stat_log_scaled = log(2)
# - stat_mean < 0 → < log(2)
# - stat_mean > 0 → > log(2)


# 
# # For state-level scale domains
# stat_domains_state <- state_data[, .(
#   min_stat = min(stat_rescaled, na.rm = TRUE),
#   max_stat = max(stat_rescaled, na.rm = TRUE)
# ), by = .(variable, year)]
# 
# # For county-level scale domains
# stat_domains_county <- county_data[, .(
#   min_stat = min(stat_rescaled, na.rm = TRUE),
#   max_stat = max(stat_rescaled, na.rm = TRUE)
# ), by = .(variable, year)]


# For state-level scale domains
stat_domains_state <- state_data[, .(
  min_stat = min(stat, na.rm = TRUE),
  max_stat = max(stat, na.rm = TRUE)
), by = .(variable, year)]

# For county-level scale domains
stat_domains_county <- county_data[, .(
  min_stat = min(stat, na.rm = TRUE),
  max_stat = max(stat, na.rm = TRUE)
), by = .(variable, year)]



# 
# 
# # For state-level scale
# stat_domains_state <- state_means[, .(
#   min_stat = min(stat_rescaled, na.rm = TRUE),
#   max_stat = max(stat_rescaled, na.rm = TRUE)
# ), by = .(variable, year)]
# 
# # For county-level scale
# stat_domains_county <- county_means[, .(
#   min_stat = min(stat_rescaled, na.rm = TRUE),
#   max_stat = max(stat_rescaled, na.rm = TRUE)
# ), by = .(variable, year)]
# 
# stat_domains_county
# stat_domains_state

# Load shapefiles from local folder
states <- st_read("data/states/cb_2021_us_state_20m.shp", quiet = TRUE)
counties <- st_read("data/counties/cb_2021_us_county_20m.shp", quiet = TRUE)

states <- st_transform(states, crs = 5070)
counties <- st_transform(counties, crs = 5070)

# Exclude Alaska, Hawaii, Puerto Rico
states <- states[!states$STUSPS %in% c("AK", "HI", "PR"), ]
counties <- counties[!counties$STATEFP %in% c("02", "15", "72"), ]

# Fix FIPS formats
states$STATEFP <- sprintf("%02s", states$STATEFP)
counties$GEOID <- sprintf("%05s", counties$GEOID)




# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  titlePanel(tagList(
    "Flourishing Statistics Explorer",
    tags$div("(numbers not final, use with care)", style = "font-size: 0.7em; color: grey;")
  )),
  sidebarLayout(
    sidebarPanel(
      selectInput("geo_level", "Geographic Level", choices = c("State", "County")),
      selectInput("var", "Variable", choices = all_vars, selected = "happiness"),
      sliderInput("year", "Year",
                  min = min(all_years), max = max(all_years), value = min(all_years),
                  step = 1, animate = animationOptions(interval = 1500, loop = TRUE)
      ),
      materialSwitch("presidents", "Show Presidential Terms", value = FALSE, status = "primary"),
      materialSwitch("covid_switch", "COVID-19 Marker", value = FALSE),
      radioButtons("theme", "Theme", choices = c("Light", "Dark"), inline = TRUE),
      downloadButton("downloadData", "Download CSV"),
      downloadButton("downloadMap", "Download PNG")
    ),
    mainPanel(
      plotOutput("map", height = "400px"),
      uiOutput("var_description"),
      plotlyOutput("timeseries_plot", height = "400px")
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    if (input$theme == "Dark") {
      session$setCurrentTheme(bs_theme(bootswatch = "darkly"))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = "flatly"))
    }
  })
  
  ts_data <- reactive({
    req(input$var)
    plot_variable_timeseries_plotly(monthly_state_data, input$var, presidents = input$presidents, covid = input$covid_switch)
  })
  
  output$timeseries_plot <- renderPlotly({
    ts_data()
  })
  
  output$map <- renderPlot({
    map_data <- filtered_data()
    
    # Compute range and ±10% padding
    stat_range <- range(map_data$stat, na.rm = TRUE)
    padding <- 0.1 * diff(stat_range)
    lower_limit <- stat_range[1] - padding
    upper_limit <- stat_range[2] + padding
    
    # Start base plot
    p <- ggplot(map_data) +
      geom_sf(aes(fill = stat), color = "black", size = 0.2)
    
    # Choose scale based on variable type
    if (input$var == "corruption") {
      p <- p + scale_fill_gradient(
        low = "white", high = "darkred", name = "Corruption",
        limits = c(lower_limit, upper_limit),
        na.value = "grey95"
      )
    } else {
      midpoint <- 0
      p <- p + scale_fill_gradient2(
        low = "red", mid = "white", high = "blue", midpoint = midpoint,
        limits = c(lower_limit, upper_limit),
        name = input$var,
        na.value = "grey95"
      )
    }
    
    # Add outer bounding box
    bbox <- st_bbox(states)
    p <- p + geom_rect(
      aes(xmin = bbox["xmin"], xmax = bbox["xmax"],
          ymin = bbox["ymin"], ymax = bbox["ymax"]),
      fill = NA, color = "black", linewidth = 0.5
    )
    
    # Final styling
    p +
      coord_sf(expand = FALSE) +
      theme_minimal() +
      theme(
        aspect.ratio = 0.55,
        legend.position = "right",
        panel.grid = element_blank()
      )
    
    if (input$geo_level == "County") {
      # Thin black borders for counties
    #  p <- p + geom_sf(data = map_data, fill = NA, color = "black", size = 0.1)
      
      # Thicker black borders for state outlines
      p <- p + geom_sf(data = states, fill = NA, color = "black", linewidth = 0.3)
    }
    p
  })
  
  output$map_old <- renderPlot({
    map_data <- filtered_data()
    p <- ggplot(map_data) +
      geom_sf(aes(fill = stat), color = "black", size = 0.2)
    
    if (input$var == "corruption") {
      p <- p + scale_fill_gradient(low = "white", high = "darkred", name = "Corruption", 
                                   limits = c(0, 1), na.value = "grey95")
    } else {
      p <- p + scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                                    limits = c(-1, 1), name = input$var, na.value = "grey95")
    }
    
    bbox <- st_bbox(states)
    p <- p+ geom_rect(
      aes(xmin = bbox["xmin"], xmax = bbox["xmax"],
          ymin = bbox["ymin"], ymax = bbox["ymax"]),
      fill = NA, color = "black", linewidth = 0.5
    )
    p + 
      coord_sf(expand = FALSE) +
      theme_minimal() +
      theme(
        aspect.ratio = 0.55,
        legend.position = "right",
        panel.grid = element_blank()
      )
    
    
  })
  
  
  
  filtered_data <- reactive({
    req(input$var, input$year, input$geo_level)
    
    if (input$geo_level == "State") {
      data <- state_data[variable == input$var & year == input$year]
      data[, FIPS := sprintf("%02s", FIPS)]
      merged <- merge(states, data, by.x = "STATEFP", by.y = "FIPS", all.x = TRUE)
    } else {
      data <- county_data[variable == input$var & year == input$year]
      data[, StateCounty := sprintf("%05s", StateCounty)]
      merged <- merge(counties, data, by.x = "GEOID", by.y = "StateCounty", all.x = TRUE)
    }
    
    return(merged)
  })
  
  output$var_description <- renderUI({
    req(input$var)
    entry <- var_info[variable == input$var]
    
    if (nrow(entry) == 0) {
      return(HTML("<p>No description available for this variable.</p>"))
    }
    
    HTML(sprintf(
      "<div style='margin-top: 20px; padding: 10px; border: 1px solid #ccc; border-radius: 5px;'>
      <h5><b>%s</b> (%s)</h5><p> %s </p></div>", entry$label, entry$variable,entry$description
    ))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("flourishing_", input$geo_level, "_", input$year, ".csv")
    },
    content = function(file) {
      fwrite(filtered_data(), file)
    }
  )
  
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste0("map_", input$geo_level, "_", input$year, ".png")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 12, height = 8, dpi = 300)
    }
  )
}

# Run app
shinyApp(ui, server)
