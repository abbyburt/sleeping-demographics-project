# import libraries
library(shiny)
library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggpmisc)
library(rsconnect)
library(viridis)
library(bslib)
library(RColorBrewer)

# load data
sleeping_couples <- read.csv("data/sleeping-alone-data.csv", sep = ",", na.strings = c("", "NA"), check.names = FALSE)

# cleaning data names
sleeping_couples <- clean_names(sleeping_couples) %>%
  rename(
    # renaming general columns
    status = which_of_the_following_best_describes_your_current_relationship_status,
    rel_length = how_long_have_you_been_in_your_current_relationship_if_you_are_not_currently_in_a_relationship_please_answer_according_to_your_last_relationship,
    sep_often = when_both_you_and_your_partner_are_at_home_how_often_do_you_sleep_in_separate_beds,
    you_sleep_where = when_youre_not_sleeping_in_the_same_bed_as_your_partner_where_do_you_typically_sleep,
    spouse_sleep_where = when_youre_not_sleeping_in_the_same_bed_where_does_your_partner_typically_sleep,
    first_sep = when_was_the_first_time_you_slept_in_separate_beds,
    sep_helps = to_what_extent_do_you_agree_with_the_following_statement_sleeping_in_separate_beds_helps_us_to_stay_together,
    sleep_better_sep = to_what_extent_do_you_agree_with_the_following_statement_we_sleep_better_when_we_sleep_in_separate_beds,
    sex_life = to_what_extent_do_you_agree_with_the_following_statement_our_sex_life_has_improved_as_a_result_of_sleeping_in_separate_beds,
    occupation = which_of_the_following_best_describes_your_current_occupation,
    # renaming reasons
    snore = what_are_the_reasons_that_you_sleep_in_separate_beds_please_select_all_that_apply,
    bathroom_trips = x_3,
    one_is_sick = x_4,
    no_phys_intimate = x_5,
    temperature_pref = x_6,
    fight = x_7,
    no_space = x_8,
    no_share_cov = x_9,
    sleep_child = x_10,
    dif_schedule = x_11
  )

# get rid of columns difficult to see
sleeping_couples <- sleeping_couples[, -7]
sleeping_couples <- subset(sleeping_couples, select = -x_2)
sleeping_couples <- subset(sleeping_couples, select = -x_12)
sleeping_couples <- subset(sleeping_couples, select = -x_13)

# get rid of 1st row that was used to label 
sleeping_couples <- sleeping_couples[-1, ]

# changing reasons to 0/1
sleeping_couples$snore <- ifelse(is.na(sleeping_couples$snore), 0, 1)
sleeping_couples$bathroom_trips <- ifelse(is.na(sleeping_couples$bathroom_trips), 0, 1)
sleeping_couples$one_is_sick <- ifelse(is.na(sleeping_couples$one_is_sick), 0, 1)
sleeping_couples$no_phys_intimate <- ifelse(is.na(sleeping_couples$no_phys_intimate), 0, 1)
sleeping_couples$temperature_pref <- ifelse(is.na(sleeping_couples$temperature_pref), 0, 1)
sleeping_couples$fight <- ifelse(is.na(sleeping_couples$fight), 0, 1)
sleeping_couples$no_space <- ifelse(is.na(sleeping_couples$no_space), 0, 1)
sleeping_couples$no_share_cov <- ifelse(is.na(sleeping_couples$no_share_cov), 0, 1)
sleeping_couples$sleep_child <- ifelse(is.na(sleeping_couples$sleep_child), 0, 1)
sleeping_couples$dif_schedule <- ifelse(is.na(sleeping_couples$dif_schedule), 0, 1)

# fixing order
sleeping_couples %>%
  mutate(
    sleeping_couples = factor(
      rel_length,
      levels = c("Less than 1 year", "1-5 years", "6-10 years", "11-15 years", "16-20 years", "More than 20 years"),
      labels = c("<1 year", "1-5 years", "6-10 years", "11-15 years", "16-20 years", "20+ years")
    ),
    sleeping_couples = factor(
      status,
      levels = c(
        "Single, but cohabiting with a significant other",
        "In a domestic partnership or civil union",
        "Married",
        "Separated",
        "Divorced",
        "Widowed"
      ),
      labels = c(
        "Dating, but cohabiting",
        "Domestic partnership",
        "Married",
        "Separated",
        "Divorced",
        "Widowed"
      )
    ),
    sleeping_couples = factor(
      sep_often,
      levels = c(
        "Never",
        "Once a year or less",
        "Once a month or less",
        "A few times per month",
        "A few times per week",
        "Every night"
      )
    )
    
  )

# making simpler regions
sleeping_couples <- sleeping_couples %>%
  mutate(us_region = case_when(
    location_census_region %in% c("Mountain", "Pacific") ~ "West",
    location_census_region %in% c("West North Central", "East North Central") ~ "Midwest",
    location_census_region %in% c("West South Central", "East South Central", "South Atlantic") ~ "South",
    location_census_region %in% c("Middle Atlantic", "New England") ~ "Northeast",
    TRUE ~ ""
  ))

# UI # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Why do American Couples Choose Not to Sleep in the Same Bed?"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create graphs to understand why surveyed American couples opt for not sleeping in the same bed, based on different characteristics you can choose from."),
      selectInput("locationInput", "Bar Graph: U.S. Region",
                  choices = c("All", "West", "Midwest", "South", "Northeast"),
                  selected = "All"),
      radioButtons(inputId = "fill_title",
                   label = "Choose Bar Graph Legend Variable",
                   choices = c("Relationship Length", "Relationship Status",
                               "Household Income", "Sleeping Apart Frequency",
                               "Gender", "Age"),
                   selected = "Relationship Length"), 
      helpText('Warning: Dot Graph may contain mature topics.'),
      checkboxInput("agreebox", "Hide Dot Graph", value = FALSE),
      conditionalPanel(
        condition = "input.agreebox == false",
        helpText('Dot Graph: To what extent do couples agree?')),
      conditionalPanel(
        condition = "input.agreebox == false",
        radioButtons(inputId = "agreement_var",
                     label = "Sleeping separately:",
                     choices = c("Helps us sleep better.",
                                 "Has improved our sex life.",
                                 "Helps us stay together."),
                     selected = "Helps us sleep better.") 
      )
    ), 
    
    mainPanel(
      p("The survey dataset was conducted and provided by FiveThirtyEight, a website that writes and makes statistical analyses on opinion poll, politics, economics, and more in the US.",
        a("Click here to view!", href = "https://github.com/fivethirtyeight/data/blob/master/sleeping-alone-data/sleeping-alone-data.csv")),
      # Output graph
      plotOutput("barGraph", height = "700px", width = "800px")
    )
  )
)



# Server # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
server <- function(input, output) {
  filteredData <- reactive({
    location <- input$locationInput
    chosen_var <- input$fill_title
    
    agreements <- input$agreement_var

    filtered <- sleeping_couples

    # making ALL an option 
    if (location != "All") {
      filtered <- filtered[filtered$us_region == location, ]
    }

  # RELATIONSHIP STATUS CHOICE
    if (chosen_var == "Relationship Status") {
      filtered <- filtered[, c(
        "status", "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      )]
      filtered <- filtered[complete.cases(filtered$status), ]
      meltedData <- gather(filtered, key = "reason", value = "frequency", -status)
      meltedData$reason <- factor(meltedData$reason, levels = c(
        "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      ), labels = c(
        "Snoring", "Frequent Bathroom Trips", "Sickness", "Not Physically Intimate",
        "Differing Temperature Preferences", "Frequent Fights", "Don't Want to Share Covers",
        "One Sleeps with Child", "Different Schedules"
      ),
      meltedData$status <- factor(meltedData$status, levels = c(
        "Single, but cohabiting with a significant other",
        "In a domestic partnership or civil union",
        "Married",
        "Separated",
        "Divorced",
        "Widowed"
      ), labels = c(
        "Dating, but cohabiting",
        "Domestic partnership",
        "Married",
        "Separated",
        "Divorced",
        "Widowed"
      )))
      
  # RELATIONSHIP LENGTH  CHOICE
    } else if (chosen_var == "Relationship Length") {
      filtered <- filtered[, c(
        "rel_length", "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      )]
      filtered <- filtered[complete.cases(filtered$rel_length), ]
      meltedData <- gather(filtered, key = "reason", value = "frequency", -rel_length)
      meltedData$reason <- factor(meltedData$reason, levels = c(
        "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      ), labels = c(
        "Snoring", "Frequent Bathroom Trips", "Sickness", "Not Physically Intimate",
        "Differing Temperature Preferences", "Frequent Fights", "Don't Want to Share Covers",
        "One Sleeps with Child", "Different Schedules"
      ),
      meltedData$rel_length <- factor(meltedData$rel_length, levels = c(
        "Less than 1 year",
        "1-5 years",
        "6-10 years",
        "11-15 years",
        "16-20 years",
        "More than 20 years"
      ), labels = c(
        "<1 year",
        "1-5 years",
        "6-10 years",
        "11-15 years",
        "16-20 years",
        "20+ years")))
      
  # HOUSEHOLD INCOME CHOICE
    } else if (chosen_var == "Household Income") {
      filtered <- filtered[, c(
        "household_income", "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      )]
      filtered <- filtered[complete.cases(filtered$household_income), ]
      meltedData <- gather(filtered, key = "reason", value = "frequency", -household_income)
      meltedData$reason <- factor(meltedData$reason, levels = c(
        "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      ), labels = c(
        "Snoring", "Frequent Bathroom Trips", "Sickness", "Not Physically Intimate",
        "Differing Temperature Preferences", "Frequent Fights", "Don't Want to Share Covers",
        "One Sleeps with Child", "Different Schedules"
      ),
      meltedData$household_income <- factor(meltedData$household_income, levels = c(
        "$0 - $24,999",
        "$25,000 - $49,999",
        "$50,000 - $99,999",
        "$100,000 - $149,999",
        "$150,000+"
      ), labels = c(
        "$0 - 24,999",
        "$25k - 49,999",
        "$50k - 99,999",
        "$100k - 149,999",
        "$150k+")))
      
  # Sleeping Apart Frequency   CHOICE
    } else if (chosen_var == "Sleeping Apart Frequency") {
      filtered <- filtered[, c(
        "sep_often", "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      )]
      filtered <- filtered[complete.cases(filtered$sep_often), ]
      # Drop rows where sep_often is "Never"
      filtered <- filtered[filtered$sep_often != "never", ]
      
      meltedData <- gather(filtered, key = "reason", value = "frequency", -sep_often)
      meltedData$reason <- factor(meltedData$reason, levels = c(
        "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      ), labels = c(
        "Snoring", "Frequent Bathroom Trips", "Sickness", "Not Physically Intimate",
        "Differing Temperature Preferences", "Frequent Fights", "Don't Want to Share Covers",
        "One Sleeps with Child", "Different Schedules"
      ))
      
      meltedData$sep_often <- factor(meltedData$sep_often, levels = c(
        "Once a year or less",
        "Once a month or less",
        "A few times per month",
        "A few times per week",
        "Every night"
      ), labels = c(
        "Once a year or less",
        "Once a month or less",
        "Few times per month",
        "Few times per week",
        "Everyday"))
     # get rid of NA 
      meltedData <- meltedData[meltedData$sep_often %in% c(
        "Once a year or less",
        "Once a month or less",
        "Few times per month",
        "Few times per week",
        "Everyday"
      ), ]
      
      
      

  # GENDER CHOICE
    } else if (chosen_var == "Gender") {
      filtered <- filtered[, c(
        "gender", "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      )]
      filtered <- filtered[complete.cases(filtered$gender), ]
      meltedData <- gather(filtered, key = "reason", value = "frequency", -gender)
      meltedData$reason <- factor(meltedData$reason, levels = c(
        "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      ), labels = c(
        "Snoring", "Frequent Bathroom Trips", "Sickness", "Not Physically Intimate",
        "Differing Temperature Preferences", "Frequent Fights", "Don't Want to Share Covers",
        "One Sleeps with Child", "Different Schedules"
      ))
      
  # AGE CHOICE
    } else if (chosen_var == "Age") {
      filtered <- filtered[, c(
        "age", "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      )]
      filtered <- filtered[complete.cases(filtered$age), ]
      meltedData <- gather(filtered, key = "reason", value = "frequency", -age)
      meltedData$reason <- factor(meltedData$reason, levels = c(
        "snore", "bathroom_trips", "one_is_sick", "no_phys_intimate",
        "temperature_pref", "fight", "no_share_cov",
        "sleep_child", "dif_schedule"
      ), labels = c(
        "Snoring", "Frequent Bathroom Trips", "Sickness", "Not Physically Intimate",
        "Differing Temperature Preferences", "Frequent Fights", "Don't Want to Share Covers",
        "One Sleeps with Child", "Different Schedules"
      ),
      meltedData$age <- factor(meltedData$age, levels = c(
        "18-29",
        "30-44",
        "45-60",
        "> 60"
      ), labels = c(
        "18 - 29",
        "30 - 44",
        "45 - 60",
        "60+")))
    } else {
      meltedData <- NULL
    }

    return(meltedData)
    
  })


# Render the bar graph # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    output$barGraph <- renderPlot({
    
    fill_variable <- switch(input$fill_title,
                            "Relationship Status" = "status",
                            "Relationship Length" = "rel_length",
                            "Household Income" = "household_income",
                            "Sleeping Apart Frequency" = "sep_often",
                            "Gender" = "gender",
                            "Age" = "age"
    )
    
    legend_title <- switch(input$fill_title,
                           "Relationship Status" = "Relationship Status",
                           "Relationship Length" = "Relationship Length",
                           "Household Income" = "Household Income",
                           "Sleeping Apart Frequency" = "Sleeping Apart Frequency",
                           "Gender" = "Gender",
                           "Age" = "Age"
    )

  # dynamic title 
    location <- input$locationInput
    if (location == "All") {
      title <- paste("Reasons by", legend_title, "in the USA")
    } else {
      title <- paste("Reasons by", legend_title, "in", location, "USA")
    }
    

    
# FIRST GRAPH !!!!!!
    max_value <- max(filteredData()$frequency)
    
    bar_graph <- ggplot(
      filteredData(), 
      aes(x = reorder(reason, frequency),
          y = frequency, 
          fill = get(fill_variable))) +
      geom_col(aes(color = get(fill_variable))) +
      theme_minimal() +
      xlab(NULL) +
      ylab("Count") +
      theme(panel.border = element_blank(),
            panel.grid.major = element_line(color = "darkgray", linewidth  = 0.2),
            panel.grid.minor = element_line(color = "darkgray", linewidth = 0.2)) +
      ggtitle(title) +
      coord_flip() + 
      scale_fill_manual(values = brewer.pal(6, "BrBG")) +
      scale_color_manual(values = brewer.pal(6, "BrBG")) + # gets rid of ugly lines !
      labs(fill = legend_title, color = legend_title) +
      theme(axis.text = element_text(size = 15),
            legend.position = "right", 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15), 
            title = element_text(size = 15),
            legend.key = element_blank()) +
      guides(fill = guide_legend(override.aes = list(color = NULL)))

    
    # SECOND GRAPH 
    
    # colors for agreements 
    custom_colors <- c(
      "Strongly agree" = "seagreen1",
      "Somewhat agree" = "darkolivegreen2",
      "Neither agree nor disagree" = "gold",
      "Somewhat disagree" = "salmon",
      "Strongly disagree" = "red"
    )
    
    #dot graph 1 - helps stay together
    dot_plot_counts <- sleeping_couples %>%
      group_by(sep_helps) %>%
      summarise(count = n()) %>%
      drop_na(sep_helps) %>%
      mutate(sep_helps = factor(sep_helps, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>%
      arrange(desc(sep_helps))
    
    dot_plot <- ggplot(
      dot_plot_counts, 
      aes(x = sep_helps, y = count, color = sep_helps)) +
      geom_point(size = 3) +
      labs(x = NULL, y = "Count") +
      geom_segment(
        aes(x = sep_helps, 
            xend = sep_helps, 
            y = 0, 
            yend = count, 
            color = sep_helps), 
        linewidth = 1.5) +
      theme_minimal() +
      ggtitle('"Sleeping separately helps us stay together."') + 
      coord_flip() + 
      scale_color_manual(values = custom_colors) +
      scale_y_continuous(breaks = seq(0, 225, by = 50)
      ) +
      theme(axis.text = element_text(size = 15), 
            title = element_text(size = 15), 
            panel.grid.major = element_line(color = "darkgray", linewidth = 0.2),
            panel.grid.minor = element_line(color = "darkgray", linewidth = 0.2)) +
      guides(color = FALSE) 
    
    #dot graph 2- sleep better
    dot_plot_counts2 <- sleeping_couples %>%
      group_by(sleep_better_sep) %>%
      summarise(count = n()) %>%
      drop_na(sleep_better_sep) %>%
      mutate(sleep_better_sep = factor(sleep_better_sep, 
                                       levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>%
      arrange(desc(sleep_better_sep))
    
    dot_plot2 <- ggplot(
      dot_plot_counts2, 
      aes(x = sleep_better_sep, y = count, color = sleep_better_sep)) +
      geom_point(size = 3) +
      labs(x = NULL, y = "Count") +
      geom_segment(
        aes(x = sleep_better_sep, 
            xend = sleep_better_sep, 
            y = 0, 
            yend = count, 
            color = sleep_better_sep), 
        linewidth = 1.5) +
      theme_minimal() +
      ggtitle('"Sleeping separately helps us sleep better."') + 
      coord_flip() + 
      scale_color_manual(values = custom_colors) +
      scale_y_continuous(breaks = seq(0, 225, by = 50)
      ) +
      scale_y_continuous(limits = c(0, 225)) +
      theme(axis.text = element_text(size = 15), 
            title = element_text(size = 15), 
            panel.grid.major = element_line(color = "darkgray", linewidth = 0.2),
            panel.grid.minor = element_line(color = "darkgray", linewidth = 0.2)) +
      guides(color = FALSE)
    
    #dot graph 3 - better sex life 
    dot_plot_counts3 <- sleeping_couples %>%
      group_by(sex_life) %>%
      summarise(count = n()) %>%
      drop_na(sex_life) %>%
      mutate(sex_life = factor(sex_life, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))) %>%
      arrange(desc(sex_life))
    
    dot_plot3 <- ggplot(
      dot_plot_counts3, 
      aes(x = sex_life, y = count, color = sex_life)) +
      geom_point(size = 3) +
      labs(x = NULL, y = "Count") +
      geom_segment(
        aes(x = sex_life, 
            xend = sex_life, 
            y = 0, 
            yend = count, 
            color = sex_life), 
        linewidth = 1.5) +
      theme_minimal() +
      ggtitle('"Our sex life has improved as a result of \n sleeping separately."') + 
      coord_flip() + 
      scale_color_manual(values = custom_colors) +
      scale_y_continuous(breaks = seq(0, 225, by = 50)
      ) +
      theme(axis.text = element_text(size = 15), 
            title = element_text(size = 15), 
            panel.grid.major = element_line(color = "darkgray", linewidth = 0.2),
            panel.grid.minor = element_line(color = "darkgray", linewidth = 0.2)) +
      guides(color = FALSE)
    
    # blank graph to fill up space on the bottom if user does not want to see dot graph
    blank_graph <- ggplot() + theme_void()
    
    
    
    if (!input$agreebox & input$agreement_var == "Helps us sleep better.") {
      bar_graph / dot_plot2
    } else if (!input$agreebox & input$agreement_var == "Has improved our sex life.") {
      bar_graph / dot_plot3
    } else if (!input$agreebox & input$agreement_var == "Helps us stay together.") {
      bar_graph / dot_plot
    } else {
      bar_graph / blank_graph
    }
    
    
    
  } )
}

# Run the Shiny app # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
shinyApp(ui, server)
