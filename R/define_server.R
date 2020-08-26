#' @importFrom shiny renderPlot renderTable
server <- function(input, output, session) {
  # these tables come from setup-tables.R
  output$row_counts <- row_counts
  output$null_counts <- null_counts
  output$ref_integrity <- ref_integrity

  # person table plots
  output$plot_sex <- shiny_sex
  output$person_plot_two <- person_plot_two
  output$person_plot_three <- person_plot_three

  # visit characteristics
  output$vo_table <- vo_table
  output$vo_by_visit_type <- vo_by_visit_type
  output$admission_by_admission_type <- admission_by_admission_type
  output$admission_source <- admission_source
  output$discharge_to <- discharge_to
  output$death_discrep_table <- death_discrep_table
  output$death_time_discrep_plot <- death_time_discrep_plot

  # visit sequencing
  output$overlap_table <- overlap_table

  ## There's a scope + reactivity issue here that I can't quite
  ## figure out (which is why it's not in a separate file)
  ## Also, raises the question of dynamically adjusting the UI
  ## otherwise we need to implement all the filters for everything
  ## in the same tabSetPanel
  output$res_adm_plot <- res_adm_plot

  # readmissions
  output$attendances_per_patient_plot <- attendances_per_patient_plot

  # visit details
  output$visit_detail_transition_plot <- visit_detail_transition_plot
  output$visit_detail_zero_los <- visit_detail_zero_los
  output$visit_detail_los_by_care_site <- visit_detail_los_by_care_site

  # outcomes
  output$outcome_table_with_visit <- outcome_table_with_visit
  output$outcome_table_without_visit <- outcome_table_without_visit

  # distribution of times
  output$distribution_of_times_plot <- distribution_of_times_plot

  # # measurements
  meas_dq <- meas_all %>%
      left_join(dq_ref[,c("concept_id", "short_name", "target_column",
                          "units_concept_id", "limits")],
                by = "concept_id")

  output$measurement_trio <- renderPlot({
    current_concept <- input$measurement_label
    current_name <- meas_dq$concept_name[meas_dq$concept_id == current_concept]
    target_col <- meas_dq$target_column[meas_dq$concept_id == current_concept]

    curr_title <- stringr::str_sub(current_name, 1, 30)
    if (nchar(curr_title) >= 30) {
      curr_title <- paste0(curr_title, "...")
    }

    working <- tbl(ctn, in_schema(schema, "measurement")) %>%
      filter(measurement_concept_id %in% !! current_concept) %>%
      collect() %>%
      mutate(across(where(is.integer64), as.integer)) %>%
      mutate(across(c(contains("date"), -contains("datetime")), as.Date))

    working_units <- as.integer(na.omit(unique(working$unit_concept_id)))
    working_operator <- as.integer(na.omit(unique(working$operator_concept_id)))

    if (length(working_units) > 0) {
      working_units_dict <- mini_dict(ctn, schema, working_units)
      label_units <- paste0(working_units_dict$concept_name, collapse = ", ")
    } else {
      label_units <- "None in use!"
    }

    if (length(working_operator) > 0) {
      working_operator_dict <- mini_dict(ctn, schema, working_operator)
      operator_units <- paste0(working_operator_dict$concept_name, collapse = ", ")
    } else {
      operator_units <- "None in use!"
    }
    measure_n <- nrow(working)
    working <- left_join(
      working %>%
        select(
          person_id,
          measurement_id,
          measurement_date,
          measurement_datetime,
          value_as_number,
          value_as_concept_id,
          visit_occurrence_id),
      st[["visit_occurrence"]] %>%
          select(visit_occurrence_id, visit_start_datetime, visit_end_datetime),
      by = "visit_occurrence_id")

    boundaries <- working %>%
      summarise(
        before = sum(measurement_datetime < visit_start_datetime, na.rm = TRUE),
        after = sum(measurement_datetime > visit_end_datetime, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "condition", values_to = "count")

    # Duplications
    dup <- working %>%
      select(.data$person_id, .data$measurement_datetime, .data[[target_col]]) %>%
      add_count(
        .data$person_id, .data$measurement_datetime, .data[[target_col]],
        name = "dupe_count") %>% 
      filter(dupe_count > 1) %>%
      nrow()

    dup <- tibble::tribble(
      ~condition, ~count,
      "duplications", dup
    )

    miss <- tibble::tribble(
      ~condition, ~count,
      "no visit", sum(is.na(working$visit_occurrence_id))
    )

    if (target_col == "value_as_number") {
      val_dist <- working %>%
        select(value_as_number) %>%
        filter(!is.na(value_as_number))
      
      if (nrow(val_dist) == 0) {
        val_dist <- "bad"
      } else {
        val_dist <- val_dist %>%
          ggplot(aes(x = value_as_number)) +
          geom_density() +
          theme_classic() +
          labs(x = label_units)
      }
    } else {
      opt <- dq_ans[dq_ans$concept_id == current_concept, c("option_concept_id", "option_name")]
      
      val_dist <- working %>%
        select(value_as_concept_id) %>%
        filter(!is.na(value_as_concept_id))
      
      if (nrow(val_dist) == 0) {
        val_dist <- "bad"
      } else {
        val_dist <- val_dist %>%
          group_by(value_as_concept_id) %>%
          tally() %>%
          mutate(value_as_concept_id = factor(
            value_as_concept_id,
            levels = opt$option_concept_id,
            labels = opt$option_name
          )) %>%
          ggplot(aes(
            x = value_as_concept_id)) +
          geom_point(aes(y = n)) +
          geom_segment(aes(
            y = 0,
            yend = n,
            xend = as.factor(value_as_concept_id))) +
          theme_classic() +
          labs(y = "number of respones", x = "categories") +
          theme(axis.title.y = element_blank()) +
          coord_flip()
      }
    }

    # timing distribution
    timing_dist <- working %>%
      select(measurement_datetime) %>%
      filter(!is.na(measurement_datetime))

    if (nrow(timing_dist) == 0) {
      timing_dist <- "bad"
    } else {
      timing_dist <- timing_dist %>%
        mutate(measurement_datetime = hms::as_hms(measurement_datetime)) %>%
        ggplot(aes(x = measurement_datetime)) +
        geom_density() +
        theme_classic() +
        labs(x = "time of sample")
    }

    # samples over time
    sample_timing <- working %>%
      select(measurement_date) %>%
      filter(!is.na(measurement_date))

    if (nrow(sample_timing) == 0) {
      sample_timing <- "bad"
    } else {
      sample_timing <- sample_timing %>%
        group_by(measurement_date) %>%
        tally() %>%
        ggplot(aes(x = measurement_date, y = n)) +
        geom_path() +
        theme_classic() +
        labs(x = "measurement date", y = "daily samples")
    }

    cant_plot <- any(c(class(sample_timing), class(timing_dist), class(val_dist)) %in% "character")

    if (!cant_plot) {
      (val_dist | timing_dist) / sample_timing
    }
  })

}
