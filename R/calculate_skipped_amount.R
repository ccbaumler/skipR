# Write a function to calculate the amount of course materials that may be
# skipped for each selected_types factor
calculate_skipped_amount <- function(assignment_data, selected_types, target_grade = 0.90, expected = 0.85) {
  # Calculate students current total
  assignment_data$Actual_Score <- ( assignment_data$Score / assignment_data$Total ) * assignment_data$Actual_Total
  current_total <- sum(assignment_data$Actual_Score, na.rm = TRUE)

  # Clear and create an expected score for assignments that are not included
  # in selected_types and have NA values
  assignment_data$Expected_Score <- NULL
  assignment_data$Expected_Score <- ifelse(!is.na(assignment_data$Actual_Score),
                                           assignment_data$Actual_Score,
                                           ifelse(!assignment_data$Type %in% selected_types,
                                                  ifelse(is.na(assignment_data$Actual_Score),
                                                         assignment_data$Actual_Total * expected,
                                                         assignment_data$Actual_Score),
                                                  assignment_data$Actual_Score))

  possible_total <- sum(assignment_data$Expected_Score, na.rm = T)
  target_total <- target_grade * 100
  print(current_total)
  print(target_total)
  print(possible_total)
  types_with_expected <- unique(assignment_data$Type[is.na(assignment_data$Score) & !assignment_data$Type %in% selected_types])
  if (current_total >= target_total) {
    return(cat("Target grade already reached; Everything in", selected_types, "can be skipped."))
  } else if (possible_total >= target_total) {
    return(cat("Target grade will be reached with expected scores; Everything in",
               selected_types,
               "can be skipped. BUT, if",
               paste(types_with_expected, collapse = ", "),
               "do not meet expected values. Check again."))
  }

  # Compute current total without the NA values
  expected_total <- sum(assignment_data$Expected_Score, na.rm = TRUE)

  hope <- expected_total - current_total
  difference <- target_total - expected_total

  if (difference <= 0) {
    types_with_expected <- unique(assignment_data$Type[is.na(assignment_data$Score) & !assignment_data$Type %in% selected_types])

    return(print(
      paste("Target grade will be reached with grading expectation; Everything in",
            types_with_expected,
            "can be skipped.")
    )
    )
  }

  # Store all possible assignments for this combination
  remaining_options <- subset(assignment_data, is.na(Expected_Score))

  if (nrow(remaining_options) == 0) {
    return("No assignments available to skip.")
  }

  # Initialize skipped counts per Type
  skipped_counts <- setNames(rep(0, length(selected_types)), selected_types)

  # Process each assignment type separately
  for (t in selected_types) {
    assignment_data[[paste0(t, '_Expected_Score')]] <- ifelse(!is.na(assignment_data$Actual_Score),
                                                              assignment_data$Actual_Score,
                                                              ifelse(!assignment_data$Type %in% t,
                                                                     ifelse(is.na(assignment_data$Actual_Score),
                                                                            assignment_data$Actual_Total * expected,
                                                                            assignment_data$Score),
                                                                     assignment_data$Score))

    updated_expected_total <- sum(assignment_data[[paste0(t, '_Expected_Score')]], na.rm = TRUE)
    updated_difference <- target_total - updated_expected_total


    cat("\nChecking", t, "type", "\n")
    type_rows <- remaining_options[remaining_options$Type == t, ]  # Get rows of this type
    cat("    Found", nrow(type_rows), "of", t, '\n')

    type_rows$Actual_Total[is.na(type_rows$Actual_Total)] <- 0
    cat("My target grade is", target_grade,
        "\n    The total points needed to reach this grade is",
        target_total,
        'out of', possible_total,
        '\n    Current total points is', current_total,
        '\n    Current total points with expected points add is', updated_expected_total,
        '\n   ', t, 'must reach', updated_difference, 'to allow skipping\n'
    )

    if (nrow(type_rows) == 0) {
      cat('\nSkipping', t)
      next
    }

    if (updated_difference <= 0) {
      cat("\nTarget grade will be reached with grading expectation; Everything in",
          t,
          "can be skipped.\n")
      next
    }

    types_with_expected <- unique(assignment_data$Type[is.na(assignment_data$Score) & !assignment_data$Type %in% selected_types])

    if (length(types_with_expected) > 0 ) {
      cat("\nWith the expected scores for",
          paste(types_with_expected, collapse = ", "),
          'set to', expected, '\n')
    }

    skipped_total <- 0
    count <- 0

    # Loop through each type of assignment to check how many can be skipped
    for (i in 1:nrow(type_rows)) {
      if ((skipped_total + type_rows$Actual_Total[i]) <= updated_difference) {
        skipped_total <- skipped_total + type_rows$Actual_Total[i]
        count <- count + 1
        print(paste(count, skipped_total, updated_difference))
      } else {
        break  # Stop if adding further would exceed the difference
      }
    }


    skipped_counts[t] <- nrow(type_rows) - (count + 1)


    if (skipped_counts[t] < 0) {
      cat('\nYou are currently only able to reach the expected grade of',
          round(updated_expected_total + skipped_total, 2),
          'not the target grade of',
          target_grade * 100, '%\n'
      )
    } else if (skipped_counts[t] == 0) {
      cat('\nYou are not able to skip any for', t, '\n')
    } else {
      cat('\nFor', t, "you may skip", skipped_counts[t], '\n')
    }
  }

  return(list(skipped_counts, assignment_data))
}
