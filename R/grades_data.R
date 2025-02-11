
grades_data <- function(df){

  # Create a dataframe with assignment names, scores, weights, and types
  possible_grades <- data.frame(
    Assignment = c(
      "Getting.to.Know.You.Survey", "Terminus.Scavenger.Hunt",
      "Week.2.Discussion", "Week.3.Discussion", "Week.4.Discussion", "Week.5.Discussion",
      "Week.6.Discussion", "Week.7.Discussion", "Week.8.Discussion", "Week.9.Discussion", "Week.10.Discussion",
      "Week.2.3.Reflection", "Week.3.4.Reflection", "Week.4.5.Reflection", "Week.5.6.Reflection",
      "Week.6.7.Reflection", "Week.7.8.Reflection", "Week.8.9.Reflection", "Week.9.10.Reflection",
      "Exit.Pre.Spring.Survey", "Upload.Spring.Class.Schedule",
      "hw01_Command.Line.and.Git", "hw02_Intro.to.Programming", "hw03_Control.Structures",
      "hw04_File.Operations", "hw05_Data.Structures", "hw06_Data.Visualizations",
      "hw07_Web.Scraping", "hw08_Work.With.Text",
      "Midterm.Exam", "Final.Exam"
    ),
    Total = c(
      5, 5,
      2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
      2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
      2.5, 2.5,
      10, 10, 10, 10, 10, 10, 10, 10,
      20, 100
    ),
    Weight = c(
      0.10, 0.10,
      0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10,
      0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10,
      0.10, 0.10,
      0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
      0.25, 0.40
    ),
    Type = c(
      "Participation", "Participation",
      "Participation", "Participation", "Participation", "Participation", "Participation", "Participation", "Participation", "Participation", "Participation",
      "Participation", "Participation", "Participation", "Participation", "Participation", "Participation", "Participation", "Participation",
      "Participation", "Participation",
      "Assignment", "Assignment", "Assignment", "Assignment", "Assignment", "Assignment", "Assignment", "Assignment",
      "Midterm", "Final"
    )
  )

  # Define the syllabus weights
  syllabus_weights <- c(
    "Participation" = 0.10,
    "Assignment" = 0.25,
    "Midterm" = 0.25,
    "Final" = 0.40
  )

  # Count number of assignments per Type
  type_counts <- table(possible_grades$Type)

  # Assign the correct weight for each Type, divided equally among its assignments
  possible_grades$Adjusted_Weight <- syllabus_weights[possible_grades$Type] / type_counts[possible_grades$Type]

  # Compute total score per type
  total_per_type <- tapply(possible_grades$Total, possible_grades$Type, sum)

  # Normalize Actual_Total using syllabus weights
  possible_grades$Actual_Total <- possible_grades$Total / total_per_type[possible_grades$Type] * possible_grades$Weight * 100

  # Verify the sum
  sum(possible_grades$Actual_Total)  # Should be 100

  str(possible_grades)
  head(possible_grades)

  # Convert grades dataframe from wide to long format to match the possible_grades df
  grades_long <- reshape(df, varying = names(df)[-1],
                         v.names = "Score", timevar = "Assignment",
                         times = names(df)[-1], direction = "long",
                         idvar = "Student")

  # Remove the row names that reshape adds to df
  rownames(grades_long) <- NULL

  # Merge student dataframe with possible_grades dataframe
  merged_df <- merge(possible_grades, grades_long, by = "Assignment", all.x = TRUE)

  # Turn Type column values into factors for downstream ease
  merged_df[ , 'Type'] <- as.factor(merged_df[ , 'Type'])
  return(merged_df)
}
