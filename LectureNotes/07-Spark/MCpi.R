library(tidyverse)

#' Simulate points in the unit square and calculate pi
#' @param n_points Number of points to simulate
#' @param seed Random seed for reproducibility
#' @return List containing estimated pi, points dataframe, and accuracy metrics
simulate_pi <- function(n_points, seed = 42) {
    set.seed(seed)
    
    # Generate points using map2_dfr
    points <- map2_dfr(
        map(1:n_points, ~runif(1)),
        map(1:n_points, ~runif(1)),
        ~tibble(x = .x, y = .y)
    )
    
    # Calculate distance from origin and determine if inside circle
    points <- points %>%
        mutate(
            distance = sqrt(x^2 + y^2),
            inside_circle = distance <= 1
        )
    
    # Calculate pi estimate
    points_inside <- sum(points$inside_circle)
    pi_estimate <- 4 * points_inside / n_points
    
    # Calculate error metrics
    absolute_error <- abs(pi - pi_estimate)
    relative_error <- absolute_error / pi * 100
    
    # Return results
    list(
        pi_estimate = pi_estimate,
        points = points,
        metrics = list(
            n_points = n_points,
            points_inside = points_inside,
            absolute_error = absolute_error,
            relative_error = relative_error
        )
    )
}

#' Plot the Monte Carlo simulation results
#' @param simulation_result Result from simulate_pi function
#' @return ggplot object
plot_simulation <- function(simulation_result) {
    simulation_result$points %>%
        ggplot(aes(x = x, y = y, color = inside_circle)) +
        geom_point(alpha = 0.5) +
        stat_function(
            fun = function(x) sqrt(1 - x^2),
            color = "black",
            n = 1000
        ) +
        coord_fixed() +
        scale_color_manual(
            values = c("FALSE" = "blue", "TRUE" = "red"),
            labels = c("Outside", "Inside")
        ) +
        labs(
            title = sprintf(
                "Monte Carlo Estimation of π\nEstimate: %.6f (Error: %.2f%%)",
                simulation_result$pi_estimate,
                simulation_result$metrics$relative_error
            ),
            x = "x",
            y = "y",
            color = "Point Location"
        ) +
        theme_minimal()
}

# Example usage:
result <- simulate_pi(100000)
print(sprintf("Estimated π: %.6f", result$pi_estimate))
print(sprintf("Absolute error: %.6f", result$metrics$absolute_error))
print(sprintf("Relative error: %.2f%%", result$metrics$relative_error))

# Create visualization
plot_simulation(result)