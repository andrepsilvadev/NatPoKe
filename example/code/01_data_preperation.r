# Author: Stefan Fallert, 2024
library(here)
create_dataframe <- function(
    path,
    species_names,
    reproduction_rate_estimate,
    carrying_capacity_estimate,
    dispersal_max_distance,
    optimum_forest_cover,
    initial_abundance,
    placeholder_trait
    # here could be many more traits
) {
    df <- data.frame(
        species = species_names,
        reproduction_rate = reproduction_rate_estimate,
        carrying_capacity = carrying_capacity_estimate,
        dispersal_max_distance = dispersal_max_distance,
        optimum_forest_cover = optimum_forest_cover,
        initial_abundance = initial_abundance,
        placeholder_trait = placeholder_trait
    )
    write.csv2(df, file = path, row.names = FALSE)
}

# Create some dummy data
n_species <- 5
create_dataframe(
    here("example/clean_data", "species_traits.csv"),
    species_names = sample(colors(), n_species, TRUE),
    reproduction_rate_estimate = runif(n_species, 0.1, 0.9),
    carrying_capacity_estimate = sample(10:1000, n_species, TRUE),
    dispersal_max_distance =  sample(2:10, n_species, TRUE),
    optimum_forest_cover = seq(0, 1, length.out = n_species),
    initial_abundance = sample(10:100, n_species, TRUE),
    placeholder_trait = sample(LETTERS, n_species, TRUE)
)
