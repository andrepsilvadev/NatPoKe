# Author: Stefan Fallert, 2024
library(terra)
library(here)
library(metaRange)

# Check if environment data is available
stopifnot(file.exists(here("example/clean_data/broadleaf_mixed_forest_1km.tif")))
stopifnot(file.exists(here("example/clean_data/species_traits.csv")))

# setup
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)
save_string <- here("example/results/")

# simulation parameters

sim_time_steps <- 20
sim_name <- "example_01"

species_traits <- read.csv(here("example/clean_data/species_traits.csv"), sep = ";", dec = ",")

# image parameters
wid <- 2000
hgt <- 1400
unt <- "px"
ppi <- 300

# load the environment
sim_env <- sds(rast(here("example/clean_data/broadleaf_mixed_forest_1km.tif")))
names(sim_env) <- c("forest_cover")

# create a simulation object
sim <- create_simulation(sim_env)

# since, we just use dummy environment data, we need to set the time layer mapping
# i.e. the simulation needs to know which layer of the environment data corresponds to which time step
# since we only have one layer, we set all time steps to the same layer
sim$set_time_layer_mapping(rep(1, sim_time_steps))

# we loop over the species_traits data frame and add the species to the simulation
for (i in seq_len(nrow(species_traits))) {
    this_species <- species_traits[["species"]][i]

    # "register" the species with the simulation
    sim$add_species(this_species)

    # add traits that need to bes strored at the population level
    sim$add_traits(
        species = this_species,
        population_level = TRUE,

        "abundance" = species_traits[["initial_abundance"]][i],
        "reproduction_rate" = species_traits[["reproduction_rate"]][i],
        "carrying_capacity" = species_traits[["carrying_capacity"]][i],
        "suitability" = 1
        # Note that the line above is shorthand, which sets an inital value.
        # this number will be extended to a matrix of the landscape size automatically
    )

    # add traits that are the same for all populations of a species
    sim$add_traits(
        species = this_species,
        population_level = FALSE,
        "dispersal_distance" =  species_traits[["dispersal_max_distance"]][i],
        "optimum_forest_cover" = species_traits[["optimum_forest_cover"]][i],
        "max_reproduction_rate" = species_traits[["reproduction_rate"]][i],
        "max_carrying_capacity" = species_traits[["carrying_capacity"]][i],

        # the following is jsut a simple kernel, but you can use any function / dispersal kernel you like
        "dispersal_kernel" = calculate_dispersal_kernel(
            max_dispersal_dist = species_traits[["dispersal_max_distance"]][i],
            kfun = negative_exponential_function,
            mean_dispersal_dist = species_traits[["dispersal_max_distance"]][i] / 2,
        )
    )
}


species_names <- sim$species_names()
sim$add_globals(
    # keep track of the species that are still alive
    "alive_species" = species_names
)


# add some global variables to track stats
# i.e. we want to know:
# - the total abundance of each species
# - the total number of suitable cells for each species
# - the total number of occupied cells for each species
# Note: this is mainly for debugging purposes, to save data, there are better ways
species_sum_abundance <- vector("list", length(species_names))
names(species_sum_abundance) <- species_names
for (i in species_names) {
    species_sum_abundance[[i]] <- list(
        "n_abundance" = vector("numeric", sim$number_time_steps),
        "n_suitable" = vector("numeric", sim$number_time_steps),
        "n_occupied" = vector("numeric", sim$number_time_steps)
    )
}
do.call(sim$add_globals, species_sum_abundance)

# now we add the processes that we want.
# this means we need to make up our mind for each species about:
# what are the important ecological processes that we want to simulate (they can be different for each species)
# & in what order do we want to execute them


# To simplify the example, we just add the same processes for all species here
sim$add_process(
    species = species_names,
    process_name = "calculate_general_suitability",
    process_fun = function() {
        self$traits[["suitability"]] <-
            1 - abs(
                self$sim$environment$current[["forest_cover"]] -
                    self$traits[["optimum_forest_cover"]])
    },
    execution_priority = 1
)

sim$add_process(
    species = species_names,
    process_name = "suitability_influence_population_parameter",
    process_fun = function() {
        self$traits[["carrying_capacity"]] <-
            self$traits[["max_carrying_capacity"]] * self$traits[["suitability"]]

        self$traits[["reproduction_rate"]] <-
            self$traits[["max_reproduction_rate"]] * self$traits[["suitability"]]
    },
    execution_priority = 2
)


sim$add_process(
    species = species_names,
    process_name = "reproduction",
    process_fun = function() {
        self$traits[["abundance"]] <-
            ricker_reproduction_model(
                self$traits[["abundance"]],
                self$traits[["reproduction_rate"]],
                self$traits[["carrying_capacity"]]
            )
    },
    execution_priority = 3
)


sim$add_process(
    species = species_names,
    process_name = "dispersal_process",
    process_fun = function() {
        # weighted dispersal
        # i.e. individuals disperse more likely into more suitable cells
        self$traits[["abundance"]] <- dispersal(
            abundance = self$traits[["abundance"]],
            weights = self$traits[["suitability"]],
            dispersal_kernel = self$traits[["dispersal_kernel"]])
    },
    execution_priority = 4
)


sim$add_process(
    process_name = "truncate",
    process_fun = function() {
        # just because there is nothing in reality like 0.5 individuals
        for (i in self$globals[["alive_species"]]) {
            self[[i]]$traits[["abundance"]] <- trunc(self[[i]]$traits[["abundance"]])
        }
    },
    execution_priority = 5
)


sim$add_process(
    process_name = "track_stats",
    process_fun = function() {
        for (i in self$globals[["alive_species"]]) {
            current_abu <- sum(self[[i]]$traits[["abundance"]], na.rm = TRUE)
            # remove species from future process queue if extinct
            # i.e. we don't need to calculate suitability for extinct species
            if (current_abu <= 1) {
                current_abu <- 0
                for (p in self[[i]]$processes) {
                    self$queue$dequeue(p$get_PID())
                }

                self$globals[["alive_species"]] <-
                    self$globals[["alive_species"]][
                        self$globals[["alive_species"]] != i
                    ]
                if (length(self$globals[["alive_species"]]) == 0) {
                    print("all species extinct")
                    self$exit()
                }
            }
            self$globals[[i]][["n_abundance"]][[self$get_current_time_step()]] <-
                current_abu

            self$globals[[i]][["n_suitable"]][[self$get_current_time_step()]] <-
                sum(self[[i]]$traits[["suitability"]] > 0, na.rm = TRUE)

            self$globals[[i]][["n_occupied"]][[self$get_current_time_step()]] <-
                sum(self[[i]]$traits[["abundance"]] > 1, na.rm = TRUE)
        }
    },
    execution_priority = 6
)


# Note: Saving the results is a process that takes the longest time
# because writing a raster to disk is slow
# So think about when you want to save results (each time step vs jsut the last one)
sim$add_process(
    process_name = "save_results",
    process_fun = function() {
        if (self$get_current_time_step() == self$number_time_steps) {
            print("saving final results")

            biodiversity <- matrix(
                0,
                nrow = nrow(self$environment$sourceSDS),
                ncol = ncol(self$environment$sourceSDS)
            )

            for (i in self$globals[["alive_species"]]) {
                intermediate <- as.numeric(self[[i]]$traits[["abundance"]] > 1)
                biodiversity <- biodiversity + intermediate
            }

            r <- terra::rast(
                self$environment$sourceSDS[[1]],
                nlyrs = 1,
                vals = biodiversity
            )
            writeRaster(
                r,
                paste0(save_string, "/", sim_name, "_biodiversity_",
                    sprintf("%03d", self$get_current_time_step()), ".tif"),
                overwrite = TRUE
            )


            for (species in self$globals[["alive_species"]]) {
                save_species(
                    # pass the species object
                    self[[species]],
                    # specify traits we want to save
                    traits = "abundance",
                    # a prefix for each time step
                    prefix = paste0(self$get_current_time_step(), "-"),
                    # where should it be saved
                    path = save_string,
                    overwrite = TRUE
                )
            }
        }
    },
    execution_priority = 7
)

set_verbosity(1L)
print("starting simulation")
sim$begin()
print("simulation finished")

#################################################




# optionally svae some results as csv
res_df <- data.frame()
for (i in species_names) {
    res_df <- rbind(
        res_df,
        data.frame(
            species = i,
            time = 1:sim$number_time_steps,
            alive = i %in% sim$globals[["alive_species"]],
            n_abundance = sim$globals[[i]][["n_abundance"]],
            n_suitable = sim$globals[[i]][["n_suitable"]],
            n_occupied = sim$globals[[i]][["n_occupied"]]
        )
    )
}
write.csv(res_df, paste0(save_string, "/", sim_name, "_res_df.csv"), row.names = FALSE)


# plotting (example, works with basically all matrix like traits)
plot_cols <- hcl.colors(100, "Purple-Yellow", rev = TRUE)

for (i in species_names) {
    png(
        filename = paste0(save_string, "/", sim_name, "_", i, "_abundance.png"),
        width = wid,
        height = hgt,
        units = unt,
        res = ppi
    )
    plot(sim, i, "abundance", col = plot_cols)
    dev.off()
}
