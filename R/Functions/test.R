health_data_path <- "Data/test_data.xlsx"
climate_data_path <- "Data/Climate_data_gh.xlsx"
map_path <- "Data/Gha_geodata/District.shp"
region_col <- "region"
district_col <- "district"
year_col <- "year"
month_col <- "month"
case_col <- "malaria_ud_five"
case_type <- "malaria"
tot_pop_col <- "pop_tot"
tmin_col <- "tmin"
tmean_col <- "tmean"
tmax_col <- "tmax"
rainfall_col <- "c_rain"
r_humidity_col <- "rh"
runoff_col <- "ranoff"
geometry_col <- "geometry"
date_col= NULL
spi_col = NULL
ndvi_col = NULL

# To be set in the next page
max_lag = 2
basis_matrices_choices <- "tmax"
inla_param <- c("tmin", "rainfall", "r_humidity")
param_term <- "tmax"
level <- "country"
param_threshold = 1
filter_year = NULL
family = "poisson"
group_by_year = FALSE
config = !FALSE
save_csv = !FALSE
save_model=!FALSE
save_fig = !FALSE
output_dir = "output_dir"