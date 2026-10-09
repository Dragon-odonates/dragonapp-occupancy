# Use this to install package before deployment
# devtools::install_github("Dragon-odonates/dragonapp-occupancy", force = TRUE)

app_path <- here::here("app")

# Deploy the shinyapp to online server
rsconnect::deployApp(
  appDir = app_path,
  appFiles = rsconnect::listDeploymentFiles(app_path),
  appName = "dragon-occupancy",
  appTitle = "Dragonflies occupancy (DRAGON project)"
)
