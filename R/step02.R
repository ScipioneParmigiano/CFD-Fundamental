library(animation)

########################################################
# Step 2: 1D Nonlinear Convection
########################################################

X <- 41                     # Number of spatial points
T_nonlinear <- 100          # Number of time steps for nonlinear convection
dx <- 2 / (X - 1)           # Spatial step size
dt_nonlinear <- 0.2 * dx    # Time step size for nonlinear convection

# Initialise spatial grid and initial condition for nonlinear convection
x <- seq(0, 5, length.out = X)
u_nonlinear <- rep(1, X)
u_nonlinear[(x >= 0.5) & (x <= 1)] <- 2

# Function to update the plot for each frame
animate_nonlinear_convection <- function(n) {
  for (t in 1:n) {
    global_u <- u_nonlinear
    for (i in 2:X) {
      u_nonlinear[i] <- global_u[i] - global_u[i] * (global_u[i] - global_u[i - 1]) * dt_nonlinear / dx
    }
    plot(x, u_nonlinear, type = 'l', ylim = c(0.5, 2.5), xlab = 'x', ylab = 'u', main = '1D Nonlinear Convection')
  }
}

# Create the animation
ani.options(interval = 50)
saveHTML({
  animate_nonlinear_convection(T_nonlinear)
}, htmlfile = "nonlinear_convection_animation.html", autobrowse = TRUE)
