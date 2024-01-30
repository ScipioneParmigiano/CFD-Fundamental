library(animation)

########################################################
# Step 1: 1D Linear Convection
########################################################

X <- 41                     # Number of spatial points
T <- 100                    # Number of time steps
c <- 1                      # Wave speed
dt <- 0.025                 # Time step size
dx <- 2 / (X - 1)           # Spatial step size

# Initialise spatial grid and initial condition
x <- seq(0, 5, length.out = X)
u <- rep(1, X)
u[(x >= 0.5) & (x <= 1)] <- 2

# Function to update the plot
animate_convection <- function() {
  for (t in 1:T) {
    global_u <- u
    for (i in 2:X) {
      u[i] <- global_u[i] - c * (global_u[i] - global_u[i - 1]) * dt / dx
    }
    plot(x, u, type = 'l', ylim = c(0.5, 2.5), xlab = 'x', ylab = 'u', main = '1D Linear Convection')
  }
}

# Create the animation
ani.options(interval = 50, nmax = 1)
saveHTML({
  animate_convection()
}, htmlfile = "linear_convection_animation.html", autobrowse = TRUE)
