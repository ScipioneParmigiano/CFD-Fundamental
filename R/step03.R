library(animation)

########################################################
# Step 3: 1D Diffusion
########################################################

X <- 41                     # Number of spatial points
T_diffusion <- 100          # Number of time steps for diffusion
v <- 0.1                    # Diffusion coefficient
dx <- 2 / (X - 1)           # Spatial step size
dt_diffusion <- 0.2 * dx^2 / v   # Time step size for diffusion

# Initialise spatial grid and initial condition for diffusion
x <- seq(0, 2, length.out = X)
u_diffusion <- rep(1, X)
u_diffusion[(x >= 0.5) & (x <= 1)] <- 2

# Function to update the plot for each frame
animate_diffusion <- function(n) {
  for (t in 1:n) {
    global_u <- u_diffusion
    for (i in 2:(X - 1)) {
      u_diffusion[i] <- v * dt_diffusion / dx^2 * (global_u[i + 1] - 2 * global_u[i] + global_u[i - 1]) + global_u[i]
    }
    plot(x, u_diffusion, type = 'l', ylim = c(0.5, 2.5), xlab = 'x', ylab = 'u', main = '1D Diffusion')
  }
}

# Create the animation
ani.options(interval = 50)
saveHTML({
  animate_diffusion(T_diffusion)
}, htmlfile = "diffusion_animation.html", autobrowse = TRUE)
