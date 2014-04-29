# Set random seed for the random generator
#  so that our results are reproducible

function gibs(nrep=105000;
              seed=123,
              nb  =5000,
              ρ   =.5)
   # Seed - sets the random number generator seed
   #   so that it is easy to create reproducible results.
   # nrep - in the number of repetitions of the MCMC draws.
   # nb   - number of observations for the "Burn-in"
   # ρ    - Correlation between Y1 and Y2
   srand(seed)

   # Create empty vectors of length nrep
   yy₁ = yy₂ = spzeros(nrep,1)

   # Calculate standard deviation of Y2
   σ   = sqrt(1-ρ^2)

   # Initialize y1 with a single normal draw
   y₁ = randn()*σ

   # Now we start the Gibbs sampler loop
   for i in 1:nrep
     y₂ = randn()*σ + ρ*y₁
     y₁ = randn()*σ + ρ*y₂
     yy₁[i]=y₁
     yy₂[i]=y₂
   end

   return (yy₁[(nb+1):end,1],yy₂[(nb+1):end,1])
end
gibs()

# Set random seed for the random generator
#  so that our results are reproducible
function gibs2(nrep=105000;
               seed=123,
               nb  =5000,
               ρ   =.5)
   # Seed - sets the random number generator seed
   #   so that it is easy to create reproducible results.
   # nrep - in the number of repetitions of the MCMC draws.
   # nb   - number of observations for the "Burn-in"
   # ρ    - Correlation between Y1 and Y2
   srand(seed)

   # Calculate standard deviation of Y2
   sigma = sqrt(1-ρ^2)

   # Create empty vectors of length nrep
   y1 = randn(nrep)
   y2 = randn(nrep)

   # Now we start the Gibbs sampler loop
   for i in 2:nrep
     y2[i] += y1[i-1]*ρ
     y1[i] += y2[i]*ρ
   end

   return (y1[(nb+1):end,1],y2[(nb+1):end,1])
end
gibs2()

@elapsed yy = gibs()
# 0.063151467
@elapsed gibs(10^6)
# 0.479542057

@elapsed yy = gibs2()
# 0.010729382
@elapsed yy = gibs2(10^6)
# 0.065821774
