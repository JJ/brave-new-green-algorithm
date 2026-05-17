using BraveNewAlgorithm

# Define a simple objective function
function objective_function(x::Vector{Float64})
    return sum(x.^2)  # Simple quadratic function
end

# Set up the algorithm parameters
n_dimensions = 2
lower_bounds = [-5.0, -5.0]
upper_bounds = [5.0, 5.0]
population_size = 20
max_generations = 100

# Run the algorithm
result = brave_new_algorithm(
    objective_function,
    n_dimensions,
    lower_bounds,
    upper_bounds,
    population_size,
    max_generations
)

# Print results
println("Best solution found: ", result.best_solution)
println("Best fitness: ", result.best_fitness)
println("Number of generations: ", result.generations)
println("Total evaluations: ", result.evaluations)
