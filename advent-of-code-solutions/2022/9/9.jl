const letters = ["U", "D", "L", "R"]
const direction = [[0, 1], [0, -1], [-1, 0], [1, 0]]
const letter_to_dir = Dict(k => v for (k, v) in zip(letters, direction))

Position = Vector{Int}

struct Command
    direction::Position
    repeat::Int
end

convert_input(x) = Command(letter_to_dir[x[1]], parse(Int, x[2]))
Linf(x::Position) = maximum(abs.(x))
sign(x::Number) = x > 0 ? 1 : x < 0 ? -1 : 0

function step_chain(direction::Position, chain::Vector{Position})
    new_chain = copy(chain)
    new_chain[1] += direction
    for i in 2:length(new_chain)
        diff = new_chain[i-1] - new_chain[i]
        if Linf(diff) > 1
            new_chain[i] += sign.(diff)
        end
    end
    return new_chain
end

function simulate_chain(N::Int, commands::Vector{Command})
    chain = [[0, 0] for _ in 1:N]
    history = [chain]
    for c in commands
        for _ in 1:c.repeat
            push!(history, step_chain(c.direction, history[end]))
        end
    end
    history
end

input = split.(readlines("input.txt")) .|> convert_input

p1 = simulate_chain(2, input) .|> last |> unique |> length
p2 = simulate_chain(10, input) .|> last |> unique |> length

println((p1, p2))
