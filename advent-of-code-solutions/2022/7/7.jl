

mutable struct State
    cwd::Vector{String}
    dir_size::Dict{Vector{String},Int}
end

function handle_line!(state::State, line::String)
    if contains(line, "cd /")
        empty!(state.cwd)
    elseif contains(line, "cd ..")
        state.cwd = state.cwd[1:end-1]
    elseif line[3:4] == "cd"
        push!(state.cwd, line[6:end])
    elseif isnumeric(line[1]) # ls
        filesize = parse(Int, first(split(line, ' ')))
        # update each one
        for n in 0:length(state.cwd)
            state.dir_size[state.cwd[1:n]] = get(state.dir_size, state.cwd[1:n], 0) + filesize
        end
    end
end

input = readlines("input.txt")
s = State(Vector{String}(), Dict{Vector{String},Int}())
foreach(x -> handle_line!(s, x), input)

sizes = collect(values(s.dir_size))
p1 = sum(sizes[sizes.<1e5])
p2 = minimum(filter(x -> x >= maximum(sizes) - 4e7, sizes))
print(p1, " ", p2)
