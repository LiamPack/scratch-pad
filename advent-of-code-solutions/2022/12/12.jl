const dirs = CartesianIndex.([(-1, 0), (0, -1), (1, 0), (0, 1)])

input_strs = readlines("input.txt")
elevs = hcat((input_strs .|> x -> map(y -> y - 'a', collect(x)))...) |> transpose
start = findfirst(elevs .== 'S' - 'a')
finish = findfirst(elevs .== 'E' - 'a')
elevs[start] = 0
elevs[finish] = 'z' - 'a'

function BFS(A::Array{Int,2}, start::CartesianIndex{2}, finish_criteria::Function, elev_check::Function)
    D::Array{Float64,2} = similar(A)
    D = fill!(D, Inf)
    D[start] = 0

    stack = [start]
    while !isempty(stack)
        cur_pos = popfirst!(stack)
        if finish_criteria(elevs, cur_pos)
            return D[cur_pos]
        end
        for dir in dirs
            new_pos = cur_pos + dir
            !(checkbounds(Bool, D, new_pos) && elev_check(elevs, cur_pos, new_pos)) && continue
            if D[new_pos] == Inf
                push!(stack, new_pos)
            end
            if D[new_pos] > D[cur_pos] + 1
                D[new_pos] = D[cur_pos] + 1
            end
        end
    end
    D
end

# First elevation condition is "Can't go up more than 1", second
# condition is "Can't go down more than one" -- as if you were playing a
# valid path back in reverse.
p1 = BFS(elevs, start, (_, x) -> x == finish, (A, x, y) -> A[y] - A[x] <= 1)
p2 = BFS(elevs, finish, (elevs, x) -> elevs[x] == 0, (A, x, y) -> A[x] - A[y] <= 1)
println((p1, p2))
