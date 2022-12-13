const dirs = CartesianIndex.([(-1, 0), (0, -1), (1, 0), (0, 1)])

input_strs = readlines("input.txt")
elevs = hcat((input_strs .|> x -> map(y -> y - 'a', collect(x)))...) |> transpose
start = findfirst(elevs .== 'S' - 'a')
finish = findfirst(elevs .== 'E' - 'a')
elevs[start] = 0
elevs[finish] = 'z' - 'a'
in_bounds(A, idx) = (idx[1] > 0 && idx[1] <= size(A,1)) && (idx[2] > 0 && idx[2] <= size(A,2))
valid_elevation(A, from, to) = A[to] - A[from] <= 1
valid_elevation2(A, from, to) = A[from] - A[to] <= 1

function BFS(A, start, finish_criteria, elev_check)
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
            !(in_bounds(elevs, new_pos) && elev_check(elevs, cur_pos, new_pos)) && continue
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

p1 = BFS(elevs, start, (_,x) -> x == finish, valid_elevation)
p2 = BFS(elevs, finish, (elevs, x) -> elevs[x] == 0, valid_elevation2)
