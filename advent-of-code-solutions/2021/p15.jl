using DataStructures

# newline at end
input = map(y->map(x -> parse(Int, x), collect(y)), split(open(f->read(f, String), "input-p15.txt"), '\n'))[1:end-1]
weights = hcat(input...)

function get_neighbors(i, j, maxN, maxM)
    neighbors =
        [(i + 1, j), (i - 1, j), (i, j+1), (i, j - 1)]

    return filter(x -> 0 < first(x) <= maxN && 0 < last(x) <= maxM, neighbors)
end

function djikstras(weights)
    distances = zeros(Float64, size(weights)...)
    fill!(distances, Inf)
    distances[1, 1] = 0

    visited = zeros(Bool, size(weights)...)

    pq = PriorityQueue{Tuple{Int, Int}, Int64}()
    enqueue!(pq, (1, 1) => 0)

    N, M = size(weights)
    while !isempty(pq)
        i, j = dequeue!(pq)
        visited[i,j] = true

        for (p, q) in get_neighbors(i, j, N, M)
            new_cost = weights[p, q] + distances[i, j]
            if !visited[p, q] && distances[p, q] > new_cost
                distances[p, q] = new_cost
                enqueue!(pq, (p, q) => distances[p, q])
            end
        end
    end
    return distances
end

print(djikstras(weights)[end, end])

tmp = vcat((mod1.(weights .+ Int8(i), Int8(9)) for i=0:4)...)
fivex_weights = hcat((mod1.(tmp .+ Int8(i), Int8(9)) for i=0:4)...)
print(djikstras(fivex_weights)[end, end])
