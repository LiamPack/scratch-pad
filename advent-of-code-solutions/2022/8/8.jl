function sight_lines(T, i, j)
        return (T[1:(i-1), j] |> reverse,
                T[(i+1):n, j],
                T[i, 1:(j-1)] |> reverse,
                T[i, (j+1):m])
end

function is_visible(T, i, j)
    n, m = size(T)
    any(all.(sight_lines(T,i,j) .|> x -> x .< T[i,j]))
end

function scenic_score(T, i, j)
    lines = sight_lines(T, i, j)
    lengths = findfirst.(x -> x >= T[i,j], lines)
    lengths = [(i  === nothing ? length(l) : i) for (i,l) in zip(lengths, lines)]
    reduce(*, lengths)
end

input_strs = readlines("input.txt")
trees = hcat((input_strs .|> x -> map(y -> parse(Int, y), collect(x)))...) |> transpose
n, m = size(trees)

visible = zeros(Int, size(trees)) # 1 = visible, 0 = not visible
visible[:, end] = visible[end, :] = visible[:, 1] = visible[1, :] .= 1
foreach(x -> visible[x...] = is_visible(trees, x...), Iterators.product(2:(n-1), 2:(m-1)))

p1 = count(==(1), visible)

scenics = zeros(Int, size(trees))
foreach(x -> scenics[x...] = scenic_score(trees, x...), Iterators.product(2:(n-1), 2:(m-1)))

p2 = maximum(scenics)
println((p1, p2))
