const alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" |> collect
const reward_map = Dict(char => i for (char, i) in zip(alphabet, 1:length(alphabet)))

input = split(open(f->read(f, String), "input.txt"), '\n')[1:end - 1]

split_input = map(x -> (x[1:Int(length(x)/2)], x[Int(length(x)/2)+1:end]), input)

function in_common(s...)
    return [intersect((s .|> Set)...)...]
end

p1 = sum(map(x -> reward_map[in_common(x...) |> first], split_input))

grouped_input = reshape(input, 3, :)

p2 = sum(mapslices(x -> reward_map[in_common(x...) |> first], grouped_input, dims=(1)))
