# should probably start doing readlines at some point
input = open(f -> read(f, String), "input.txt")[1:end-1] # newline as usual

function solve(s, N)
    for i in 1:length(s)
        if s[i:(i+N-1)] |> unique |> length == N
            return i + N - 1
        end
    end
end

p1 = solve(input, 4)
p2 = solve(input, 14)
