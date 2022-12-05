struct Interval
        l::Number
        r::Number
end

function string_to_interval(s)
        str = split(s, '-')
        return Interval(parse.(Int, str)...)
end

function contains(i::Interval, j::Interval)
        return (i.l <= j.l && i.r >= j.r) || (j.l <= i.l && j.r >= i.r)
end

function overlap(i::Interval, j::Interval)
        return (i.l <= j.l && i.r >= j.l) || (j.l <= i.l && j.r >= i.l)
end

input = split(open(f -> read(f, String), "input.txt"), '\n')[1:end-1]
input = split.(input, ',')
input = [string_to_interval.(x) for x in input]
p1 = length(filter(x -> contains(x...), input))
p2 = length(filter(x -> overlap(x...), input))
