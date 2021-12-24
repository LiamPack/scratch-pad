using DataStructures

input = split(open(f->read(f, String), "input-p14.txt"), '\n')

tpl = input[1]
rules = Dict(split.(input[3:end-1], " -> "))
pairs = counter(map(*, tpl, tpl[2:end]))
chars = counter(tpl)

for _ in 1:40
    for (s, c) in copy(pairs)
        a,b = s[1], s[2]
        x = rules[a*b]
        pairs[a*b] -= c
        pairs[a*x] += c
        pairs[x*b] += c
        chars[x[1]] += c
    end
end

