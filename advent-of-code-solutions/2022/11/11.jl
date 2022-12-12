mutable struct Monkey
    items::Vector{Int}
    operation::Function
    divisible_by::Int
    true_throw::Int
    false_throw::Int
    inspects::Int
end

update_items1!(m::Monkey) = begin
    m.items = floor.(Int, m.operation.(m.items) .// 3)
    m.inspects += length(m.items)
end
update_items2!(m::Monkey, N::Int) = begin
    m.items = m.operation.(m.items) .% N
    m.inspects += length(m.items)
end

who_to_throw(m::Monkey) = map(x -> x % m.divisible_by == 0 ? m.true_throw : m.false_throw, m.items)

throw(m::Monkey, n::Vector{Monkey}) = begin
    iter = collect(zip(m.items, who_to_throw(m)))
    while !isempty(iter)
        (item, who) = pop!(iter)
        push!(n[who+1].items, item)
        pop!(m.items)
    end
end

function to_operation(s)
    f(x::Int) = begin 
        args = ((s[1] == "old" ? x : tryparse(Int, s[1])),  (s[3] == "old" ? x : tryparse(Int, s[3])))
        s[2] == "*" ? *(args...) : +(args...)
    end
    f
end

last_to_number(s) = tryparse(Int, split(s)[end])

function parse_monkey(s)
    Monkey(tryparse.(Int, split(replace(s[2], !isnumeric => " "))),
           to_operation(split(s[3])[4:end]),
           last_to_number.(s[4:end])..., 0)
end

function solve1(monkeys)
    for _ in 1:20
        for m in monkeys
            update_items1!(m)
            throw(m, monkeys)
        end
    end
    sort(map(x -> x.inspects, monkeys); rev=true)
end

function solve2(monkeys)
    N = prod(m.divisible_by for m in monkeys)
    println(N)    
    for _ in 1:10_000
        for m in monkeys
            update_items2!(m, N)
            throw(m, monkeys)
        end
    end
    sort([m.inspects for m in monkeys]; rev=true)
end


input_txt = split.(split(readchomp("input.txt"), "\n\n"), "\n")
monkeys = input_txt .|> parse_monkey

p1 = solve1(deepcopy(monkeys))
p2 = solve2(deepcopy(monkeys))
println((prod(p1[1:2]), prod(p2[1:2])))
