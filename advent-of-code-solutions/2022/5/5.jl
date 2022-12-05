import DataStructures: Stack

function move1(stacks, N, i, j)
    for _ in 1:N
        push!(stacks[j], pop!(stacks[i]))
    end
end

function move2(stacks, N, i, j)
    q = [pop!(stacks[i]) for _ in 1:N]
    for k in 1:N
        push!(stacks[j], q[end-(k-1)])
    end
end

function make_stacks(init)
    indices = 2:4:length(init[1])
    stacks = [Stack{Char}() for _ in indices]
    for row in init[2:end]
        for (j, stack) in zip(indices, stacks)
            if row[j] != ' '
                push!(stack, row[j])
            end
        end
    end
    stacks
end

function solve(init, moves, f)
    stacks = make_stacks(init)
    for m in moves
        f(stacks, m...)
    end
    return [first(stack) for stack in stacks]
end

input = split(open(f -> read(f, String), "input.txt"), "\n\n")[1:end]
input = input .|> x -> split(x, '\n')

init = reverse(input[1])
moves = input[2] .|> x -> replace(x, !isnumeric => ' ') .|> x -> split(x, ' ')
moves = filter.(x -> x != "", moves) .|> x -> map(y -> parse(Int, y), x)
moves = moves[1:end-1]

p1 = solve(init, moves, move1)
p2 = solve(init, moves, move2)
