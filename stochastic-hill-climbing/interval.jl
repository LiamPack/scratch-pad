using Distributions
using Plots
d = Normal(0, 1)

f(x, σ) = 2 < x < 5 ? (1/σ) : σ
update(x, σ, L) = mod(x + f(x, σ) * rand(Normal(0,1)), L)

p = plot()
N = 500000
L = 10
for σ in [1, 1.1, 1.5, 2, 3, 4, 5, 10]
    xs = [L*rand(Float64)]
    update0(x) = update(x, σ, L)

    for i in 1:N
        append!(xs, update0(xs[end]))
    end
    stephist!(p, xs, normalize=:pdf, alpha=0.5, label="σ=$(σ)")
    title!(p, "Brownian motion, bias on [2,5], p.b.c")
end
