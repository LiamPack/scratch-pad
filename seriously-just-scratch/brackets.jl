# messing around with bracket "cost" functions as you go up the
# bracket. makes pretty fractals and wanted to see if the first half
# of a bracket can be transformed into the same form as the second
# half
using Plots
c(n, s) = 2^n - (s - 1)
cost(n, s) = n == 0 ? 0 : cost(n - 1, s > 2^(n - 1) ? 2^(n - 1) - (s - 2^(n-1)) : s) + c(n, s)
n = 18
xs = [i for i in 1:2^n]
costs = [cost(n,i) for i in 1:2^n]
for i in 1:2^(n-1)
    costs[i] -= c(n, i) - c(n, 2^(n - 1) - (i - 2^(n-1)))
end
display(plot(xs, costs))
display(plot(xs[1:2^(n-1)], costs[1:2^(n-1)]))
