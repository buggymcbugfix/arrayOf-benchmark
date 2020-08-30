# Benchmarking `smallArrayOf#`

## Results

![A visualisation of output.csv](arrayOf-benchmark.png)

[Writeup on my ¬¬blog](https://github.com/buggymcbugfix/not-not-a-blog/blob/master/2020-08-17-array.md).

## Running the Benchmark

Assuming that `$HC` points to GHC built via the `wip/buggymcbugfix/arrayOf-primop` branch:

    $ cabal run -w$HC --allow-newer
