# Efficient Estimation of Bid-Ask Spreads from Open, High, Low, and Close Prices

Implements an efficient estimator of bid-ask spreads from open, high, low, and close prices.

## Installation

Install this package with:

```R
install.packages("bidask")
```

## Usage

Load the library:

```R
library("bidask")
```

Arguments:

```R
edge(open, high, low, close, sign=FALSE)
```

| field   | description                                 |
| ------- | ------------------------------------------- |
| `open`  | Numeric vector of open prices               |
| `high`  | Numeric vector of high prices               |
| `low`   | Numeric vector of low prices                |
| `close` | Numeric vector of close prices              |
| `sign`  | Whether signed estimates should be returned |

The input prices must be sorted in ascending order of the timestamp.

The output value is the spread estimate. A value of 0.01 corresponds to a spread of 1%.

## Example

```R
library("bidask")

df = read.csv("https://raw.githubusercontent.com/eguidotti/bidask/main/pseudocode/ohlc.csv")
edge(df$Open, df$High, df$Low, df$Close)
```

## Cite as

> Ardia, D., Guidotti, E., Kroencke, T.A. (2024). Efficient Estimation of Bid-Ask Spreads from Open, High, Low, and Close Prices. *Journal of Financial Economics*, 161, 103916. [doi: 10.1016/j.jfineco.2024.103916](https://doi.org/10.1016/j.jfineco.2024.103916)

A BibTex  entry for LaTeX users is:

```bibtex
@article{edge,
  title = {Efficient estimation of bid–ask spreads from open, high, low, and close prices},
  journal = {Journal of Financial Economics},
  volume = {161},
  pages = {103916},
  year = {2024},
  doi = {https://doi.org/10.1016/j.jfineco.2024.103916},
  author = {David Ardia and Emanuele Guidotti and Tim A. Kroencke},
}
```

