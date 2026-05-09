# Installation Guide

## Requirements

To build and run the project, you should have:

- OCaml with an OPAM switch
- Dune 3.21 or newer

- Python 3
- the `yfinance` package

## Clone the Repository

```bash
git clone git@github.coecis.cornell.edu:ak2788/TeamProject3110.git
cd TeamProject3110
```

## OCaml Setup

If you already have a working CS 3110 OPAM switch, activate it and install the
project dependencies:

```bash
eval $(opam env)
opam install dune ounit2 bisect_ppx odoc ocamlformat
```

If you prefer to use a fresh switch for the project, create one first, then run
the same install command above.

## Building the Project

```bash
dune build
```

## Datasets

The trading environment expects CSV files with the columns:

```text
Date,Close
2020-01-02,300.35
2020-01-03,297.43
...
```

CSV files should be placed in the `data/` directory.

The repository already includes sample datasets:

- `data/AAPL_data.csv`
- `data/MSFT_data.csv`
- `data/VZ_data.csv`

### Optional: Fetch New Stock Data

The repo includes `data/fetch_stock_data.py`, which downloads the last 10 years
of daily closing prices for a ticker.

Install the Python dependency:

```bash
python3 -m pip install yfinance
```

Then run:

```bash
python3 data/fetch_stock_data.py AAPL data/AAPL_data.csv
```

You can replace `AAPL` with another ticker and choose a different output file
inside `data/`.

## Running the Program

Run the executable with:

```bash
dune exec teamproject3110
```

The program will prompt you to choose one of two modes:

1. Train a new model
2. Test an existing model

### Train Mode

In train mode, the program:

- asks for a CSV file from `data/`
- evaluates several baseline strategies
- trains a Q-learning trading agent
- prints evaluation metrics
- optionally shows an ASCII PnL plot
- optionally saves the PnL curve to CSV
- optionally saves the learned model in `models/`

Example:

```text
Enter CSV filename (in data/): AAPL_data.csv
```

### Test Mode

In test mode, the program:

- asks for a CSV file from `data/`
- asks for a saved model from `models/`
- evaluates that model greedily on the chosen dataset
- prints metrics
- optionally shows and saves the PnL curve

The repository already includes a couple sample models:

Example inputs:

```text
Enter CSV filename (must be in data/): AAPL_data.csv
Enter model filename (must be in models/): AAPL_trader_1.txt
```

## Running the Tests

```bash
dune test
```

## Helpful Commands

Rebuild from scratch:

```bash
dune clean
dune build
```

Run the test suite after cleaning:

```bash
dune clean
dune test
```
