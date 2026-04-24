# Installation Guide

## Setup

Clone the repository and install dependencies:

```bash
git clone https://github.com/<org>/<repo>.git
cd <repo>
```

---

## Getting a Dataset

You need a CSV file with `Date` and `Close` columns placed in the `data/` directory. There are two ways to get one:

**Option 1 — Use an already included script, or use the included Python script to generate one:**
```bash
python data/fetch_stock_data.py STOCK_TICKER data/STOCK_TICKER_data.csv
```
This will download price data and save a CSV to `data/` automatically.

**Option 2 — Bring your own CSV:**
Create or download a CSV file in the following format and place it in `data/`:
```
Date,Close
2020-01-02,300.35
2020-01-03,297.43
...
```

---

## Running

```bash
dune exec bin/main.exe
```

You will be prompted to enter the filename of your CSV:
```
Enter CSV filename (in data/): AAPL_data.csv
```

---

## Running the Tests

```bash
dune test
```
