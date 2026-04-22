import yfinance as yf
import csv
import sys
from datetime import datetime, timedelta
 
 
def fetch_closing_prices(ticker: str, output_file: str = None) -> str:
    """
    Fetch the last 10 years of daily closing prices for a given stock ticker
    and save them to a CSV file.
 
    Args:
        ticker:      Stock ticker symbol (e.g. 'AAPL', 'MSFT')
        output_file: Optional output CSV filename. Defaults to '<TICKER>_closing_prices.csv'
 
    Returns:
        Path to the saved CSV file.
    """
    if output_file is None:
        output_file = f"{ticker.upper()}_closing_prices.csv"
 
    end_date = datetime.today()
    start_date = end_date - timedelta(days=365 * 10)
 
    print(f"Fetching data for {ticker.upper()} from {start_date.date()} to {end_date.date()} ...")
 
    stock = yf.Ticker(ticker)
    hist = stock.history(start=start_date.strftime("%Y-%m-%d"), end=end_date.strftime("%Y-%m-%d"))
 
    if hist.empty:
        raise ValueError(f"No data returned for ticker '{ticker}'. Please check the symbol and try again.")
 
    # Keep only the Close column and reset index so Date becomes a regular column
    close_df = hist[["Close"]].reset_index()
    close_df.columns = ["Date", "Close"]
    close_df["Date"] = close_df["Date"].dt.strftime("%Y-%m-%d")
    close_df["Close"] = close_df["Close"].round(4)
 
    with open(output_file, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["Date", "Close"])
        for _, row in close_df.iterrows():
            writer.writerow([row["Date"], row["Close"]])
 
    print(f"Saved {len(close_df)} rows to '{output_file}'")
    return output_file
 
 
if __name__ == "__main__":
 
    ticker_symbol = sys.argv[1]
    out_file = sys.argv[2] if len(sys.argv) >= 3 else None
 
    fetch_closing_prices(ticker_symbol, out_file)