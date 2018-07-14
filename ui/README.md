Setup Flow
--

1. Register product id's to listen for 
1. Add an account 
    * secret/passphrase/key
    * account nickname
1. Add trade engine to account
    * choose product
    * set amount of base currency (pool)
    * set trade window (size of timeframe to trade against)
    * set channel properties (Buy/Sell channels)
      * position relative to median in std deviation
      * "thickness" in std deviation
      * stop loss logic
        * ?? elaborate this
        * stop trigger deviation (relative to size, above for sell channel below for buy)
	    * stop position deviation (when triggered the price the stop order will be placed)
		* re-entry trigger? (how do get out of long/short mode?)
      * maximum active orders in channel
      * set the order size multiplier (goes off of the quote size of product)	
1. Turn engine either on/off (default to off)

Trade Flow
--

**All objects working with public or private endpoints have to request for time slots
that a valid request can be made through a scheduler. This will ensure that maximum requests/sec will be
followed fairly rigidly. There could be some overlap if a request fails and 
has to be tried again, but will probably not be a big deal with 1-3 trade engines 
per account.**

1. Account contains one or more engines and will handle creating them and will
register the trade engine with ticker collector and unregister when engines are removed
1. On a tick all running trade engines with the product id, will be forwarded info
1. every tick a trade  engine receives the following logic loop will occur
  1. check if price should have triggered any pending orders (buy/sell)
    * for buy orders look at highest buy price and if price is less than, check for order completion
    * on order completion record proper ledger records
    * recurse until no orders left, or highest order price is no longer greater than tick price
  2. check if ticker price is within channel and channel has a "free slot" (not over max orders)
    * divide the channel size by the maximum number of orders to find free slots
    * if a free slot is found attempt to make an order (if failure just move on)
      * on succesful order placement record a debit for on-hand and a credit for pending
      * put the order in the pending bucket to be checked on subsequent loops
      * notify any listeners of the pending amounts and order information
  3. check if ticker price triggers any stop logic
    * if stop logic is triggered clear all orders (buy and sell)
      * query order status (filled amount etc...) and attempt to cancel
      * add proper credit to on-hand ({order size} - {fill size} - {fee})
      * add proper debit to pending abs({fill size})
    * once we guarantee no orders exist and ledgers are accurate determine the type
of stop that needs to be placed
      * if ticker price is greater/less than we make a market order instead
and bypass "stop watch" straight to long/short mode
      * if stop order fails, make a market order until sucessful to guarantee order is placed
        * loop until order is marked as done
        * record proper ledger entries for on-hand
        * skip "stop watch" altogether and go into long/short mode
    * put engine in a "stop watch" mode
  4. check if an engine stop request has been made
    * clear orders/update ledgers/notify listeners

Stop Mode
--