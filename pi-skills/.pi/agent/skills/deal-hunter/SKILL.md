---
name: deal-hunter
description: Find the best deals on products across German marketplaces. Searches used, refurbished, and new listings, compares prices across sellers, and surfaces the best value options. Defaults to Germany as delivery country. Use when asked to "find deals", "best price", "where to buy", "cheapest", "compare prices", "find the best deal for", "search for", "price comparison", "used", "refurbished", or "günstig".
---

# Deal Hunter

Find the best deals on products across German marketplaces. Defaults to Germany as delivery country.

## Step 1: Parse the Request

Extract from the user's query:
- **Product name** — the item they want (e.g., "ThinkPad T14", "iPhone 15", "Sony WH-1000XM5")
- **Budget** — max price if mentioned (e.g., "under 500€", "max 300")
- **Condition preference** — new, used, refurbished, or any (default: any)
- **Country** — delivery country (default: Germany if not specified)
- **Specifics** — model year, specs, color, storage size, etc.

If the user is vague ("laptop under 400€"), ask 1–2 clarifying questions before searching.

## Step 2: Search Marketplaces

Search across relevant marketplaces for the product. Use parallel web_search calls where possible.

Read `${CLAUDE_SKILL_ROOT}/references/marketplaces.md` for the full marketplace list and search strategies.

**Search strategy:**
1. Search Idealo/Geizhals for price baseline (new items)
2. Search eBay.de KleinAnzeigen for used/private deals
3. Search trusted refurbishers (AfB, refurbed.de, Back Market) for refurbished deals
4. Search Amazon.de for new + warehouse deals

**Query construction:**
- Include the product name, condition ("gebraucht", "refurbished", "generalüberholt"), and site filter
- Example: `"ThinkPad T14 Gen 2" gebraucht site:ebay.de`
- Example: `"Sony WH-1000XM5" refurbished site:refurbed.de`

## Step 3: Fetch and Compare Listings

For each promising listing found:
1. Fetch the page to get exact price, condition, specs, seller info
2. Extract: price (incl. shipping), condition, seller rating/trust, warranty, delivery time
3. Check if shipping to Germany is available (if country ≠ Germany)

**Skip listings that:**
- Are scams (too cheap, vague description, no photos)
- Have no shipping to Germany
- Are from sellers with <95% positive feedback (eBay) or no history (KleinAnzeigen)
- Are "defekt" or "Bastler" unless the user explicitly wants broken items

## Step 4: Evaluate Deals

Score each deal using the criteria in `${CLAUDE_SKILL_ROOT}/references/evaluation-criteria.md`.

**Ranking factors (weighted):**
1. **Price vs. new** — bigger discount = better (40%+ is excellent)
2. **Condition accuracy** — "very good" with photos > "good" with no photos
3. **Seller trust** — commercial refurbisher > private seller with history > unknown
4. **Warranty** — 12+ months > 6 months > none
5. **Shipping cost** — free shipping > reasonable > excessive
6. **Total cost** — price + shipping, in EUR

## Step 5: Present Findings

Return a concise comparison table, then detailed listings.

**Comparison table:**

| # | Seller | Condition | Price | Shipping | Total | Warranty | Why |
|---|--------|-----------|-------|----------|-------|----------|-----|
| 1 | AfB Shop | Very Good | €349 | Free | **€349** | 12mo | Best value, non-profit |
| 2 | eBay (private) | Good | €299 | €15 | **€314** | None | Cheapest, but no warranty |
| 3 | Amazon (new) | New | €499 | Free | **€499** | 2yr | Safe fallback |

**Detailed listing format:**

```
🏆 #1: [Seller Name] — €[Total]
   Link: [URL]
   Condition: [grade]
   Specs: [key specs]
   Seller: [type + rating]
   Warranty: [duration]
   Shipping: [cost + time]
   ⚠️ Note: [any caveat]
```

**Always include:**
- Direct links to each listing
- Total cost (price + shipping)
- One-line verdict on each option
- A "safe pick" recommendation (best balance of price + trust)

## Step 6: Follow-Up

If the user wants to buy one:
1. Verify the listing is still active (re-fetch the page)
2. Check for coupon codes or cashback options
3. Warn about any red flags specific to that seller/listing

## See Also

- `${CLAUDE_SKILL_ROOT}/references/marketplaces.md` — full marketplace list with search tips
- `${CLAUDE_SKILL_ROOT}/references/evaluation-criteria.md` — deal scoring rubric
