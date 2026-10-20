import pandas as pd
import re
import time
from playwright.sync_api import sync_playwright

def scrape_sa_real_estate():
    print("Initializing Playwright with stealth settings...")
    with sync_playwright() as p:
        # Launch browser with arguments to hide automation footprints
        browser = p.chromium.launch(
            headless=False,
            args=[
                "--disable-blink-features=AutomationControlled",
                "--start-maximized"
            ]
        )
        
        # Create context with realistic user agent and viewport
        context = browser.new_context(
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
            viewport={"width": 1920, "height": 1080},
            device_scale_factor=1,
        )
        
        page = context.new_page()
        
        # Target Sold URL (or change to buy URL if preferred)
        target_url = "https://www.realestate.com.au/sold/in-sa/list-1"
        print(f"Navigating to {target_url}...")
        
        # Use domcontentloaded or load
        page.goto(target_url, timeout=60000, wait_until="domcontentloaded")
        
        print("\n>>> CHECK THE BROWSER WINDOW NOW. <<<")
        print("If a verification or blank loading screen appears, solve it in the browser.")
        input("Once you can see the property listings loaded on your screen, press Enter here... ")
        
        # Scroll down to load lazy elements
        page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
        time.sleep(3)
        
        listing_cards = page.locator("article[data-testid='property-card'], article.property-card").all()
        print(f"Found {len(listing_cards)} property listings.")
        
        extracted_data = []
        
        for card in listing_cards:
            try:
                # 1. Address
                address_elem = card.locator("[data-testid='property-address'], .residential-card__address-heading")
                address = address_elem.inner_text().strip() if address_elem.count() > 0 else "N/A"
                
                # 2. Postcode (SA 4-digit code starting with 5)
                postcode_match = re.search(r'\b(5\d{3})\b', address)
                postcode = postcode_match.group(1) if postcode_match else "N/A"
                
                # 3. Price
                price_elem = card.locator("[data-testid='property-price'], .property-price")
                price = price_elem.inner_text().strip() if price_elem.count() > 0 else "Contact Agent"
                
                # Features container
                features_elem = card.locator("[data-testid='property-features'], .property-features")
                features_text = features_elem.inner_text() if features_elem.count() > 0 else ""
                
                # 4. Rooms (Bedrooms)
                beds_match = re.search(r'(\d+)\s*(?:bed|Bed)', features_text)
                rooms = beds_match.group(1) if beds_match else "N/A"
                
                # 5. Bathrooms
                baths_match = re.search(r'(\d+)\s*(?:bath|Bath)', features_text)
                baths = baths_match.group(1) if baths_match else "N/A"
                
                # 6. Car Parking Space
                cars_match = re.search(r'(\d+)\s*(?:parking|car|Car|garage)', features_text, re.IGNORECASE)
                cars = cars_match.group(1) if cars_match else "N/A"
                
                # 7. Total Area
                area_match = re.search(r'([\d,]+\.?\d*\s*(?:m²|sqm|ha|acre|acres))', features_text, re.IGNORECASE)
                total_area = area_match.group(1) if area_match else "N/A"
                
                extracted_data.append({
                    "Address": address,
                    "Postcode": postcode,
                    "Price": price,
                    "No. of Rooms": rooms,
                    "No. of Bathrooms": baths,
                    "No. of Car Parking Space": cars,
                    "Total Area": total_area
                })
            except Exception:
                continue
                
        browser.close()
        
    if extracted_data:
        df = pd.DataFrame(extracted_data)
         output_file = "C:/Project_R/Real_estate/real_estate-1/webscrape/sa_sold_properties_data.xlsx"
        df.to_excel(output_file, index=False)
        print(f"Data successfully saved to {output_file}!")
        print(df.head())
    else:
        print("No data extracted because the page content was blocked or empty.")

if __name__ == "__main__":
    scrape_sa_real_estate()