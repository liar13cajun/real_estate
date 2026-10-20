# scraper - multiple 

import time
import re
from datetime import datetime, timedelta
import pandas as pd
from seleniumbase import Driver

def scrape_suburbs_sold_properties():
    print("Launching undetected browser for Suburb Sold listings...")
    driver = Driver(uc=True, headless=False)
    
    # Target URLs and their respective tab names for Excel
    target_suburbs = {
        "St Marys": "st+marys,+sa+5042",
        "Ascot Park": "ascot+park,+sa+5043",
        "Camden Park": "camden+park,+sa+5038",
        "Edwardstown": "edwardstown,+sa+5039",
        "Mitchell Park": "mitchell+park,+sa+5043",
        "Plympton": "plympton,+sa+5038"
    }
    
    cutoff_date = datetime.now() - timedelta(days=365) # 6 months ago
    print(f"Target: Collecting sold properties back to {cutoff_date.strftime('%d %b %Y')}")
    
    output_file = "sa_suburbs_sold_last_12_months.xlsx"
    
    # Use Pandas ExcelWriter to save each suburb into its own separate tab
    with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
        try:
            for suburb_name, url_slug in target_suburbs.items():
                print(f"\n--- Scraping Suburb: {suburb_name} ---")
                suburb_data = []
                page_num = 1
                max_pages = 10 # Adjust if a suburb has higher volume
                
                while page_num <= max_pages:
                    target_url = f"https://www.realestate.com.au/sold/property-house-townhouse-unit+apartment-villa-in-{url_slug}/list-{page_num}"
                    print(f"Fetching Page {page_num}: {target_url}")
                    driver.get(target_url)
                    
                    time.sleep(5)
                    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
                    time.sleep(3)
                    
                    cards = driver.find_elements("css selector", "article, [data-testid*='listing'], .residential-card")
                    if not cards:
                        print(f"No more listings found for {suburb_name}.")
                        break
                    
                    page_old_properties = 0
                    valid_cards_on_page = 0
                    
                    for card in cards:
                        try:
                            card_text = card.text
                            if not card_text or "sold" not in card_text.lower():
                                continue
                            
                            # 1. Address
                            try:
                                address_elem = card.find_element("css selector", "[data-testid='property-address'], .residential-card__address-heading, h3")
                                address = address_elem.text.strip()
                            except Exception:
                                address = "N/A"
                            
                            # 2. Price
                            try:
                                price_elem = card.find_element("css selector", "[data-testid='property-price'], .property-price")
                                price = price_elem.text.strip()
                            except Exception:
                                price_match = re.search(r'(\$[\d,]+)', card_text)
                                price = price_match.group(1) if price_match else "Contact Agent"
                            
                            # 3, 4, 5, 6. Rooms, Bathrooms, Car Spaces, and Total Area via aria-labels
                            rooms, bathrooms, car_spaces, total_area = "N/A", "N/A", "N/A", "N/A"
                            try:
                                primary_ul = card.find_element("css selector", "ul.residential-card__primary, ul[aria-label*='bedroom']")
                                aria_full = primary_ul.get_attribute("aria-label")
                                
                                if aria_full:
                                    # Updated regex to support both singular and plural (e.g. '1 bedroom' vs '4 bedrooms')
                                    beds_m = re.search(r'(\d+)\s+bedrooms?', aria_full, re.IGNORECASE)
                                    baths_m = re.search(r'(\d+)\s+bathrooms?', aria_full, re.IGNORECASE)
                                    cars_m = re.search(r'(\d+)\s+car\s+spaces?', aria_full, re.IGNORECASE)
                                    area_m = re.search(r'([\d,]+\.?\d*\s*(?:m²|sqm|Ha|hectares|acre|acres))', aria_full, re.IGNORECASE)
                                    
                                    if beds_m: rooms = beds_m.group(1)
                                    if baths_m: bathrooms = baths_m.group(1)
                                    if cars_m: car_spaces = cars_m.group(1)
                                    if area_m: total_area = area_m.group(1)
                            except Exception:
                                pass
                            
                            # Fallback check if main aria-label missed bathrooms
                            if bathrooms == "N/A":
                                baths_fallback = re.search(r'(\d+)\s*(?:bath|baths|bathroom|bathrooms)', card_text, re.IGNORECASE)
                                if baths_fallback:
                                    bathrooms = baths_fallback.group(1)

                            # 7. Sold Date & 6-Month Filter
                            date_match = re.search(r'Sold on\s+([0-9]{1,2}\s+[A-Za-z]+\s+[0-9]{4})', card_text)
                            sold_date_str = date_match.group(1) if date_match else "N/A"
                            
                            if sold_date_str != "N/A":
                                try:
                                    sold_date_obj = datetime.strptime(sold_date_str, "%d %b %Y")
                                    if sold_date_obj < cutoff_date:
                                        page_old_properties += 1
                                        continue # Skip items older than 6 months
                                except ValueError:
                                    pass
                            
                            item_dict = {
                                "Address": address,
                                "Price": price,
                                "No. of Rooms": rooms,
                                "No. of Bathrooms": bathrooms,
                                "No. of Car Parking Space": car_spaces,
                                "Total Area": total_area,
                                "Sold Date": sold_date_str
                            }
                            
                            if item_dict not in suburb_data and address != "N/A":
                                suburb_data.append(item_dict)
                                valid_cards_on_page += 1
                                
                        except Exception:
                            continue
                    
                    print(f"Processed page {page_num} for {suburb_name}. Added {valid_cards_on_page} records.")
                    
                    # Stop pagination for this suburb if properties are older than 6 months
                    if page_old_properties >= 8:
                        print(f"Reached properties older than 6 months for {suburb_name}.")
                        break
                        
                    page_num += 1
                    time.sleep(2)
                
                # Write suburb data to its own Excel tab (Excel sheet names max out at 31 characters)
                sheet_name = suburb_name[:31]
                df_suburb = pd.DataFrame(suburb_data)
                df_suburb.to_excel(writer, sheet_name=sheet_name, index=False)
                print(f"-> Saved {len(df_suburb)} records to tab [{sheet_name}]")
                
        finally:
            driver.quit()
            
    print(f"\nAll suburbs successfully processed and saved to workbook: {output_file}")

if __name__ == "__main__":
    scrape_suburbs_sold_properties()