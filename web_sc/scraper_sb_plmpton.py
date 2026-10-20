import time
import re
from datetime import datetime, timedelta
import pandas as pd
from seleniumbase import Driver

def scrape_sa_sold_properties():
    print("Launching undetected browser for Sold listings...")
    driver = Driver(uc=True, headless=False)
    
    extracted_data = []
    page_num = 1
    max_pages = 80  # Adjust safety limit as needed (approx 25-30 listings per page)
    
    # Calculate cutoff date (6 months ago from today)
    cutoff_date = datetime.now() - timedelta(days=365)
    print(f"Target: Collecting sold properties back to {cutoff_date.strftime('%d %b %Y')}")
    
    try:
        while page_num <= max_pages:
            target_url = f"https://www.realestate.com.au/sold/property-house-townhouse-unit+apartment-villa-in-plympton,+sa+5038/list-{page_num}"
            print(f"\nScraping Page {page_num}: {target_url}")
            driver.get(target_url)
            
            time.sleep(5)
            
            # Scroll down to trigger lazy loading of cards
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(3)
            
            cards = driver.find_elements("css selector", "article, [data-testid*='listing'], .residential-card")
            if not cards:
                print("No more listing cards found on this page. Ending pagination.")
                break
                
            print(f"Found {len(cards)} elements on page {page_num}.")
            page_old_properties_found = 0
            
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
                    
                    # 2. Price (Sold Price)
                    try:
                        price_elem = card.find_element("css selector", "[data-testid='property-price'], .property-price")
                        price = price_elem.text.strip()
                    except Exception:
                        price_match = re.search(r'(\$[\d,]+)', card_text)
                        price = price_match.group(1) if price_match else "Contact Agent"
                    
                    # 3, 4, 5, 6. Rooms, Bathrooms, Car Spaces, and Total Area via aria-labels
                    rooms, bathrooms, car_spaces, total_area = "N/A", "N/A", "N/A", "N/A"
                    
                    try:
                        primary_ul = card.find_element("css selector", "ul.residential-card__primary, ul[aria-label*='bedrooms']")
                        aria_full = primary_ul.get_attribute("aria-label")
                        
                        if aria_full:
                            beds_m = re.search(r'(\d+)\s+bedrooms', aria_full, re.IGNORECASE)
                            baths_m = re.search(r'(\d+)\s+bathrooms', aria_full, re.IGNORECASE)
                            cars_m = re.search(r'(\d+)\s+car\s+spaces?', aria_full, re.IGNORECASE)
                            area_m = re.search(r'([\d,]+\.?\d*\s*(?:m²|sqm|Ha|hectares|acre|acres))', aria_full, re.IGNORECASE)
                            
                            if beds_m: rooms = beds_m.group(1)
                            if baths_m: bathrooms = baths_m.group(1)
                            if cars_m: car_spaces = cars_m.group(1)
                            if area_m: total_area = area_m.group(1)
                    except Exception:
                        pass
                    
                    # 7. Sold Date Extraction & Validation
                    date_match = re.search(r'Sold on\s+([0-9]{1,2}\s+[A-Za-z]+\s+[0-9]{4})', card_text)
                    sold_date_str = date_match.group(1) if date_match else "N/A"
                    
                    if sold_date_str != "N/A":
                        try:
                            sold_date_obj = datetime.strptime(sold_date_str, "%d %b %Y")
                            # Check if the property is older than 6 months
                            if sold_date_obj < cutoff_date:
                                page_old_properties_found += 1
                                continue # Skip properties older than 6 months
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
                    
                    if item_dict not in extracted_data and address != "N/A":
                        extracted_data.append(item_dict)
                        
                except Exception:
                    continue
            
            print(f"Collected valid listings so far: {len(extracted_data)}")
            
            # If an entire page or majority of properties are older than 6 months, we can stop paginating further back
            if page_old_properties_found >= 10:
                print("Reached listings older than 6 months. Stopping pagination loop.")
                break
                
            page_num += 1
            
        # Export final dataset to Excel
        if extracted_data:
            df = pd.DataFrame(extracted_data)
            output_file = "sa_sold_plymton_properties.xlsx"
            df.to_excel(output_file, index=False)
            print(f"\nSuccessfully exported {len(df)} records to Excel: {output_file}")
            print(df.head())
        else:
            print("No matching listings found within the last 12 months.")
            
    finally:
        driver.quit()

if __name__ == "__main__":
    scrape_sa_sold_properties()