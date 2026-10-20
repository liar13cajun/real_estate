import time
import re
import pandas as pd
from seleniumbase import Driver

def scrape_sa_sold_properties():
    print("Launching undetected browser for Sold listings...")
    driver = Driver(uc=True, headless=False)
    
    try:
        target_url = "https://www.realestate.com.au/sold/in-sa/list-1"
        print(f"Navigating to {target_url}...")
        driver.get(target_url)
        
        # Allow time to render
        time.sleep(6)
        
        # Scroll down to trigger lazy loading
        print("Scrolling page to load sold listings...")
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(3)
        
        cards = driver.find_elements("css selector", "article, [data-testid*='listing'], .residential-card")
        print(f"Discovered {len(cards)} potential elements. Filtering sold properties...")
        
        extracted_data = []
        
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
                
                # 3, 4, 5, 6. Rooms, Bathrooms, Car Spaces, and Total Area using the precise HTML aria-labels
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
                
                # Fallback to individual <li> inspection if needed
                if rooms == "N/A" or bathrooms == "N/A" or car_spaces == "N/A" or total_area == "N/A":
                    try:
                        li_elems = card.find_elements("css selector", "li[aria-label]")
                        for li in li_elems:
                            lbl = li.get_attribute("aria-label").lower()
                            val_elem = li.find_element("css selector", "p")
                            val = val_elem.text.strip() if val_elem else ""
                            
                            if "bed" in lbl:
                                rooms = val
                            elif "bath" in lbl:
                                bathrooms = val
                            elif "car" in lbl or "parking" in lbl:
                                car_spaces = val
                            elif "land size" in lbl or "m²" in lbl or "ha" in lbl:
                                total_area = val
                    except Exception:
                        pass
                
                # 7. Sold Date
                date_match = re.search(r'Sold on\s+([0-9]{1,2}\s+[A-Za-z]+\s+[0-9]{4})', card_text)
                sold_date = date_match.group(1) if date_match else "N/A"
                
                item_dict = {
                    "Address": address,
                    "Price": price,
                    "No. of Rooms": rooms,
                    "No. of Bathrooms": bathrooms,
                    "No. of Car Parking Space": car_spaces,
                    "Total Area": total_area,
                    "Sold Date": sold_date
                }
                
                if item_dict not in extracted_data and address != "N/A":
                    extracted_data.append(item_dict)
                
            except Exception:
                continue
                
        print(f"Successfully parsed {len(extracted_data)} unique sold property listings.")
        
        if extracted_data:
            df = pd.DataFrame(extracted_data)
            output_file = "sa_sold_properties_data.xlsx"
            df.to_excel(output_file, index=False)
            print(f"Data successfully exported to Excel: {output_file}")
            print(df.head())
        else:
            print("No sold listings could be parsed.")
            
    finally:
        driver.quit()

if __name__ == "__main__":
    scrape_sa_sold_properties()