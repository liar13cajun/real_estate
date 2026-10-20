from seleniumbase import Driver
import time

# Initialize an undetected browser instance
driver = Driver(uc=True, headless=False)

try:
    print("Navigating to realestate.com.au...")
    driver.get("https://www.realestate.com.au/buy/in-sa/list-1")

    # Give it time to load or handle any manual click if needed
    time.sleep(10)

    print("Page title:", driver.title)
    # You can extract elements here using driver.find_elements(...)

finally:
    driver.quit()