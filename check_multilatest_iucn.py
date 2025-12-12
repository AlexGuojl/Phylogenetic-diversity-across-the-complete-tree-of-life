import pandas as pd
import math
import requests
import time
import os
import random
import sys
import site
site.main()


iucn_total=pd.read_csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_iucn_data_all.csv")
iucn_true = iucn_total[iucn_total["latest"] == True]

def safe_get_json(url, headers, max_retries=5):
    """

    """
    for attempt in range(1, max_retries + 1):
        try:
            response = requests.get(url, headers=headers, timeout=20)

            if response.status_code == 200:
                return response.json()

            if response.status_code == 429:
                wait = random.uniform(30, 60)
                print(f"⏳ Hit API rate-limit (429). Waiting {wait:.1f}s…")
                time.sleep(wait)
                continue

            print(f"⚠️ API error {response.status_code}: retrying…")
            time.sleep(random.uniform(3, 6))

        except requests.exceptions.RequestException as e:
            print(f"⚠️ Network error: {e}. Retrying…")
            time.sleep(random.uniform(3, 6))

    print(f"❌ Failed to fetch after {max_retries} retries: {url}")
    return None

def get_latest_iucn_assessment(species_id_list, api_token):
    """
    """
    headers = {
        "accept": "application/json",
        "Authorization": api_token
    }

    results = []

    for species_id in species_id_list:
        
        time.sleep(1.0 + random.random())

        url = f"https://api.iucnredlist.org/api/v4/taxa/sis/{species_id}"
        data = safe_get_json(url, headers)
        if data is None:
            continue

        taxon = data.get("taxon", {})
        name = taxon.get("scientific_name", "NA")

        assessments = data.get("assessments", [])

        # find all latest=True 
        latest_items = [a for a in assessments if a.get("latest") is True]

        if not latest_items:
            print(f"⚠ No latest=True for speciesID {species_id}")
            continue

        # if only one, use it
        if len(latest_items) == 1:
            best = latest_items[0]

        else:
            # most recent one
            for a in latest_items:
                try:
                    a["_year"] = int(a.get("year_published", 0))
                except:
                    a["_year"] = 0
            best = max(latest_items, key=lambda x: x["_year"])

        category = best.get("red_list_category_code")

        results.append({
            "speciesID": species_id,
            "name": name,
            "latest": True,
            "category": category
        })

    return results

ids = list(iucn_true["speciesID"])

if __name__ == "__main__":
    API_TOKEN = ""
    results = get_latest_iucn_assessment(ids, API_TOKEN)
    df_realtrue = pd.DataFrame(results)
    df_realtrue.to_csv("real_latest_iucn.csv")
    
