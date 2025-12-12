import pandas as pd
import math
import requests
import time
import os
import random

df_leaves = pd.read_csv("/Users/alexgjl/Desktop/master/项目2/文件/updated_ordered_leaves_2.0.csv", low_memory=False)


iucn_ids = list(df_leaves["iucn"])
cleaned_iucn_ids = []
for item in iucn_ids:
    if item is None or (isinstance(item, float) and math.isnan(item)):
        continue
    parts = str(item).split("|")
    cleaned_iucn_ids.extend(parts)


num_ids = [int(x) for x in cleaned_iucn_ids]

# ---------------------
def safe_get_json(url, headers, retries=5):
    for attempt in range(1, retries + 1):
        try:
            response = requests.get(url, headers=headers)
            if response.status_code == 200 and response.headers.get("Content-Type","").startswith("application/json"):
                return response.json()
        except Exception as e:
            print(f"Request exception: {e}")
        
        print(f"⚠ Attempt {attempt} failed, retrying in 2s...")
        time.sleep(2)
    return None


def get_latest_iucn_assessment(species_id_list, api_token, batch_size=50, output_file="/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_iucn_data.csv"):
    headers = {"accept": "application/json", "Authorization": api_token}
    table = []
    total = len(species_id_list)
    
    
    if os.path.exists(output_file):
        df_existing = pd.read_csv(output_file, encoding="gbk")
        done_ids = set(df_existing["speciesID"].astype(int))
    else:
        done_ids = set()
    
    step = max(1, total // 5)
    progress = 1

    for idx, species_id in enumerate(species_id_list, start=1):
        if species_id in done_ids:
            continue  
        
        url = f"https://api.iucnredlist.org/api/v4/taxa/sis/{species_id}"
        data = safe_get_json(url, headers, retries=5)

        if data is None:
            print(f"❌ Failed species {species_id}, skipping...")
            continue

        row = {
            "speciesID": species_id,
            "name": data["taxon"]["scientific_name"],
            "latest": data["assessments"][0]["latest"],
            "category": data["assessments"][0]["red_list_category_code"]
        }
        table.append(row)

        
        time.sleep(1.5+ random.random())

        
        if idx % step == 0 and progress <= 5:
            print(f"{progress}/5 done")
            progress += 1

        
        if idx % batch_size == 0 or idx == total:
            if table:
                df_batch = pd.DataFrame(table)
                if os.path.exists(output_file):
                    df_batch.to_csv(output_file, mode="a", header=False, index=False, encoding="utf-8")
                else:
                    df_batch.to_csv(output_file, index=False, encoding="utf-8")
                table = []

    return True


if __name__ == "__main__":
    API_TOKEN = ""
    get_latest_iucn_assessment(num_ids, API_TOKEN)
