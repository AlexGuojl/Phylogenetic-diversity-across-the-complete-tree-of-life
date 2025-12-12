import pandas as pd
import math
import requests
import time
import os
import random
import sys
import site
site.main()



df_leaves = pd.read_csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/updated_ordered_leaves_3.0.csv", low_memory=False)


iucn_ids = list(df_leaves["iucn"])
cleaned_iucn_ids = []
for item in iucn_ids:
    if item is None or (isinstance(item, float) and math.isnan(item)):
        continue
    parts = str(item).split("|")
    cleaned_iucn_ids.extend(parts)

updated_iucn = pd.read_csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_iucn_data.csv")




ls_alreadyhave = [str(x) for x in list(updated_iucn["speciesID"])]
speciesnostatus = [x for x in cleaned_iucn_ids if x not in ls_alreadyhave]#this iucnid is incorrect
df_speciesnoiucn = df_leaves[df_leaves["iucn"] == '158886']
df_speciesnoiucn=df_speciesnoiucn[["name","id","iucn"]]
#I searched the iucn website and it returns VU for this species
new_row = pd.DataFrame([{
    "speciesID": "158886",
    "name": "Mesamphiagrion ovigerum",
    "latest": True,
    "category": "VU"
}])

updated_iucn = pd.concat([updated_iucn, new_row], ignore_index=True)

#synonyms
sub_df = df_leaves.groupby('iucn').filter(lambda x: len(x) > 1)
sub_df1 = sub_df[["name","id","iucn"]]


nolatest = updated_iucn[updated_iucn["latest"] == False]






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


def get_latest_iucn_assessment(
        species_id_list,
        api_token,
        batch_size=50,
        output_file="/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_iucn_data4.csv"):
    
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

        assessments = data.get("assessments", [])
        if not assessments:
            print(f"⚠️ No assessments for {species_id}, skipping...")
            continue

        
        latest_assessment = next((a for a in assessments if a.get("latest") is True), None)

        if latest_assessment is None:
            print(f"⚠️ No latest=True assessment for {species_id}, skipping...")
            continue

        
        row = {
            "speciesID": species_id,
            "name": data["taxon"]["scientific_name"],
            "latest": True,
            "category": latest_assessment.get("red_list_category_code")
        }

        table.append(row)

      
        time.sleep(0.5 + random.random())

        
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




SPECIES_ID = list(nolatest["speciesID"])

if __name__ == "__main__":
    API_TOKEN = ""
    table = get_latest_iucn_assessment(SPECIES_ID, API_TOKEN)

    #iucn_dataset4.csv
#now: get latest if have
    #else: choose assessment[0]

iucn_already_have4 = pd.read_csv("/Users/alexgjl/Desktop/third_submission_to_nc/updated_fields/latest_iucn_data4.csv")
latest = updated_iucnp[updated_iucn["latest"] == True]
latest_iucn = pd.concat([latest,iucn_already_have4],axis = 0)

species_no_latst_estimation = nolatest[~nolatest["speciesID"].isin(iucn_already_have4["speciesID"])]
iucn_total = pd.concat([latest_iucn,species_no_latst_estimation],axis = 0)
iucn_total.to_csv("latest_iucn_data_all.csv")

#check if any species have more than one "true latest estimate"







