from urllib.parse import urlencode
import json
import httpx
import pandas as pd

# we should use browser-like request headers to prevent being instantly blocked
BASE_HEADERS = {
    "accept-language": "en-US,en;q=0.9",
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36",
    "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "accept-language": "en-US;en;q=0.9",
    "accept-encoding": "gzip, deflate, br",
}

def searchZillow(searchTerm="colorado",mapBounds=[-105,39,-104,39.75],price=[250000,500000],beds=[1,5],baths=[1,5]):
  url = "https://www.zillow.com/search/GetSearchPageState.htm?"
  parameters = {
      "searchQueryState": {
          "pagination": {},
          "usersSearchTerm": "colorado",
          # map coordinates that indicate New Haven city's area
          "mapBounds": {
              "west": mapBounds[0],
              "east": mapBounds[2],
              "south": mapBounds[1],
              "north": mapBounds[3],
          },
          "filterState": {
              "price": {"min": price[0],"max": price[1]},
              "beds": {"min": beds[0],"max": beds[1]},
              "baths": {"min": baths[0],"max": baths[1]},
              "ah": {"value": True},
              "con": {"value": False},
              "mf": {"value": False},
              "manu": {"value": False},
              "land": {"value": False},
              "apa": {"value": False},
              "apco": {"value": False}
          },
      },
      "wants": {
          "cat1": ["listResults", "mapResults"], "cat2": ["total"]
      },
      "requestId": 2,
  }
  response = httpx.get(url + urlencode(parameters), headers=BASE_HEADERS)
  # assert response.status_code == 200, "request has been blocked"
  data = response.json()
  results = response.json()["cat1"]["searchResults"]["mapResults"]
  # print(json.dumps(results, indent=2))
  # print(f"found {len(results)} property results")
  results_df = pd.DataFrame.from_records(results)
  varNames = [key for key in results_df.keys()]
  results_df = pd.DataFrame.to_numpy(results_df)
  return [results_df,varNames]
