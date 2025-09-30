# This script downloads the raw data from its permanent Zenodo archive.

# The DOI link from your Zenodo upload.
# NOTE: You need the link for the actual ZIP file, not the main record page.
# You can get this by right-clicking the "Download" button on Zenodo and "Copy Link Address".
file_url <- "https://zenodo.org/records/1234567/files/ba_project_raw_data.zip"

# The destination for the downloaded zip file.
zip_destination <- "data/raw/ba_project_raw_data.zip"

# The directory to unzip the contents into.
unzip_destination <- "data/raw/"

# Download the file.
download.file(url = file_url, destfile = zip_destination)

# Unzip the file.
unzip(zipfile = zip_destination, exdir = unzip_destination)

# Optional: Clean up by deleting the zip file after unzipping.
file.remove(zip_destination)

print("Raw data downloaded and unzipped successfully.")