import os
from ftplib import FTP 

ftp = FTP("ftp.consrv.ca.gov")
ftp.login()
ftp.cwd("pub/oil/new_database_format")

print("File List:")
files = ftp.nlst()
print(files)

for file in files:
	gFile = open("ftp_data/" + file, "wb")
	ftp.retrbinary("RETR " + file, gFile.write)
	gFile.close()

ftp.quit()



# ftp.retrlines("LIST")

# ftp.cwd("pub/oil/new_database_format")

# listing = []
# ftp.retrlines("LIST", listing.append)
# words = listing[0].split(None, 8)
# filename = words[-1].lstrip()

# local_filename = os.path.join("~/documents/stat_157/induced_seismicity/ftp_data/", filename)
# lf = open(local_filename, "wb")
# ftp.retrbinary("RETR " + filename, lf.write, 8*1024)
# lf.close()