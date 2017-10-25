import pyodbc
import csv

years = list(range(1977, 2018))
filenames = [(str(year) + "_Production_Database.accdb", str(year)) for year in years]

tables = ["CaliforniaOilandGasWells", "CaliforniaOilandGasWellInjection"]

for filename, year in filenames:

	for table in tables:

		# MS ACCESS DB CONNECTION
		pyodbc.lowercase = False
		conn_str = "DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};" + "DBQ=ftp_data/injection_data/{};".format(filename)
		conn = pyodbc.connect(conn_str)

		# OPEN CURSOR AND EXECUTE SQL
		cur = conn.cursor()
		sql_str = "SELECT * FROM {}{}".format(year, table)
		cur.execute(sql_str)

		colnames = list(cur.description)
		colnames = [name[0] for name in colnames]

		# OPEN CSV AND ITERATE THROUGH RESULTS
		if table == "CaliforniaOilandGasWells":
			out_str = "ftp_data/injection_data/csvs/" + year + "general.csv"
		else:
			out_str = "ftp_data/injection_data/csvs/" + year + "injections.csv"
		print(out_str)
		with open(out_str, 'w', newline='') as f:
		    writer = csv.writer(f)
		    writer.writerow(colnames)
		    for row in cur.fetchall():
		        writer.writerow(row)

		cur.close()
		conn.close()