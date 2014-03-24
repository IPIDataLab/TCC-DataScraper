import requests
import time, calendar
import sqlite3
import json, csv

# Set up SQLlite params
conn = sqlite3.connect(":memory:")
conn.text_factory = str
curr = conn.cursor()

# Create query param based of current date
year = time.strftime("%Y")
prev_mon = int(time.strftime("%m")) - 1
prev_day = calendar.monthrange(int(year), prev_mon)[1]

query_date = year + str(prev_mon).zfill(2) + str(prev_day)
out_date = year + '-' + str(prev_mon).zfill(2) + '-' + str(prev_day)

# Custom HTTP header param of api key for morph
headers = {'x-api-key': 'vyhjk1ORXQTfwjY3vytr'}
# Requests http request collating custom header and query date
r = requests.get('https://api.morph.io/byndcivilization/TCCScraper/data.json?query=select%20*%20from%20data%20where%20date%3D' + query_date, headers=headers)

# Format data as json
new_data = r.json()

# Create SQLlite DB to query and reformat
curr.execute('DROP TABLE IF EXISTS swdata')
curr.execute('CREATE TABLE swdata(F INT,M INT,dateString VARCHAR,mission VARCHAR,tccIso3Alpha VARCHAR,T,date DATE,tccIso3Num VARCHAR,type VARCHAR,tcc VARCHAR)')

# Populate with incoming json data
key_list = ('F','M','dateString','mission','tccIso3Alpha','T','date','tccIso3Num','type','tcc')
for row in new_data:
	curr.execute('INSERT INTO swdata(F,M,dateString,mission,tccIso3Alpha,T,date,tccIso3Num,type,tcc) VALUES (?,?,?,?,?,?,?,?,?,?);', tuple(row[k] for k in key_list))

# Massage data
curr.execute("ALTER TABLE swdata RENAME TO swdata_tmp")

curr.execute("CREATE TABLE swdata (F INTEGER, tccIso3Alpha TEXT, M INTEGER, mission TEXT, T INTEGER, date INTEGER, type TEXT, tcc TEXT, tccIso3Num TEXT, dateString TEXT)")
curr.execute("INSERT INTO swdata(F, tccIso3Alpha, M, mission, T, date, type, tcc, tccIso3Num, dateString) SELECT F, TCCISO3ALPHA, M, MISSION, T, DATE, TYPE, TCC, TCCISO3NUM, DATESTRING FROM swdata_tmp")
curr.execute("DROP TABLE swdata_tmp")

# Disaggregate each type of contribution into a separate table
curr.execute("CREATE TABLE eom AS SELECT date, dateString, tcc, tccIso3Alpha, tccIso3Num, mission, type, M AS eom_M, F AS eom_F, T AS eom_T FROM `swdata` WHERE type = 'Experts on Mission'")
curr.execute("CREATE TABLE troops AS SELECT date, dateString, tcc, tccIso3Alpha, tccIso3Num, mission, type, M AS troops_M, F AS troops_F, T AS troops_T FROM `swdata` WHERE type = 'Contingent Troop'")
curr.execute("CREATE TABLE fpu AS SELECT date, dateString, tcc, tccIso3Alpha, tccIso3Num, mission, type, M AS fpu_M, F AS fpu_F, T AS fpu_T FROM `swdata` WHERE type = 'Formed Police Units'")
curr.execute("CREATE TABLE ip AS SELECT date, dateString, tcc, tccIso3Alpha, tccIso3Num, mission, type, M AS ip_M, F AS ip_F, T AS ip_T FROM `swdata` WHERE type = 'Individual Police'")

# Full join of eom and troop tables
curr.execute("CREATE TABLE eom_troops AS SELECT eom.date, eom.dateString, eom.tcc, eom.tccIso3Alpha, eom.tccIso3Num, eom.mission, eom.eom_M, eom.eom_F, eom.eom_T, troops.troops_M, troops.troops_F, troops.troops_T FROM eom INNER JOIN troops ON eom.date = troops.date AND eom.tcc = troops.tcc AND eom.mission = troops.mission UNION ALL SELECT eom.date, eom.dateString, eom.tcc, eom.tccIso3Alpha, eom.tccIso3Num, eom.mission, eom.eom_M, eom.eom_F, eom.eom_T, CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER) FROM eom WHERE NOT EXISTS (SELECT * FROM troops WHERE eom.date = troops.date AND eom.tcc = troops.tcc AND eom.mission = troops.mission) UNION ALL SELECT troops.date, troops.dateString, troops.tcc, troops.tccIso3Alpha, troops.tccIso3Num, troops.mission, CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), troops.troops_M, troops.troops_F, troops.troops_T FROM troops WHERE NOT EXISTS (SELECT * FROM eom WHERE eom.date = troops.date AND eom.tcc = troops.tcc AND eom.mission = troops.mission)")

# Full join of fpu and ip tables
curr.execute("CREATE TABLE fpu_ip AS SELECT fpu.date, fpu.dateString, fpu.tcc, fpu.tccIso3Alpha, fpu.tccIso3Num, fpu.mission, fpu.fpu_M, fpu.fpu_F, fpu.fpu_T, ip.ip_M, ip.ip_F, ip.ip_T FROM fpu INNER JOIN ip ON fpu.date = ip.date AND fpu.tcc = ip.tcc AND fpu.mission = ip.mission UNION ALL SELECT fpu.date, fpu.dateString, fpu.tcc, fpu.tccIso3Alpha, fpu.tccIso3Num, fpu.mission, fpu.fpu_M, fpu.fpu_F, fpu.fpu_T, CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER) FROM fpu WHERE NOT EXISTS (SELECT * FROM ip WHERE fpu.date = ip.date AND fpu.tcc = ip.tcc AND fpu.mission = ip.mission) UNION ALL SELECT ip.date, ip.dateString, ip.tcc, ip.tccIso3Alpha, ip.tccIso3Num, ip.mission, CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), ip.ip_M, ip.ip_F, ip.ip_T FROM ip WHERE NOT EXISTS (SELECT * FROM fpu WHERE fpu.date = ip.date AND fpu.tcc = ip.tcc AND fpu.mission = ip.mission)")

# Full join of joined tables
curr.execute("CREATE TABLE eom_troops_fpu_ip AS SELECT eom_troops.date, eom_troops.dateString, eom_troops.tcc, eom_troops.tccIso3Alpha, eom_troops.tccIso3Num, eom_troops.mission, eom_troops.eom_M, eom_troops.eom_F, eom_troops.eom_T, eom_troops.troops_M, eom_troops.troops_F, eom_troops.troops_T, fpu_ip.fpu_M, fpu_ip.fpu_F, fpu_ip.fpu_T, fpu_ip.ip_M, fpu_ip.ip_F, fpu_ip.ip_T FROM eom_troops INNER JOIN fpu_ip ON eom_troops.date = fpu_ip.date AND eom_troops.tcc = fpu_ip.tcc AND eom_troops.mission = fpu_ip.mission UNION ALL  SELECT eom_troops.date, eom_troops.dateString, eom_troops.tcc, eom_troops.tccIso3Alpha, eom_troops.tccIso3Num, eom_troops.mission, eom_troops.eom_M, eom_troops.eom_F, eom_troops.eom_T, eom_troops.troops_M, eom_troops.troops_F, eom_troops.troops_T, CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER) FROM eom_troops WHERE NOT EXISTS (SELECT * FROM fpu_ip WHERE eom_troops.date = fpu_ip.date AND eom_troops.tcc = fpu_ip.tcc AND eom_troops.mission = fpu_ip.mission) UNION ALL SELECT fpu_ip.date, fpu_ip.dateString, fpu_ip.tcc, fpu_ip.tccIso3Alpha, fpu_ip.tccIso3Num, fpu_ip.mission, CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), CAST(NULL AS INTEGER), fpu_ip.fpu_M, fpu_ip.fpu_F, fpu_ip.fpu_T, fpu_ip.ip_M, fpu_ip.ip_F, fpu_ip.ip_T FROM fpu_ip WHERE NOT EXISTS ( SELECT * FROM eom_troops WHERE eom_troops.date = fpu_ip.date AND eom_troops.tcc = fpu_ip.tcc AND eom_troops.mission = fpu_ip.mission)")

#Write out tables into csv file
data = curr.execute('SELECT * FROM eom_troops_fpu_ip')

gender_data = data.fetchall()

sql_load = open('2-sql_gender_load.txt','w')
sql_load.write("SELECT setval('gender_id_seq', (SELECT MAX(id) from \"gender\"));" + "\n")
sql_load.write("INSERT INTO gender" + "\n")
sql_load.write("(date, date_string,tcc,tcc_iso_3_alpha,tcc_iso_3_num,mission,ip_M,ip_F,ip_T,fpu_M,fpu_F,fpu_T,eom_M,eom_F,eom_T,troops_M,troops_F,troops_T,total_M,total_F,total_T)" + "\n")
sql_load.write("VALUES" + "\n")

gender_load = []

for row in gender_data:
	row_format = []
	row_format.append("'" + str(row[0]) + "'")
	row_format.append("'" + row[1] + "'")
	if row[2] == 'Tanzania, United Republic of':
		row_format.append("'" + "Tanzania" + "'")
	elif row[2] == 'Moldova, Republic of':
		row_format.append("'" + "Moldova" + "'")
	elif row[2] == 'The former Yugoslav Republic of Macedonia':
		row_format.append("'" + "Macedonia" + "'")
	else:
		row_format.append("'" + row[2] + "'")
	row_format.append("'" + row[3] + "'")
	row_format.append("'" + row[4] + "'")
	row_format.append("'" + row[5] + "'")
	if row[15] == None:
		ip_M = 0
	else:
		ip_M = row[15]
	
	if row[16] == None:
		ip_F = 0
	else:
		ip_F = row[16]

	if row[17] == None:
		ip_T = 0
	else:
		ip_T = row[17]

	if row[12] == None:
		fpu_M = 0
	else:
		fpu_M = row[12]
	
	if row[13] == None:
		fpu_F = 0
	else:
		fpu_F = row[13]

	if row[14] == None:
		fpu_T = 0
	else:
		fpu_T = row[14]

	if row[6] == None:
		eom_M = 0
	else:
		eom_M = row[6]
	
	if row[7] == None:
		eom_F = 0
	else:
		eom_F = row[7]

	if row[8] == None:
		eom_T = 0
	else:
		eom_T = row[8]

	if row[9] == None:
		troops_M = 0
	else:
		troops_M = row[9]
	
	if row[10] == None:
		troops_F = 0
	else:
		troops_F = row[10]

	if row[11] == None:
		troops_T = 0
	else:
		troops_T = row[11]

	total_M = ip_M + fpu_M + eom_M + troops_M
	total_F = ip_F + fpu_F + eom_F + troops_F
	total_T = ip_T + fpu_T + eom_T + troops_T

	row_format.append(str(ip_M))
	row_format.append(str(ip_F))
	row_format.append(str(ip_T))
	row_format.append(str(fpu_M))
	row_format.append(str(fpu_F))
	row_format.append(str(fpu_T))
	row_format.append(str(eom_M))
	row_format.append(str(eom_F))
	row_format.append(str(eom_T))
	row_format.append(str(troops_M))
	row_format.append(str(troops_F))
	row_format.append(str(troops_T))
	row_format.append(str(total_M))
	row_format.append(str(total_F))
	row_format.append(str(total_T))

	gender_load.append(row_format)

for row in gender_load:
	sql_load.write("(")
	for i in range(len(row)):
		entry = row[i]

		if i < 20:
			sql_load.write(entry + ",")
		else:
			sql_load.write(entry)
	sql_load.write(")," + "\n")