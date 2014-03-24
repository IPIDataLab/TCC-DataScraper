import csv
import json

# read in csv
tcc = open('tcc_files/Data.TCC.csv','r')
reader=csv.reader(tcc)

# create json array
tcc_json = []

date = 'date'

for row in reader:
	#check for header row
	if row[0] =='date':
		pass
	else:
		#check for new date
		if row[0] == date:
			pass
		else:
			#add new date object into array
			tcc_json.append({})
			# formate date
			if row[0][5] == "0":
				date_obj = row[0][6]+"/"+row[0][2:4]
			else:
				date_obj = row[0][5:7]+"/"+row[0][2:4]
			tcc_json[-1]["date"] = date_obj
			tcc_json[-1]["countries"] = {}
			date = row[0]
		insert = {}
		insert['name'] = row[1]
		insert['region'] = row[6]
		insert['continent'] = row[5]
		insert['contributions'] = {}
		insert['contributions']['num_missions'] = int(row[17])
		if row[18] == "NA":
			pass
		else:
			insert['contributions']['num_troops'] = int(row[18])
		if row[24] == "NA":
			pass
		else:
			insert['contributions']['num_pol'] = int(row[22])
		if row[26] == "NA":
			pass
		else:
			insert['contributions']['num_milobvs'] = int(row[26])
		if row[30] == "NA":
			pass
		else:
			insert['contributions']['num_tot'] = int(row[30])
		tcc_json[-1]["countries"][row[2]] = insert

		

tcc_output = open("tcc_files/TCC.json", "w")
# magic happens here to make it pretty-printed
tcc_output.write(json.dumps(tcc_json, indent=4, separators=(',', ':')))
tcc_output.close()