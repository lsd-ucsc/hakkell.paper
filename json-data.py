import sys
import json

with open(sys.argv[1], mode='r') as fd:
    data = json.load(fd)

print("a,b,mean")
for trial in data:
    a, b = trial["reportName"].split('/')
    mean = trial["reportAnalysis"]["anMean"]["estPoint"]
    print(f'{a},{b},{mean}')
