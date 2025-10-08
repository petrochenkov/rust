from collections import defaultdict

sums1 = defaultdict(lambda: 0)
sums2 = defaultdict(lambda: 0)

with open("lal.txt", "r", encoding="utf-8") as f:
    for line in f:
        line = line.strip()
        name, count1, count2 = line.split()
        sums1[name] += int(count1)
        sums2[name] += int(count2)

vec = []
for name, count in sums1.items():
    count2 = sums2[name]
    if count2 == 0:
        continue
    ratio = int(count / count2)
    vec.append((name, count, count2, ratio))

vec.sort(key=lambda x: x[2], reverse=True)

for name, count, count2, ratio in vec:
    print(name, " ", count, " ", count2, ratio)
