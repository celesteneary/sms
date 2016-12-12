import binascii
# After running this script, must still open the file
# And add "memory_initialization_radix=16;" on top and a semicolon
# At the bottom of the file
filename = 'transbot.sms'
with open(filename, 'rb') as f:
    content = f.read()
x = binascii.hexlify(content)
i = 0
charCount = 0
print len(x)/2
while i < len(x):
	if charCount == 2:
		x = x[0:i] + " " + x[i:]
		charCount = 0
	elif x[i] != " ":
		charCount = charCount + 1
        i = i + 1
filename = open('transbot.coe', 'w')
filename.write(x)
