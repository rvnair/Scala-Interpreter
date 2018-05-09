Plainjane = "Ride with the mob"

for({i = 0} (!(i == 2)) {i = i + 1}){
	for({j = 0} (!(j == 2)) {j = j + 1}){
		print Plainjane
	}
}

bool = (0 | !0 <= 1 & 1 * 1) == 1 + 2^2 - 2^2
Str = "testing booleans"
if bool
    print(Str)

math = (2^2 + 2^2 + 3^2) / 17
if math {
	print 1738
}
else {
	print 99999
}

truth = 1
Meme = "ayy lmao"
function = fun {
	for({k = 0} (!(k == 5)) {k = k + 1}){
		while (!(truth == 0)){
			print Meme
			truth = truth + 1
			if truth == 3 {
				truth = 0
			}
		}
		truth = 1
	}
}

function()
