x = 3               # x : int
y = "abc"           # y : string

def f1(y):          # y : T(1)            / x : int,  y : string
	global x    # y : T(1), x : int   / x : int,  y : string
	x = y       # y : T(1), x : T(1)  / x : T(1), y : string
	return x    # !ret! : T(1), y : T(1), x : T(1)

def f2(y):          # y : T(1)            / x : int,  y : string
	x = y       # y : T(1), x : T(1)  / x : int,  y : string
	return x    # !ret! : T(1)

f(y)                # x : int, y : string
print x
