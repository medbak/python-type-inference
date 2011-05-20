global_var = 0
def f():
	global global_var
	global_var = 1
	local_var = 2
	def g():
		local_var = 3
	g()
	return local_var
print f(), global_var
