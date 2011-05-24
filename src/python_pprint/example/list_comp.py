def f1():
     global g
     g = 1
     return g
 
def f2():
     global g
     g = 2
     return g
 
#[3, 4, 3, 4]
y = [x + g for x in [f1(), f2(), f1(), f2()]]
