with open("x86-64 experiments") as f:
    for x in f:
        a, b ,c= x.split("[")
        name = a.split(" ")[1]
        unsafe = b.split("]")[0]
        safe = c.split("]")[0]
        print(f"{name} {unsafe} {safe}")