def likes(names):
    def ans(names):
        if len(names) == 0:
            return "no one likes this"
        elif len(names) == 1:
            return "{0} likes this".format(names[0])
        elif len(names) == 2:
            return "{0} and {1} like this".format(names[0], names[1])
        elif len(names) == 3:
            return "{0}, {1} and {2} like this".format(names[0], names[1], names[2])
        else:
            return "{0}, {1} and {s} others like this".format(names[0], names[1], s = (len(names) - 2))

    return ans(names)
        
print(likes([]))
print(likes(['Peter']))
print(likes(['Peter', 'Jacob']))
print(likes(['Max', 'John', 'Mark']))
print(likes(['Alex', 'Jacob', 'Mark', 'Max']))
