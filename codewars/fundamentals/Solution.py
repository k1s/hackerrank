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
        

def isValidWalk(walk):
    from collections import Counter
    counter = Counter(walk)
    return len(walk) == 10 and counter['n'] == counter['s'] and counter['e'] == counter['w']


def find_nb(m):
    n = 0
    s = 0
    while s < m:
        s = n ** 3 + s
        n = n + 1
    if (s == m):
        return n - 1
    else:
        return -1
