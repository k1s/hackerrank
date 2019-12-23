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

def filter_list(l):
    return [x for x in l if isinstance(x, int)]


print(filter_list([1,2,'a','b']))

def  to_weird_case(string):
    def word(w):
        return "".join([x.upper() if ind % 2 == 0 else x.lower() for ind, x in enumerate(w)])

    return " ".join([word(w) for w in string.split()])


print(to_weird_case('This is a test'))


def isPower(n, x):
    num = n
    while num >= x:
        if (num % x != 0 or x == 1):
            return False
        if num == x:
            return True
        num = num / x
    return False
        
    
def digitsSum(n):
    return sum(map(int, str(n)))

greatNum = lambda n: n > 10 and isPower(n, digitsSum(n))

def power_sumDigTerm(n):
    nums = range(2, 99)
    acc = set()
    pq = [[pow(p, 2), p, 2] for p in nums]
    
    while len(acc) < n: 
        smallest = pq[0]

        if greatNum(smallest[0]):
            acc.add(smallest[0])
    
        power = smallest[2]
        new_base = smallest[1] + 1
        new_value = pow(new_base, power)
    
        pq[0] = [new_value, new_base, power + 1]
        pq.sort()
        
    return sorted(acc)[n - 1]
