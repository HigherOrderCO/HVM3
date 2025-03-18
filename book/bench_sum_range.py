class Nil:
    def __init__(self):
        self.tag = "Nil"

class Cons:
    def __init__(self, head, tail):
        self.tag = "Cons"
        self.head = head
        self.tail = tail

def range_custom(n, xs):
    result = xs
    for i in range(n, 0, -1):
        result = Cons(i - 1, result)
    return result

def sum_custom(lst):
    total = 0
    while lst.tag != "Nil":
        total = (total + lst.head) & 0xFFFFFFFF
        lst = lst.tail
    return total

def main():
    print(sum_custom(range_custom(50000000, Nil())))

if __name__ == "__main__":
    main()
