// List representation
const Nil = { $: "Nil" };
const Cons = (head, tail) => ({ $: "Cons", head, tail });

function range(n, xs) {
  let result = xs;
  for (let i = n; i > 0; i--) {
    result = Cons(i - 1, result);
  }
  return result;
}

function sum(list) {
  let sum = 0;
  while (list.$ !== "Nil") {
    sum  = (sum + list.head) >>> 0;
    list = list.tail;
  }
  return sum;
}

function main() {
  console.log(sum(range(50_000_000, Nil)));
}

main();

// translate this to python
