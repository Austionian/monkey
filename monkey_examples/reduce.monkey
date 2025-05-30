let reduce = fn(arr, initial, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result
    } else {
      iter(rest(arr), f(result, first(arr)))
    }
  }

  iter(arr, initial);
};

let arr = [1, 2, 3, 4];

let sum = fn(arr) {
  reduce(arr, 0, fn(initial, element) { initial + element })
};

sum(arr);
