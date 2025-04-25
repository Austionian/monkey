let fibonacci = fn(n) {
  let bottom_up = [0, 1, 1];

  let i = 3;
  loop {
    if (i == n + 1) {
      break;
    }

    let a = bottom_up[i - 2];
    let b = bottom_up[i - 1];
    bottom_up = push(bottom_up, a + b);

    i = i + 1;
  }

  last(bottom_up);
}

fibonacci(75);
