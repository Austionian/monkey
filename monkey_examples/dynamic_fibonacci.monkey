let fibonacci = fn(n) {
  let bottom_up = [0, 1, 1];

  let i = 3;
  loop {
    if (i == n + 1) {
      break;
    }

    bottom_up = push(bottom_up, bottom_up[i - 2] + bottom_up[i - 1]);

    i = i + 1;
  }

  last(bottom_up);
}

fibonacci(75);
