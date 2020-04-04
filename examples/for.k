extern putchard(char);

def printstar(n m)
  for i = 0, i < n, 1 in
    putchard(10) +
    for j = 0, j < m, 1 in
      putchard(42);

printstar(2, 5);
