

```bash
$ git ls-tree -r --name-only HEAD | grep ".*.scala" |\
  xargs -n1 git blame --line-porcelain | grep "^author " |\
  sort | uniq -c | sort -nr
```

+-------------------+---------+-----------+-----------+-------------+
| author            | commits | additions | deletions | blame lines |
+-------------------+---------------------+-----------+-------------+
| Martin Odersky    | 3 158   | 184 864   | 107 580   | 75380       |
| Dmitry Petrashko  |   830   | 160 883   |  51 408   | 87112       |
| Felix Mulder      |   185   |  27 011   |  18 298   |  8705       |
| Guillaume Martres |   182   |   9 731   |   2 420   |  6291       |
| Others            |   908   |           |           | 13166       |
+-------------------+---------+-----------+-----------+-------------+
