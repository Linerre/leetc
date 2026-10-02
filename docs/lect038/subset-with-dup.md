Input `[1, 2, 2]` is first sorted in-place:

```text
nums = [1, 2, 2]
```

Then `f(nums, 0, 0, [], ans)` runs. The recursive trace:

```text
f(i=0, size=0, path=[])
  group of 1s ends at j=1
  ─ exclude 1 ─► f(i=1, size=0, path=[])
  ─ include 1 ─► path=[1], f(i=1, size=1, path=[1])

  f(i=1, size=0, path=[])
    group of 2s ends at j=3
    ─ exclude 2s ─► f(i=3, size=0, path=[])  → push []
    ─ include one 2 ─► path=[2], f(i=3, size=1, path=[2])  → push [2]
    ─ include two 2s ─► path=[2,2], f(i=3, size=2, path=[2,2])  → push [2,2]

  f(i=1, size=1, path=[1])
    group of 2s ends at j=3
    ─ exclude 2s ─► f(i=3, size=1, path=[1])  → push [1]
    ─ include one 2 ─► path=[1,2], f(i=3, size=2, path=[1,2])  → push [1,2]
    ─ include two 2s ─► path=[1,2,2], f(i=3, size=3, path=[1,2,2])  → push [1,2,2]
```

Final `ans`:

```text
[
  [],
  [2],
  [2, 2],
  [1],
  [1, 2],
  [1, 2, 2]
]
```

The `while` loop groups equal numbers so duplicates are handled by choosing how many of the whole group to include, rather than treating each `2` separately.
