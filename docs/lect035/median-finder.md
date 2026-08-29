# Bug hunting

## Original version
```typescript
addNum(num: number): void {
    const maxTop: number | null = this.maxHeap.front();
    if (this.maxHeap.isEmpty() || (maxTop && maxTop >= num)) {
      this.maxHeap.enqueue(num);
    } else {
      this.minHeap.enqueue(num);
    }
    this.balance();
  }

  findMedian(): number {
    const minTop = this.minHeap.front();
    const maxTop = this.maxHeap.front();
    if (this.minHeap.size() === this.maxHeap.size()) {
      if (minTop && maxTop) return (minTop + maxTop) / 2;
    } else {
      return this.minHeap.size() > this.maxHeap.size() ? (minTop as number) : (maxTop as number);
    }
    return 0;
  }


  balance(): void {
    if (Math.abs(this.minHeap.size() - this.maxHeap.size()) === 2) {
      if (this.minHeap.size() > this.maxHeap.size()) {
        const minTop = this.minHeap.dequeue();
        if (minTop) this.maxHeap.enqueue(minTop);
      } else {
        const maxTop = this.maxHeap.dequeue();
        if (maxTop) this.minHeap.enqueue(maxTop);
      }
    }
  }
```
## Fixed by Claude 
Good catch — the bug is a classic JavaScript truthiness pitfall: **`0` is falsy**, but `0` is a perfectly valid number to store in these heaps. The TypeScript code uses `if (value)` checks where it should use `if (value !== null)`, and this breaks in multiple places whenever a heap's top (or dequeued value) happens to be `0`.

Let's compare against the Java version, which never has this problem because Java's `if` only accepts actual booleans — there's no implicit truthiness coercion for `Integer`.

**Bug 1 — `addNum`, routing logic:**
```typescript
if (this.maxHeap.isEmpty() || (maxTop && maxTop >= num)) {
```
If `maxTop === 0`, `maxTop && ...` short-circuits to `false` even though `0 >= num` might be true. So a number that should go into `maxHeap` gets incorrectly routed to `minHeap`. The Java equivalent (`maxHeap.peek() >= num`) has no such issue since it's a direct comparison.

**Bug 2 — `balance()`, the data-loss bug (most severe):**
```typescript
const minTop = this.minHeap.dequeue();
if (minTop) this.maxHeap.enqueue(minTop);
```
If the dequeued value is `0`, `if (minTop)` is `false`, so the `0` is **silently dropped instead of being moved to the other heap**. This permanently loses an element from the dataset, which will corrupt every subsequent median calculation. This is almost certainly what's failing test 21 (some test case must include a `0` value at the right moment to trigger rebalancing on it).

**Bug 3 — `findMedian()`, same issue:**
```typescript
if (minTop && maxTop) return (minTop + maxTop) / 2;
...
if (maxTop && minTop) return ...
```
If either heap's top is `0`, these conditions fail even though both values exist, and execution falls through — in the worst case returning the wrong branch or the fallback `return 0` at the end, which happens to be "accidentally correct" sometimes and wrong other times, making the bug intermittent and hard to spot.

**The fix** — replace every truthy check with an explicit `null`/`undefined` check:

```typescript
addNum(num: number): void {
  const maxTop: number | null = this.maxHeap.front();
  if (this.maxHeap.isEmpty() || (maxTop !== null && maxTop >= num)) {
    this.maxHeap.enqueue(num);
  } else {
    this.minHeap.enqueue(num);
  }
  this.balance();
}

findMedian(): number {
  const minTop = this.minHeap.front();
  const maxTop = this.maxHeap.front();
  if (this.minHeap.size() === this.maxHeap.size()) {
    if (minTop !== null && maxTop !== null) return (minTop + maxTop) / 2;
  } else {
    return this.minHeap.size() > this.maxHeap.size() ? (minTop as number) : (maxTop as number);
  }
  return 0;
}

balance(): void {
  if (Math.abs(this.minHeap.size() - this.maxHeap.size()) === 2) {
    if (this.minHeap.size() > this.maxHeap.size()) {
      const minTop = this.minHeap.dequeue();
      if (minTop !== null) this.maxHeap.enqueue(minTop);
    } else {
      const maxTop = this.maxHeap.dequeue();
      if (maxTop !== null) this.minHeap.enqueue(maxTop);
    }
  }
}
```

**Takeaway:** in JS/TS, `0`, `NaN`, `""`, and `null`/`undefined` are all falsy, so `if (value)` is only safe when you actually want to treat `0` the same as "absent." Whenever a variable can legitimately be `0` (heap tops, array indices, counts, etc.), always check with `!== null` / `!== undefined` instead of relying on truthiness. The Java version dodges this entire class of bug because it doesn't have JS-style implicit boolean coercion.
