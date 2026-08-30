## Some minor bugs
Found it. It's in `dec()`.

In the Java version, these two lines run **unconditionally**, after the if/else:
```java
bucket.set.remove(key);
if (bucket.set.isEmpty()) {
    remove(bucket);
}
```

But in the TypeScript port, the equivalent lines are placed **only inside the `else` branch**:

```typescript
dec(key: string): void {
    const keyBucket = this.map.get(key);
    if (keyBucket === undefined) return;

    if (keyBucket.cnt === 1) {
      this.map.delete(key);            // <-- key removed from map...
      // BUG: missing keyBucket.set.delete(key) and empty-bucket cleanup here!
    } else {
      // ... (correct logic to move key to a different bucket)
      keyBucket.set.delete(key);
      if (keyBucket.set.size === 0) Bucket.remove(keyBucket);
    }
  }
```

**Effect of the bug:** when `keyBucket.cnt === 1`, the key is deleted from `map`, but it's never removed from `keyBucket.set`. So the bucket keeps a "ghost" string in its set forever, and the bucket itself is never unlinked from the list even though (from the map's perspective) it should be empty. This corrupts `getMinKey()`/`getMaxKey()`, since `head.next`/`tail.prev` buckets can retain stale keys that no longer have any real frequency count.

This matches the trace: after `dec a..f`, the cnt=1 bucket should shrink to `{k, l}`, but instead it retains `{a,b,c,d,e,f,k,l}` (only g,h,i,j get properly removed later, because that removal happens in `inc()`, whose delete-and-cleanup lines *are* correctly placed unconditionally). That's exactly why `getMinKey` wrongly returns `"a"` instead of a key from `{k, l}`.

**Fix** — pull the cleanup out of the `else`, so it always runs:

```typescript
dec(key: string): void {
    const keyBucket = this.map.get(key);
    if (keyBucket === undefined) return;

    if (keyBucket.cnt === 1) {
      this.map.delete(key);
    } else {
      if (keyBucket.prev && keyBucket.prev.cnt === keyBucket.cnt - 1) {
        this.map.set(key, keyBucket.prev);
        keyBucket.prev.set.add(key);
      } else {
        const newBucket = new Bucket(key, keyBucket.cnt - 1);
        this.map.set(key, newBucket);
        if (keyBucket.prev) Bucket.insert(keyBucket.prev, newBucket);
      }
    }
    // moved out of the else so it always runs, matching the Java version
    keyBucket.set.delete(key);
    if (keyBucket.set.size === 0) Bucket.remove(keyBucket);
  }
```

(Separately: the exact strings expected in your test, like `"i"` vs `"g"`, come down to Java `HashSet` iteration order vs JS `Set` insertion order — that's not a real bug, since the AllOne problem only requires *any* key with the correct max/min frequency. The `dec()` cleanup omission above is the actual functional bug.)

## Do maps guarantee O1?
Good question — they're different data structures under the hood, but both land on O(1) average.

**Java's `HashMap`**

`HashMap` (what's used in this code as `HashMap<String, Bucket>`) is a genuine hash table: keys are hashed, hash bucketed into an array, and lookup/insert/delete are O(1) average (O(n) worst case with heavy collisions, though Java 8+ mitigates this by treeifying long collision chains into red-black trees, giving O(log n) worst case in that scenario).

Note: Java also has `TreeMap`, which is a red-black tree with O(log n) operations and sorted key iteration — but that's *not* what's used here. This code uses `HashMap`, so it's O(1) average, same complexity class as needed.

**JavaScript/TypeScript's `Map`**

`Map` in JS is also specified to be implemented as a hash table (the ECMAScript spec requires "average access time... sublinear on the number of elements," and in practice all major engines — V8, SpiderMonkey, JSC — implement it as a genuine hash table). So `Map.get/set/delete` are O(1) average, just like Java's `HashMap`.

Some relevant differences from Java's `HashMap`, though none affect this problem's complexity:
- JS `Map` preserves **insertion order** during iteration (Java `HashMap` has no guaranteed order).
- JS `Map` allows any value as a key (objects, functions, etc.), not just types with proper `hashCode`/`equals`.
- JS `Map` doesn't have Java's collision-chain treeification, but in practice this doesn't matter for string keys with V8's hash implementation.

**So for this problem:** both `map.get(key)` / `map.set(key, ...)` / `map.delete(key)` in the TS version and `map.get(key)` / `map.put(key, ...)` / `map.remove(key)` in the Java version are O(1) average — matching the problem's O(1) requirement. The rest of the design (doubly linked list of buckets, each bucket holding a `Set`/`HashSet` of keys at that frequency) is what makes `inc`, `dec`, `getMaxKey`, `getMinKey` all O(1) as well, since bucket insertion/removal and set add/remove are also O(1) average.
