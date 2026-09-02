## How to implement `makeBTreeFromList`
I want you to teach me how to create a binary tree based on a given array of numbers in Typescript. The tree node class is defined as below

```typescript
export class TreeNode {
  val: number;
  left: TreeNode | null;
  right: TreeNode | null;
  constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
    this.val = (val===undefined ? 0 : val);
    this.left = (left===undefined ? null : left);
    this.right = (right===undefined ? null : right);
  }
}
```

My current implementation (buggy) is as follows

```typescript
export type NodeVal = number | null;
export function makeTreeFromList(vals: NodeVal[]): TreeNode {
  const n = vals.length;
  let root = new TreeNode();
  for (let i = 0; i < n; i++) {
    if (i === 0) {
      root.val = vals[i] ?? 0;
    } else {
      const pval = vals[Math.floor((i - 1) / 2)]; // parent val
      const cval = vals[i];
      console.group();
      console.log('i=', i);
      console.log('pval=', pval);
      console.groupEnd();
      // leverage typescript null/undefined checking to skip null children
      if (pval != null && cval != null) insertNode(root, new TreeNode(cval), pval);
    }
  }
  return root;
}

function insertNode(root: TreeNode, node: TreeNode, pval: number): void {
  let parent: TreeNode | null = root;
  while (parent) {
    if (parent.val === pval) {
      if (parent.left) parent.right = node;
      else parent.left = node;
    } else {
      if (parent.left) insertNode(parent.left, node, pval);
      if (parent.right) insertNode(parent.right, node, pval);
    }
  }
}
```

As you can see, when two neighbour nodes have the same value, new child will be created for both of them. There may be other bugs as well.

Correctly implemented, when an array `[3,9,20,null,null,15,7]` is passed, a binary tree should look like the one in the picture attached.

Do not rush into coding, I want to know if there is any standard or commonly used algorithms to create a binary tree based on given info? By commonly used, I mean something like pre-order, in-order traveral of a binary tree.

## Claude response
Yes — there's a well-known standard for this, and it's worth understanding *why* your current approach breaks before fixing it.

## The core idea: it's not in-order/pre-order, it's level-order (BFS)

The array `[3,9,20,null,null,15,7]` is a **level-order (breadth-first) serialization** of the tree — the same format LeetCode uses for tree problems. Each value appears in the order you'd visit it doing a BFS, level by level, left to right.

The key rule that makes this format work: **a `null` node's children are never included in the array.** Only *real* nodes contribute two slots (for their left and right child) to the array. A `null` is just "this position doesn't exist" — it doesn't get expanded further.

So for `[3, 9, 20, null, null, 15, 7]`:
- `3` is the root
- `3`'s children are `9` and `20`
- `9`'s children are `null` and `null` (9 is a leaf) — these consume two slots but produce nothing
- `20`'s children are `15` and `7`

## Why your index-math approach is fragile

You used `Math.floor((i-1)/2)` to find the parent's *value*, and (2i+1, 2i+2) is the classic formula for a **complete binary tree stored in a fully-padded array** (like a binary heap). That formula only works if *every* node — real or null — reserves its 2 child-slots in the array, all the way down to a fixed depth. That's a different, denser format than the one you're actually being given.

Your bug is really two separate problems:
1. **Wrong construction strategy**: mixing "index-based complete tree" assumptions with a "BFS with null-skipping" array.
2. **Searching by value instead of by structure**: `insertNode` walks the tree looking for a node whose `.val === pval`. If two nodes share a value (or you have a value of `0`, which is falsy-ish in other contexts), this becomes ambiguous or wrong. Structure should never be determined by value equality.

## The standard algorithm: BFS with a queue

This is how it's conventionally done, and it directly mirrors how the array was produced:

1. Create the root from `vals[0]`, and push it onto a queue.
2. Keep a pointer `i = 1` into the array.
3. While there are still nodes in the queue and array elements left:
   - Dequeue a node — this is the "current parent."
   - Take the next array value: if it's not null, create a `TreeNode`, set it as `current.left`, and enqueue it. Advance `i`.
   - Take the array value after that: if it's not null, create a `TreeNode`, set it as `current.right`, and enqueue it. Advance `i`.
4. Repeat until the array is exhausted.

Because you only enqueue *real* nodes (nulls are never enqueued), null nodes correctly never "consume" further array slots — which matches how the array was built in the first place. No value comparisons, no ambiguity, no recursion needed (though a recursive index-tracking variant also exists).

This queue-based approach is essentially the inverse of how you'd *serialize* a tree into this format (also BFS), which is why it's the standard.

Want me to walk through implementing this in TypeScript step by step, or would you like to try coding it yourself first with this algorithm in mind?
