## Question
I have below function that converts a binary tree to an array of numbers or nulls. The problem is I don't want the trailing nulls. With a tree like in the image, it outputs `[3,9,20,null,null,20,15,7,null,null,null,null]`. How to get rid of the trailing `null`s?

```typescript
export type NodeVal = number | null;
export function treeToArray(head: TreeNode | null): NodeVal[] {
  const arr = Array<NodeVal>();
  if (head) {
    const queue = new Queue<TreeNode | null>();
    queue.enqueue(head);
    while (!queue.isEmpty()) {
      const node = queue.dequeue();
      if (node) {
        arr.push(node.val);
        queue.enqueue(node.left);
        queue.enqueue(node.right);
      } else {
        arr.push(null);
      }
    }
  }
  return arr;
}

```

## Reply
Simplest fix: just trim trailing `null`s off the end after building the array.

```typescript
export type NodeVal = number | null;
export function treeToArray(head: TreeNode | null): NodeVal[] {
  const arr = Array<NodeVal>();
  if (head) {
    const queue = new Queue<TreeNode | null>();
    queue.enqueue(head);
    while (!queue.isEmpty()) {
      const node = queue.dequeue();
      if (node) {
        arr.push(node.val);
        queue.enqueue(node.left);
        queue.enqueue(node.right);
      } else {
        arr.push(null);
      }
    }
  }
  while (arr.length > 0 && arr[arr.length - 1] === null) {
    arr.pop();
  }
  return arr;
}
```

For your example, that turns `[3,9,20,null,null,20,15,7,null,null,null,null]` into `[3,9,20,null,null,20,15,7]`.

Note this only removes trailing nulls — any nulls in the *middle* (like the two after `9`) stay, since they're needed to indicate structure (they mark that `9` has no children, which matters for reconstructing the tree). That matches the format LeetCode uses for tree-array serialization, if that's what you're going for.

One small thing worth watching: `arr.pop()` is O(1) so trimming a lot of trailing nulls is cheap either way, but if you ever expect really large trees and want to avoid the two-pass feel, you could track the index of the last non-null value while building the array and slice once at the end instead — but honestly the pop-loop above is simpler and plenty fast for this.
