import { TreeNode } from './util.ts';

// Medium 103: https://leetcode.cn/problems/binary-tree-zigzag-level-order-traversal/description/
const MAXN = 2001;
export function zigzagLevelOrder(root: TreeNode | null): number[][] {
  const levels: number[][] = [];
  if (root !== null) {
    const queue = new Array<TreeNode>(MAXN);
    let l = 0;
    let r = 0;
    // true:  right -> left
    // false: left -> true
    let reverse: boolean = false;
    queue[r++] = root;

    while (l < r) {
      const size = r - l;
      const level = new Array<number>();

      // process the entire level
      for (let i = reverse ? r - 1 : l, j = reverse ? -1 : 1, k = 0; k < size; i += j, k++) {
        const node = queue[i];
        if (node) level.push(node.val);
      }

      // add node's children (if any) into the queue
      for (let i = 0; i < size; i++) {
        const cur = queue[l++];
        if (cur && cur.left) queue[r++] = cur.left;
        if (cur && cur.right) queue[r++] = cur.right;
      }
      levels.push(level);
      reverse = !reverse;

    }
  }
  return levels;
};
