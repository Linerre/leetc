import { Queue } from '@datastructures-js/queue';
import { TreeNode } from './util.ts';

// Medium 102: https://leetcode.cn/problems/binary-tree-level-order-traversal/description/
function levelOrder(root: TreeNode | null): number[][] {
  const ans: number[][] = [];
  if (root !== null) {
    const queue = new Queue<TreeNode>();
    const levels = new Map<TreeNode, number>();
    queue.enqueue(root);
    levels.set(root, 0);
    while (!queue.isEmpty()) {
      const cur = queue.dequeue();
      if (cur === null) return [];
      const level = levels.get(cur);
      if (level === undefined) return [];
      if (ans.length === level) ans.push(new Array<number>());
      ans[level].push(cur.val);
      if (cur.left) {
        queue.enqueue(cur.left);
        levels.set(cur.left, level + 1);
      }
      if (cur.right) {
        queue.enqueue(cur.right);
        levels.set(cur.right, level + 1);
      }
    }
  }
  return ans;
};
