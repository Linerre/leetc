import { Queue } from '@datastructures-js/queue';
import { TreeNode } from './util.ts';

// Medium 102: https://leetcode.cn/problems/binary-tree-level-order-traversal/description/
function levelOrder1(root: TreeNode | null): number[][] {
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

const MAXN = 2001;
function levelOrder2(root: TreeNode | null): number[][] {
  const queue: TreeNode[] = Array<TreeNode>(MAXN);
  let levels: number[][] = [];
  if (root !== null) {
    // l marks leftmost node and r - 1 marks the rightmost node
    // this line is also equivalent to clearing the queue
    let l = 0;
    let r = 0;
    // put root into the queue first
    queue[r] = root;
    r++;
    // repeat until there is nothing in the queue
    while (l < r) {
      const size = r - l;
      let level = new Array<number>();
      // repeat size times
      for (let i = 0; i < size; i++) {
        // 1. take a node from the queue
        const node = queue[l];
        l++;
        level.push(node!.val);
        if (node && node.left) {
          queue[r] = node.left;
          r++;
        }
        if (node && node.right) {
          queue[r] = node.right;
          r++;
        }
      }
      levels.push(level);
    }
  }
  return levels;
}
