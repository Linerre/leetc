import { TreeNode } from '../lect036/util.ts';

function pathSum(root: TreeNode | null, targetSum: number): number[][] {
  const ans: number[][] = [];
  if (root) {
    const path: number[] = [];
    f(root, targetSum, 0, path, ans);
  }
  return ans;
};


function f(node: TreeNode | null, aim: number, sum: number, path: number[], ans: number[][]): void {
  if (node) {
    if (node.left === null && node.right === null) {
      // leaf node
      if (node.val + sum === aim) {
        path.push(node.val);
        ans.push(path.map(v => v));
        // reset state to the one before visiting current node
        path.pop();
      }
    } else {
      // not leaf node so need to record it and move down
      path.push(node.val);
      if (node.left) f(node.left, aim, sum + node.val, path, ans);
      if (node.right) f(node.right, aim, sum + node.val, path, ans);
      // reset state to the one before visiting current node
      path.pop();
    }
  }
}
