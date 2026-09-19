import { TreeNode } from '../lect036/util.ts';

// Medium 337: https://leetcode.cn/problems/house-robber-iii/description/
function rob(root: TreeNode | null): number {
  const { yes, no } = f(root);
  return Math.max(yes, no);
};

type Option = {
  yes: number;
  no: number;
}

function f(head: TreeNode | null): Option {
  if (head === null) return { yes: 0, no: 0 };
  let y = head.val;
  let n = 0;
  const lopt = f(head.left);
  y += lopt.no;
  n += Math.max(lopt.yes, lopt.no);

  const ropt = f(head.right);
  y += ropt.no;
  n += Math.max(ropt.yes, ropt.no);

  return { yes: y, no : n };
}
