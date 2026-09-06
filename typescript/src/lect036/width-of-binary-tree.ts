import { TreeNode } from './util.ts'; 

const MAXN = 3001;
function widthOfBinaryTree(root: TreeNode | null): number {
  if (root === null) return 0;
  const nodeQueue = new Array<TreeNode>(MAXN);
  const indexQuque = new Array<number>(MAXN);
  let l = 0;
  let r = 0;
  let ans = 1;
  
  // root index starts with 1
  nodeQueue[r] = root;
  indexQuque[r++] = 1;

  // repeat until queue is empty
  while (l < r) {
    const size = r - l;
    const first = indexQuque[l];
    ans = Math.max(ans, indexQuque[r - 1] - first + 1);

    for (let i = 0; i < size; i++) {
      const n = nodeQueue[l];
      // re-base to avoid unbounded growth
      const ni = indexQuque[l++] - first + 1;
      if (n.left) {
        nodeQueue[r] = n.left;
        indexQuque[r++] = ni * 2;
      }
      if (n.right) {
        nodeQueue[r] = n.right;
        indexQuque[r++] = ni * 2 + 1;
      }
    }
  }

  return ans;
};
