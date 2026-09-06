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
    // pop node and its index from both queues
    const li = indexQuque[l] ?? 0;
    const ri = indexQuque[r - 1] ?? 0;
    if (li === 0 || ri === 0) return ans;
    // width = rightmost non-null node index - leftmost non-null node + 1
    ans = Math.max(ans, ri - li + 1);

    // put current nodes' children into queue for next loop
    for (let i = 0; i < size; i++) {
      const n = nodeQueue[l] ?? null;
      const ni = indexQuque[l++] ?? 0;
      if (n?.left && ni !== 0) {
        nodeQueue[r] = n?.left;
        indexQuque[r++] = ni * 2;
      }
      if (n?.right && ni !== 0) {
        nodeQueue[r] = n?.right;
        indexQuque[r++] = ni * 2 + 1;
      }
    }
  }
  return ans;
};
