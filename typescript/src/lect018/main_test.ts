import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import {
    TreeNode,
    preOrder,
} from './main.ts';


function newTree1(): TreeNode {
    const root = new TreeNode(1);
    root.left = new TreeNode(2);
    root.right = new TreeNode(3);
    root.left.left = new TreeNode(4);
    root.left.right = new TreeNode(5);
    root.right.left = new TreeNode(6);
    root.right.right = new TreeNode(7);
    return root;
}

Deno.test('Test preOrder without recursion', () => {
    const head = newTree1();
    assertEquals(preOrder(head), [1,2,4,5,3,6,7]);
})
