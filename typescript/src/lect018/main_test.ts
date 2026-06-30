import {  assert, assertEquals, assertFalse } from 'jsr:@std/assert';
import {
    TreeNode,
    inOrder,
    postOrderWithTwoStacks,
    postOrderWithOneStack,
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

Deno.test('Test preOrder without recursion 0', () => {
    const head = newTree1();
    assertEquals(preOrder(head), [1,2,4,5,3,6,7]);
});


Deno.test('Test preOrder without recursion 1', () => {
    const root = new TreeNode(1);
    root.right = new TreeNode(2);
    root.right.right = new TreeNode(3);
    assertEquals(preOrder(root), [1,2,3]);
});

Deno.test('Test preOrder without recursion 2', () => {
    const root = new TreeNode(1);
    root.left = new TreeNode(4)
    root.right = new TreeNode(2);
    root.right.right = new TreeNode(3);
    assertEquals(preOrder(root), [1,4,2,3]);
});

Deno.test('Test inOrder without recursion 1', () => {
    const root = newTree1();
    assertEquals(inOrder(root), [4,2,5,1,6,3,7]);
});

Deno.test('Test inOrder without recursion 2', () => {
    const root = new TreeNode(1);
    // left subtree
    root.left = new TreeNode(2);
    root.left.left = new TreeNode(4);
    root.left.left.right = new TreeNode(5);
    root.left.left.right.left = new TreeNode(6);
    root.left.right = new TreeNode(7);
    // right subtree
    root.right = new TreeNode(3);
    root.right.right = new TreeNode(8);
    root.right.right.left = new TreeNode(9);
    root.right.right.left.left = new TreeNode(10);
    assertEquals(inOrder(root), [4,6,5,2,7,1,3,10,9,8]);
});

Deno.test('Test postOrderTwoStacks', () => {
    const root = newTree1();
    assertEquals(postOrderWithTwoStacks(root), [4,5,2,6,7,3,1]);
});

Deno.test('Test postOrderWithOneStack', () => {
    const root = newTree1();
    assertEquals(postOrderWithOneStack(root), [4,5,2,6,7,3,1]);
});
