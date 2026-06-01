// Binary tree traversal
// Pre-order traversal: current node -> left subtree -> right subtree
// In-order traversal: left subtree -> current node -> right subtree
// post-order traversal: left subtree -> current node -> right subtree
// For orders above, any substree traveral will follow the corresponding order
class TreeNode {
    value: number;
    left: TreeNode;
    right: TreeNode;

    constructor(value: number) {
        this.value = value;
        this.left = null;
        this.right = null;
    }

    // This method does nothing but help understand traveral
    f(head: TreeNode): void {
        if (head === null) return;
        // 1
        f(head.left);
        // 2
        f(head.right);
        // 3
    }

    static preOrder(head: TreeNode): void {
        if (head === null) return;
        console.log(head.value);
        this.preOrder(head.left);
        this.preOrder(head.right);
    }

    static inOrder(head: TreeNode): void {
        if (head === null) return;
        this.inOrder(head.left);
        console.log(head.value);
        this.inOrder(head.right);
    }

    static postOrder(head: TreeNode): void {
        if (head === null) return;
        this.postOrder(head.left);
        this.postOrder(head.right);
        console.log(head.value);
    }
}

function main() {
    const root = new TreeNode(1);
    root.left = new TreeNode(2);
    root.right = new TreeNode(3);
    root.left.left = new TreeNode(4);
    root.left.right = new TreeNode(5);
    root.right.left = new TreeNode(6);
    root.right.right = new TreeNode(7);

    console.group()
    console.log('Pre order');
    TreeNode.preOrder(root);
    console.groupEnd();

    console.group()
    console.log('In order');
    TreeNode.inOrder(root);
    console.groupEnd();

    console.group()
    console.log('Post order');
    TreeNode.postOrder(root);
    console.groupEnd();
}

main();
