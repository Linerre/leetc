export class TreeNode {
    val: number;
    left: TreeNode | null;
    right: TreeNode | null;

    constructor(val: number) {
        this.val = val;
        this.left = null;
        this.right = null;
    }
};

export class Stack<T> {
    #stack: T[];
    #size: number;

    constructor(capacity?: number) {
        this.#stack = capacity !== undefined ? new Array(capacity) : [];
        this.#size = 0;
    }

    isEmpty(): boolean {
        return this.#size === 0;
    }

    push(item: T): void {
        this.#stack[this.#size++] = item;
    }

    pop(): T | undefined {
        if (this.isEmpty()) return undefined;
        return this.#stack[--this.#size];
    }

    peek(): T | undefined {
        if (this.isEmpty()) return undefined;
        return this.#stack[this.#size - 1];
    }

    size(): number {
        return this.#size;
    }
}

// Pre-order without recursion
// 1. Push a non-null node into a stack
// 2. if stack is not empty, pop the top node, print it
// 3. push its right node onto stack if any
// 4. push its left node onto stack if any
// 5. repeat 2-4
export function preOrder(head: TreeNode): number[] {
    // mainly for testing purposes
    const result = Array();

    if (head !== null) {
        const stack = new Stack<TreeNode>();
        stack.push(head);
        let topHead: TreeNode | undefined;
        while (!stack.isEmpty()) {
            topHead = stack.pop();
            console.log(head.val, ' ');
            result.push(head.val);
            if (topHead && topHead.right !== null) {
                stack.push(topHead.right);
            }
            if (topHead && topHead.left !== null) {
                stack.push(topHead.left);
            }
        }
        console.log()
    }

    return result;
}
