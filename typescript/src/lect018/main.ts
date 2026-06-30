export class TreeNode {
    val: number;
    left: TreeNode | null;
    right: TreeNode | null;

    constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
        this.val = (val === undefined ? 0 : val);
        this.left = (left === undefined ? null : left);
        this.right = (right === undefined ? null : right);
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
export function preOrder(head: TreeNode | null): number[] {
    // mainly for testing purposes
    const result = Array();

    if (head !== null) {
        const stack = new Stack<TreeNode>();
        stack.push(head);
        let topHead: TreeNode | undefined;
        while (!stack.isEmpty()) {
            topHead = stack.pop();
            if (topHead) {
                console.log(topHead.val);
                result.push(topHead.val);
            }
            if (topHead && topHead.right !== null) {
                stack.push(topHead.right);
            }
            if (topHead && topHead.left !== null) {
                stack.push(topHead.left);
            }
        }
    }

    return result;
}

// In-order without recursion
// 1. Push all nodes along the left edge of substree head onto stack
// 2. For each node in stack, pop it, print, repeat step 1 for all nodes along the right edge of this node
// 3. Stop until there is no substree and stack is empty
export function inOrder(head: TreeNode | null): number[] {
    // mainly for testing purposes;
    const result = new Array();
    // For null head, return empty array, which basically does nothing
    if (head !== null) {
        const stack = new Stack<TreeNode>();
        let h: TreeNode | null | undefined;
        h = head;
        while (!stack.isEmpty() || h) {
            if (h) {
                stack.push(h);
                h = h.left;     // move head left down one level - step 1
            } else {
                h = stack.pop();
                if (h) {
                    console.log(h.val);
                    result.push(h.val);
                    h = h.right; // step 2
                }
            }
        }
    }

    return result;
}


// Similar to preOrder but with one more stack to reverse the order
export function postOrderWithTwoStacks(head: TreeNode | null): number[] {
    // mainly for testing purposes
    const result = Array();

    if (head !== null) {
        const stack = new Stack<TreeNode>();
        const collect = new Stack<TreeNode>(); // for reversing the order
        stack.push(head);
        let topHead: TreeNode | undefined;
        while (!stack.isEmpty()) {
            topHead = stack.pop();
            // collect instead of print
            if (topHead) collect.push(topHead);
            if (topHead && topHead.left !== null) {
                stack.push(topHead.left);
            }
            if (topHead && topHead.right !== null) {
                stack.push(topHead.right);
            }
        }
        // print from collect stack
        while (!collect.isEmpty()) {
            let h = collect.pop();
            if (h) {
                console.log(h.val);
                result.push(h.val);
            }
        }
    }
    return result;
}


export function postOrderWithOneStack(head: TreeNode | null): number[] {
    const result: number[] = new Array();
    if (head !== null) {
        let h: TreeNode | null | undefined = head;
        const stack = new Stack<TreeNode>();
        stack.push(h)

        // If no nodes get printed, h remains to be root
        // If any node gets printed, h becomes that node
        // after the first print, h represents last-printed node
        while (!stack.isEmpty()) {
            const cur = stack.peek();
            if (cur && cur.left && h != cur.left && h != cur.right) {
                // has left subtree and needs to process
                stack.push(cur.left);
            } else if (cur && cur.right && h != cur.right){
                // has right substree and needs to process
                stack.push(cur.right);
            } else {
                // either no left/right subtree or both have been processed
                if (cur) {
                    console.log(cur.val);
                    result.push(cur.val);
                }
                h = stack.pop();
            }
        }
    }
    return result;
}
