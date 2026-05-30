// Implement queue using two stacks
// 1. Prepare two stacks: in and out
// 2. in-stack can put items into out-stack iff out-stack is empty
// 3. when in-stack puts items into out-stack, it must release all the items
// Easy 232: https://leetcode.com/problems/implement-queue-using-stacks/
class Stack {
    #stack: number[];
    _size: number;

    constructor() {
        this.#stack = new Array();
        this._size = 0;
    }

    isEmpty(): boolean {
        return this._size === 0;
    }

    push(n: number): void {
        this.#stack[this._size++] = n;
    }

    pop(): number {
        return this.#stack[--this._size];
    }

    peek(): number {
        return this.#stack[this._size - 1];
    }

    size(): number {
        return this._size;
    }
}

export class StackQueue {
    in: Stack;
    out: Stack;

    constructor() {
        this.in = new Stack();
        this.out = new Stack();
    }

    inToOut(): void {
        if (this.out.isEmpty()) {
            while(!this.in.isEmpty()) {
                this.out.push(this.in.pop());
            }
        }
    }

    push(x: number): void {
        this.in.push(x);
        this.inToOut();
    }

    pop(): number {
        this.inToOut();
        return this.out.pop();
    }

    peek(): number {
        this.inToOut();
        return this.out.peek();
    }

    empty(): boolean {
        return this.in.isEmpty() && this.out.isEmpty();
    }
}

// Implement stack using queue
class Queue {
    #queue: number[];
    l: number;
    r: number;

    constructor() {
        this.#queue = Array();
        this.l = 0;
        this.r = 0;
    }

    isEmpty(): boolean {
        return this.l === this.r;
    }

    // Put in a new item
    offer(n: number): void {
        this.#queue[this.r++] = n;
    }

    poll(): number {
        return this.#queue[this.l++];
    }

    head(): number {
        return this.#queue[this.l];
    }

    tail(): number {
        return this.#queue[this.r-1];
    }

    size(): number {
        return this.r - this.l;
    }
}

// Easy 225: https://leetcode.com/problems/implement-stack-using-queues/
export class QueueStack {
    queue: Queue;

    constructor() {
        this.queue = new Queue();
    }

    // O(n): insert the new item and re-insert all of the rest after the new item
    push(x: number): void {
        const n = this.queue.size();
        this.queue.offer(x);
        for (let i = 0; i < n; i++) {
            this.queue.offer(this.queue.poll());
        }
    }

    pop(): number {
        return this.queue.poll();
    }

    top(): number {
        return this.queue.head();
    }

    empty(): boolean {
        return this.queue.isEmpty();
    }
}
