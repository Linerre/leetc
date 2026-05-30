// A queue can be viewed as a range of L (inclusive) to R (exclusive) in an fixed-length array.
// if L < R, there must be items in this queue.
// Similarly, if L == R, the queue must be empty.
// At the very first, L == R == 0 and the queue is empty.
// To insert/put an item, place it at the position of current R and the inc R
// To pop/remove an item, choose the one at the position of current L and inc L
// The total number of insertions should NOT exceed the array size
export class Queue {
    #queue: number[];
    l: number;
    r: number;

    constructor(n: number) {
        this.#queue = Array(n);
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

// Array-based stack behavior:
// Adding an item, put it at size and inc size
// Removing an item, pop num at size-1 and dec size
// At any given time, the total number in stack should NOT exceed the stack size
export class Stack {
    #stack: number[];
    _size: number;

    constructor(n: number) {
        this.#stack = Array(n);
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


// Circular queue
// 1. only when size < limit: put a new item at tail, inc tail or tail -> 0, inc size
// 2. only when size > 0: pop a new item at head, inc head or head -> 0, dec size
// 3. size controls if operation 1 or 2 can be done
// 622: https://leetcode.com/problems/design-circular-queue/description/
export class CircularQueue {
    queue: number[];
    l: number;                  // head
    r: number;                  // tail
    size: number;
    limit: number;

    constructor(limit: number) {
        this.queue = new Array(limit);
        this.limit = limit;
        this.l = 0;
        this.r = 0;
        this.size = 0;
    }

    enQueue(value: number): boolean {
        if (this.isFull()) {
            return false;
        } else {
            this.queue[this.r] = value;
            this.r = this.r === this.limit - 1 ? 0 : this.r + 1;
            this.size++;
            return true;
        }
    }

    deQueue(): boolean {
        if (this.isEmpty()) {
            return false;
        } else {
            this.l = this.l === this.limit - 1 ? 0 : this.l + 1;
            this.size--;
            return true;
        }
    }

    Front(): number {
        if (this.isEmpty()) {
            return -1;
        } else {
            return this.queue[this.l];
        }
    }

    Rear(): number {
        if (this.isEmpty()) {
            return -1;
        } else {
            const last = this.r === 0 ? this.limit - 1 : this.r - 1;
            return this.queue[last];
        }
    }

    isEmpty(): boolean {
        return this.size === 0;
    }

    isFull(): boolean {
        return this.size === this.limit;
    }
}
