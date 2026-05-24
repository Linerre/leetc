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
