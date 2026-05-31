// Double-ended circular queue based on an array of K
// Items can be inserted at both head (l) and tail (r)
// Items can be popped at both head (l) and tail (r)
// 0. For empty queue, insert at l == r == 0; size++
// 1. Head insertion (l moves towards left):
// - if l == 0, insert at k - 1, l = k - 1
// - if l != 0, insert at l - 1, l--
// 2. Head deletion (l moves towards right):
// - if l == k - 1, l moves to 0;
// - if l != k - 1, l++
// 3. Tail inerstion (r moves towards right):
// - if r == k - 1, insert at 0, r = 0;
// - if r != k - 1, insert at r + 1, r++;
// 4. Tail deletion (r moves towards left):
// - if r == 0, r moves to k - 1; r = k - 1;
// - if r != 0, r moves to r - 1, r--;
// Medium 641: https://leetcode.com/problems/design-circular-deque/description/
export class CircularDequeu {
    deque: number[];
    limit: number;
    size: number;
    l: number;
    r: number;

    constructor(k: number) {
        this.deque = Array(k);
        this.limit = k;
        this.size = 0;
        this.l = 0;
        this.r = 0;
    }

    insertFront(value: number): boolean {
        if (this.isFull()) return false;
        if (this.isEmpty()) {
            // reset l and r to 0
            this.l = this.r = 0;
            this.deque[0] = value;
        } else {
            this.l = this.l === 0 ? this.limit - 1 : this.l - 1;
            this.deque[this.l] = value;
        }
        this.size++;
        return true;
    }

    insertLast(value: number): boolean {
        if (this.isFull()) return false;
        if (this.isEmpty()) {
            // reset l and r to 0
            this.l = this.r = 0;
            this.deque[0] = value;
        } else {
            this.r = this.r === this.limit - 1 ? 0 : this.r + 1;
            this.deque[this.r] = value;
        }
        this.size++;
        return true;
    }

    deleteFront(): boolean {
        if (this.isEmpty()) {
            return false;
        } else {
            this.l = (this.l === this.limit - 1) ? 0 : this.l + 1;
            this.size--;
            return true;
        }
    }

    deleteLast(): boolean {
        if (this.isEmpty()) {
            return false;
        } else {
            this.r = this.r === 0 ? this.limit - 1 : this.r - 1;
            this.size--;
            return true;
        }
    }

    getFront(): number {
        if (this.isEmpty()) return -1;
        else return this.deque[this.l];
    }

    getRear(): number {
        if (this.isEmpty()) return -1;
        else return this.deque[this.r]
    }

    isEmpty(): boolean {
        return this.size === 0;
    }

    isFull(): boolean {
        return this.size === this.limit;
    }
}
