// Medium 155: https://leetcode.com/problems/min-stack/description/
export class MinStack {
    #data: number[];
    #min: number[];
    size: number;

    constructor() {
        this.#data = Array();
        this.#min = Array();
        this.size = 0;
    }

    // 1. put new value into data stack
    //    - if this stack is empty or new value is smaller, put it to min stack
    //    - else put the last min value to min stack again
    // 2. inc size
    push(n: number): void {
        this.#data[this.size] = n;
        if (this.size === 0 || n < this.#min[this.size-1]) {
            this.#min[this.size] = n;
        } else {
            this.#min[this.size] = this.#min[this.size-1];
        }
        this.size++;
    }

    pop(): void {
        this.size--;
    }

    top(): number {
        return this.#data[this.size-1];
    }

    getMin(): number {
        return this.#min[this.size-1];
    }
}
