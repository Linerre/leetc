// Medium 2166: https://leetcode.cn/problems/design-bitset/description/
export class Bitset {
    _set: number[];
    readonly size: number;
    zeros: number;
    ones: number;
    reverse: boolean;

    constructor(size: number) {
        this._set = new Array((Math.floor((size + 31) / 32)));
        this._set.fill(0);
        this.size = size;
        this.zeros = size;
        this.ones = 0;
        this.reverse = false;
    }

    // Add number to the bitset
    fix(idx: number): void {
        const slot = Math.floor(idx / 32);
        const bit = idx % 32;
        if (!this.reverse) {
            // 0 means non-exiting in non-reverse state
            if ((this._set[slot] & (1 << bit)) === 0) {
                this.zeros--;
                this.ones++;
                this._set[slot] |= (1 << bit);
            }
        } else {
            // 1 means non-exiting in reverse state
            if ((this._set[slot] & (1 << bit)) !== 0) {
                this.zeros--;
                this.ones++;
                this._set[slot] ^= (1 << bit);
            }
        }
    }

    unfix(idx: number): void {
        const slot = Math.floor(idx / 32);
        const bit = idx % 32;
        if (!this.reverse) {
            // non-zero means existing in non-reverse state
            if ((this._set[slot] & (1 << bit)) !== 0) {
                this.zeros++;
                this.ones--;
                this._set[slot] ^= (1 << bit);
            }
        } else {
            // 0 means exiting in reverse state
            if ((this._set[slot] & (1 << bit)) === 0) {
                this.zeros++;
                this.ones--;
                this._set[slot] |= (1 << bit);
            }
        }
    }

    flip(): void {
        this.reverse = !this.reverse;
        const tmp = this.zeros;
        this.zeros = this.ones;
        this.ones = tmp;
    }

    all(): boolean {
        return this.ones === this.size;
    }

    one(): boolean {
        return this.ones > 0;
    }

    count(): number {
        return this.ones;
    }

    toString(): string {
        let str: string = '';
        for (let i = 0, k = 0, number, status; i < this.size; k++) {
            number = this._set[k];
            for (let j = 0; j < 32 && i < this.size; j++, i++) {
                // status represents each bit (1 or 0)
                status = (number >> j) & 1;
                status ^= this.reverse ? 1 : 0;
                str += (status).toString();
            }
        }
        return str;
    }
}
