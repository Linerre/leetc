// Medium 380: https://leetcode.cn/problems/insert-delete-getrandom-o1/description/
export class RandomizedSet {
  private map: Map<number, number>;
  private arr: number[];

  constructor() {
    this.map = new Map<number, number>();
    this.arr = new Array<number>();
  }

  insert(val: number): boolean {
    if (this.map.has(val)) return false;
    this.map.set(val, this.arr.length);
    this.arr.push(val);
    return true;
  }

  remove(val: number): boolean {
    const valIndex: number | undefined = this.map.get(val);
    if (valIndex === undefined) return false;
    const lastIndex = this.arr.length - 1;
    const lastVal = this.arr[lastIndex];
    this.map.set(lastVal, valIndex);
    this.map.delete(val);
    this.arr[valIndex] = lastVal;
    this.arr.pop();
    return true;
  }

  getRandom(): number {
    return this.arr[Math.floor(Math.random() * this.arr.length)];
  }
}
