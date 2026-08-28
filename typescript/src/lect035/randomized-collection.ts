// Hard 381: https://leetcode.cn/problems/insert-delete-getrandom-o1-duplicates-allowed/description/
export class RandomizedCollection {
  private map: Map<number, Set<number>>;
  private arr: number[];

  constructor() {
    this.map = new Map<number, Set<number>>();
    this.arr = new Array<number>();
  }

  insert(val: number): boolean {
    let set: Set<number> | undefined = this.map.get(val);
    if (set === undefined) {
      set = new Set([this.arr.length]);
      this.map.set(val, set);
    } else {
      set.add(this.arr.length);
    }
    this.arr.push(val);
    return set.size === 1;
  }

  remove(val: number): boolean {
    const valSet: Set<number> | undefined = this.map.get(val);
    if (valSet === undefined) return false;

    const anyValIndex = valSet.values().next().value;
    if (anyValIndex === undefined) return false;

    const lastIndex = this.arr.length - 1;
    const lastVal = this.arr[lastIndex];
    // if val happens to appear at the end of arr, we remove this one
    if (val === lastVal) {
      valSet.delete(lastIndex);
    } else {
      this.arr[anyValIndex] = this.arr[lastIndex];
      const lastValSet: Set<number> | undefined = this.map.get(lastVal);
      if (lastValSet) {
        lastValSet.add(anyValIndex);
        lastValSet.delete(lastIndex);
        valSet.delete(anyValIndex);
      }
    }
    this.arr.pop();

    // If after removal, val has no presence at all, remove it from map
    if (valSet.size === 0) this.map.delete(val);
    return true;
  }

  getRandom(): number {
    return this.arr[Math.floor(Math.random() * this.arr.length)];
  }
}
