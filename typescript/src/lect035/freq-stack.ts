// Hard 895: https://leetcode.cn/problems/maximum-frequency-stack/
export class FreqStack {
  private topFreq: number;
  // each layer represents a frequency level: 1,2,3,etc
  private layers: Map<number, number[]>;
  // value to freq
  private valueFreqs: Map<number, number>;
  constructor() {
    this.topFreq = 0;
    this.layers = new Map<number, number[]>();
    this.valueFreqs = new Map<number, number>();
  }

  push(val: number): void {
    const valFreq = this.valueFreqs.get(val);
    if (valFreq === undefined) this.valueFreqs.set(val, 1);
    else this.valueFreqs.set(val, valFreq + 1);

    const curValFreq = this.valueFreqs.get(val);
    if (curValFreq === undefined) return;
    if (!this.layers.has(curValFreq)) {
      this.layers.set(curValFreq, new Array<number>());
    }
    const curFreqVals = this.layers.get(curValFreq);
    curFreqVals?.push(val);

    this.topFreq = Math.max(this.topFreq, curValFreq);
  }

  pop(): number {
    // take the top freq layer and pop the last item in it
    const topFreqVals = this.layers.get(this.topFreq);
    const ans = topFreqVals?.pop();
    if (ans === undefined) return -9999;
    // if there're no items left after popping, remove this layer
    if (topFreqVals?.length === 0) {
      this.layers.delete(this.topFreq);
      this.topFreq--;
    }
    // also update freq map to reduce the val's freq by 1 or remove
    // the val-freq pair if it appears only once
    const freq = this.valueFreqs.get(ans);
    if (freq === undefined) return -9999;
    if (freq === 1) {
      this.valueFreqs.delete(ans);
    } else {
      this.valueFreqs.set(ans, freq-1);
    }
    return ans;
  }
}
