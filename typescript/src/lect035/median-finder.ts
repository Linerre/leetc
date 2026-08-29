import { PriorityQueue } from '@datastructures-js/priority-queue';
export class MedianFinder {
  private minHeap: PriorityQueue<number>;
  private maxHeap: PriorityQueue<number>;

  constructor() {
    this.minHeap = new PriorityQueue<number>((a, b) => a - b);
    this.maxHeap = new PriorityQueue<number>((a, b) => b - a);
  }

  addNum(num: number): void {
    const maxTop: number | null = this.maxHeap.front();
    // In JavaScript, if maxTop is 0, it evaluates to false
    if (this.maxHeap.isEmpty() || (maxTop !== null && maxTop >= num)) {
      this.maxHeap.enqueue(num);
    } else {
      this.minHeap.enqueue(num);
    }
    this.balance();
  }

  findMedian(): number {
    const minTop = this.minHeap.front();
    const maxTop = this.maxHeap.front();
    if (this.minHeap.size() === this.maxHeap.size()) {
      if (minTop !== null && maxTop !== null) return (minTop + maxTop) / 2;
    } else {
      return this.minHeap.size() > this.maxHeap.size() ? (minTop as number) : (maxTop as number);
    }
    return 0;
  }


  balance(): void {
    if (Math.abs(this.minHeap.size() - this.maxHeap.size()) === 2) {
      if (this.minHeap.size() > this.maxHeap.size()) {
        const minTop = this.minHeap.dequeue();
        if (minTop !== null) this.maxHeap.enqueue(minTop);
      } else {
        const maxTop = this.maxHeap.dequeue();
        if (maxTop !== null) this.minHeap.enqueue(maxTop);
      }
    }
  }
}
