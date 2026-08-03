import { PriorityQueue } from '@datastructures-js/priority-queue';

/**
 **************************************************
 ****************** Use PriorityQueu **************
 **************************************************
 */

// Medium 2208: https://leetcode.cn/problems/minimum-operations-to-halve-array-sum/description/
export function halveArray1(nums: number[]): number {
    // Max priority queue
    const heap = new PriorityQueue<number>((a, b) => b - a);
    let sum = 0;
    nums.forEach((num) => {
        heap.enqueue(num);
        sum += num;
    });

    // Reduce by goal
    const goal = sum / 2;
    let count = 0;
    for (let amt = 0, cur: number | null; amt < goal; amt = cur !== null ? amt + cur : amt + 0, count++) {
        cur = heap.dequeue();
        if (cur) {
            cur = cur / 2;
            heap.enqueue(cur);
        }
    }
    return count;
}


/**
 **************************************************
 ****************** Use heap **********************
 **************************************************
 */
