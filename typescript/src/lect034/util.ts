/**
 * Definition for singly-linked list.
 */

export class ListNode {
    val: number
    next: ListNode | null
    constructor(val?: number, next?: ListNode | null) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
    }
}

/**
 * Definition for _Node.
 */
export class _Node {
    val: number
    next: _Node | null
    random: _Node | null

    constructor(val?: number, next?: _Node, random?: _Node) {
        this.val = (val===undefined ? 0 : val)
        this.next = (next===undefined ? null : next)
        this.random = (random===undefined ? null : random)
    }
}


/**
 * A navie, stupid implementation that only updates head's next in each loop
 * function makeListFromArray(nums: number[]): ListNode | null {
 *     const head =  new ListNode(0, null);
 *     nums.reduce(
 *         (accNode, num) => {
 *             accNode!.next = new ListNode(num, null);
 *             return accNode;
 *         },
 *         head
 *     );
 *     console.log(head);
 *     return head ? head.next : null;
 * }
 */

// Helpers to verify correctness
export function makeListFromArray(nums: number[]): ListNode | null {
    if (nums.length < 1) return null;

    let head = new ListNode(0, null);
    let h: ListNode | null  = head;
    for (let i = 0; i < nums.length; i++) {
        if (i === 0) {
            head.val = nums[i];
            continue;
        }
        h.next = new ListNode(nums[i], null);
        h = h.next;
    }
    return head;
}

// [val, node_index]
export type NodePairs = [number, number | null][];
export function makeListWithRandomFromArray(pairs: NodePairs): _Node | null {
    if (pairs.length < 1) return null;

    const head = new _Node(pairs[0][0]);
    let prev: _Node = head;
    // create the linked-list without random pointers
    for (let i = 1; i < pairs.length && prev; i++) {
        prev.next = new _Node(pairs[i][0]);
        prev = prev.next;
    }

    // set up random pointer for each node in the list
    let h: _Node | null = head;
    for (let i = 0; i < pairs.length; i++) {
        const [val, randome_index] = pairs[i];
        // find current node
        let cnt = 0;
        let cur: _Node | null = head;
        while (cur !== null && cnt < i) {
            cur = cur.next;
            cnt++;
        }

        // find random node
        cnt = 0;
        if (typeof randome_index === 'number') {
            let random: _Node | null = head;
            while (random !== null && cnt < randome_index) {
                cnt++;
                random = random.next;
            }

            // set up random for cur
            if (cur) cur.random = random;
        }
    }
    return head;
}

export function makeListFromArrayWithRing(vals: number[], pos?: number): ListNode | null {
    if (vals.length < 1) return null;

    let head = new ListNode(0, null);
    let h: ListNode | null  = head;
    for (let i = 0; i < vals.length; i++) {
        if (i === 0) {
            head.val = vals[i];
            continue;
        }
        h.next = new ListNode(vals[i], null);
        h = h.next;
    }

    // impossible to have a ring in the list
    if (vals.length < 2) return head;

    if (typeof pos === 'number' && pos > 0) {
        let cnt = 0;
        let t: ListNode | null = null;
        h = head;
        while (cnt < pos && h) {
            cnt++;
            h = h.next;
        }
        // find tail
        t = h;
        while (t && t.next) {
            t = t.next;
        }
        // connect tail's next to h to create the ring
        t!.next = h;
    }
    return head;
}

export function intersect(
    headA: ListNode | null,
    headB: ListNode | null,
    skipA: number,
    skipB: number
): boolean {
    if (headA === null || headB === null) return false;

    let a = headA;
    let b = headB;
    let skip = 0;
    // At most a moves to the last node
    while (skip < skipA && a.next) {
        a = a.next;
        skip++;
    }
    skip = 0;
    while (skip < skipB - 1 && b.next) {
        b = b.next;
        skip++;
    }

    // no intersection
    if (b.next === null) return false;

    // intersect at the node with the same value. A: 1->2->5 A: 4->7->5
    // if skipA === skipB === 3, then the two lists will still
    // intersect even your intention is different. To pretend they are
    // not intersected, skipA must not equal skipB. This has nothing to
    // do with the solution, just to make the simulation correct.
    if (a.val === b.next.val) {
        b.next = a;
        return true;
    } else {
        return false;
    }
}

export function skipToNode(head: ListNode | null, skip: number): number | null {
    if (head == null) return null;

    let a = head;
    let cnt = 0;
    while (cnt < skip && a.next) {
        a = a.next;
        cnt++;
    }
    return a.val;
}

export function printList(head: ListNode | null): number[] {
    if (!head) return [];
    const vals: number[] = [];
    while(head) {
        vals.push(head.val);
        head = head.next;
    }
    return vals;
}

export function printListWithRandom(head: _Node | null): NodePairs {
    if (!head) return [];
    const pairs: NodePairs = [];
    let index = 0;
    let cur: _Node | null = head;
    let h: _Node | null = head;
    while(cur) {
        const random = cur.random;
        if (random) {
            while(h) {
                if (random.val === h.val) break;
                index++;
                if (h) h = h.next;
            }
            // console.log([cur.val, index])
            pairs.push([cur.val, index]);
        } else {
            pairs.push([cur.val, null]);
        }
        index = 0;
        h = head;
        if (cur) cur = cur.next;
    }
    return pairs;
}
