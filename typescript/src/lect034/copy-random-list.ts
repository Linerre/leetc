import { _Node } from './util.ts';

// Medium 138: https://leetcode.cn/problems/copy-list-with-random-pointer/
export function copyRandomList(head: _Node | null): _Node | null {
    if (head === null) return null;
    let cur: _Node | null = head;
    let next: _Node | null = null;
    // Transform a->b->c->d... to a->a'->b->b'->c->c'->d->d'->...
    // where a' is copy of a, without the random pointer setup and so on
    while (cur !== null) {
        next = cur.next;
        cur.next = new _Node(cur.val);
        cur.next.next = next;
        cur = next;
    }

    // go back to head
    cur = head;
    // now copy random pointers from a to a' for all copied nodes above
    let copy: _Node | null = null;
    while (cur !== null && cur.next) {
        // next will be b, c, d, etc
        next = cur.next.next;
        // copy will be 'a, b', c', d' etc
        copy = cur.next;
        // important!
        copy.random = cur.random ? cur.random.next : null;
        // move onto next
        cur = next;
    }
    const copyHead: _Node | null = head.next;

    // Go back to head and separate original nodes from copied nodes;
    cur = head;
    while (cur !== null && cur.next) {
        next = cur.next.next;
        copy = cur.next;
        cur.next = next;
        copy.next = next ? next.next : null;
        cur = next;
    }
    return copyHead;
};
