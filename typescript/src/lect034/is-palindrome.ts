import { ListNode } from './util.ts';

// Medium 234: leetcode.cn/problems/palindrome-linked-list/description/
/**
 * A simpler solution will be using a stack.
 */
export function isPalindrome(head: ListNode | null): boolean {
    if (head === null || head.next === null) return true;

    // Use fast/slow pointers (usually for middle point)
    let slow: ListNode | null = head;
    let fast: ListNode | null = head;

    while (fast && fast.next && fast.next.next) {
        slow = slow ? slow.next : null;
        fast = fast.next.next;
    }

    // now slow points to middle node
    // reverse nodes after slow
    let prev: ListNode | null = slow;
    let cur: ListNode | null = prev ? prev.next : null;
    // head -> ... -> slow/prev -> cur -> ... -> last;
    let next: ListNode | null = null;
    // important when there are even number of nodes
    if (prev) prev.next = null;
    while (cur) {
        next = cur.next;
        cur.next = prev;
        prev = cur;
        cur = next;
    }
    // now prev points to the last ListNode
    // head -> ... -> slow <- ... <- prev
    // cur will become null

    let isPalindrome: boolean = true;
    let left: ListNode | null = head;
    let right: ListNode | null = prev;

    while (left && right) {
        if (left.val !== right.val) {
            isPalindrome = false;
            // no early return because we need to restore the reversed list
            break;
        }
        left = left.next;
        right = right.next;
    }

    // restore the reversed past
    cur = prev ? prev.next : null;
    if (prev) prev.next = null;
    next = null;
    while (cur) {
        next = cur.next;
        cur.next = prev;
        prev = cur;
        cur = next;
    }

    return isPalindrome;
};
