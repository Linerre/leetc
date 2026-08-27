// Medium 146: https://leetcode.cn/problems/lru-cache/description/
export class DoubleListNode {
    key: number;
    val: number;
    prev: DoubleListNode | null;
    next: DoubleListNode | null;

    constructor(key: number, val: number) {
        this.key = key;
        this.val = val;
        this.prev = this.next = null;
    }
}

export class DoubleList {
    private head: DoubleListNode | null;
    private tail: DoubleListNode | null;

    constructor() {
        this.head = this.tail = null;
    }

    addNode(newNode: DoubleListNode | null): void {
        if (newNode === null) return;
        if (this.head === null || this.tail === null) {
            this.head = newNode;
            this.tail = newNode;
        } else {
            this.tail.next = newNode;
            newNode.prev = this.tail;
            this.tail = newNode;
        }
    }

    moveNodeToTail(node: DoubleListNode | null): void {
        if (node === null) return;
        if (this.tail === null) return;
        if (node === this.tail) return;
        if (this.head && node === this.head) {
            this.head = node.next;
            this.head!.prev = null;
        } else {
            // node in between
            node.prev!.next = node.next;
            node.next!.prev = node.prev;
        }
        this.tail.next = node;
        node.prev = this.tail;
        node.next = null;
        this.tail = node;
    }

    removeHead(): DoubleListNode | null {
        if (this.head === null || this.tail === null) return null;
        const oldHead = this.head;
        if (this.head === this.tail) {
            this.head = null;
            this.tail = null;
        } else {
            this.head = oldHead.next;
            this.head!.prev = null;
            oldHead.next = null;
        }
        return oldHead;
    }
}


export class LRUCache {
    private keyNodeMap: Map<number, DoubleListNode>;
    private nodeList: DoubleList;
    readonly capacity: number;

    constructor(cap: number) {
        this.keyNodeMap = new Map<number, DoubleListNode>();
        this.nodeList = new DoubleList();
        this.capacity = cap;
    }

    get(key: number): number {
        const node: DoubleListNode | undefined = this.keyNodeMap.get(key);
        if (node) {
            this.nodeList.moveNodeToTail(node);
            return node.val;
        }
        return -1;
    }

    put(key: number, value: number): void {
        const node: DoubleListNode | undefined = this.keyNodeMap.get(key);
        if (node) {
            node.val = value;
            this.nodeList.moveNodeToTail(node);
        } else {
            if (this.keyNodeMap.size === this.capacity) {
                const node: DoubleListNode | null = this.nodeList.removeHead();
                const key = node?.key ?? undefined;
                if (key) this.keyNodeMap.delete(key);
            }
            const newNode = new DoubleListNode(key, value);
            this.keyNodeMap.set(key, newNode);
            this.nodeList.addNode(newNode);
        }
    }
}
