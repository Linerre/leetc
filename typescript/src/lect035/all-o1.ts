// Hard 432: https://leetcode.cn/problems/all-oone-data-structure/description/
export class Bucket {
  set: Set<string>;
  cnt: number;
  prev: Bucket | null;
  next: Bucket | null;

  constructor(s: string, c: number) {
    this.set = new Set<string>();
    this.set.add(s);
    this.cnt = c;
    this.prev = this.next = null;
  }

  /**
   * Insert a bucket `post` right after cur, after the insertion:
   * cur->post->next
   * cur<-post<-next
   * where next was cur.next but now post.next
   */
  static insert(cur: Bucket, post: Bucket): void {
	cur.next!.prev = post;
	post.next = cur.next;
	cur.next = post;
	post.prev = cur;
  }

  /**
   * Remove current bucket from the linked list. After the removal:
   * old->next
   * next<-old
   * where old was cur.prev and next was cur.next
   */
  static remove(cur: Bucket): void {
	cur.prev!.next = cur.next;
	cur.next!.prev = cur.prev;
  }
}


/**
 * There will always be two buckets, one on the leftmost and one on
 * the rightmost.  They represent key freq of 0 and MAX respectively
 * (larger than 5 * 10^4).  By designing so, given a bucket,
 * bucket.prev and bucket.next will always exist.  The string in these
 * two buckets are empty strings all the time.
 */
export class AllOne {
  head: Bucket;
  tail: Bucket;
  map: Map<string, Bucket>;
  
  constructor() {
    this.head = new Bucket('', 0);
    this.tail = new Bucket('', 2 << 31)
    // connect head and tail in the beginning
    this.head.next = this.tail;
    this.tail.prev = this.head;
    // but never record them in the map as they can be accessed by
    // this.head and this.tail
    this.map = new Map<string, Bucket>();
  }

  inc(key: string): void {
    let keyBucket: Bucket | null | undefined = this.map.get(key);
    if (keyBucket === undefined) {
      if (this.head.next && this.head.next.cnt === 1) {
        // there is already a bucket for freq 1, put new key there 
        this.map.set(key, this.head.next);
        this.head.next.set.add(key);
      } else {
        // need to create a bucket for this key for the first time
        keyBucket = new Bucket(key, 1);
        this.map.set(key, keyBucket);
        Bucket.insert(this.head, keyBucket);
      }
    } else {
      // key already inserted, inc its freq by first checking if the
      // immediate next bucket has cnt + 1.  If there is no such
      // bucket, create a new one for this key.
      if (keyBucket.next?.cnt === keyBucket.cnt + 1) {
        this.map.set(key, keyBucket.next);
        keyBucket.next.set.add(key);
      } else {
        const newBucket = new Bucket(key, keyBucket.cnt + 1);
        this.map.set(key, newBucket);
        Bucket.insert(keyBucket, newBucket);
      }
      // In either of above cases, key is mapped to a new bucket, so
      // we need to remove it from the old one.  If after removal, the
      // old bucket becomes empty, we also need to delete this bucket.
      keyBucket.set.delete(key);
      if (keyBucket.set.size === 0) Bucket.remove(keyBucket);
    }
  }

  dec(key: string): void {
    const keyBucket = this.map.get(key);
    if (keyBucket === undefined) return;

    if (keyBucket.cnt === 1) {
      this.map.delete(key);
    } else {
      // if bucket has an immediate left neighbor, put the new key in
      // that bucket; otherwise create a new bucket that is the
      // immediate left neighbor.
      if (keyBucket.prev && keyBucket.prev.cnt === keyBucket.cnt - 1) {
        this.map.set(key, keyBucket.prev);
        keyBucket.prev.set.add(key);
      } else {
        const newBucket = new Bucket(key, keyBucket.cnt - 1);
        this.map.set(key, newBucket);
        if (keyBucket.prev) Bucket.insert(keyBucket.prev, newBucket);
      }
    }
    // Remove the key from old bucket.  If after removal there is no
    // key left in that bucket, remove that bucket completely.
    keyBucket.set.delete(key);
    if (keyBucket.set.size === 0) Bucket.remove(keyBucket);
  }

  getMaxKey(): string {
    return this.tail.prev?.set.values().next().value ?? '';
  }
  
  getMinKey(): string {
    return this.head.next?.set.values().next().value ?? '';
  }
}

