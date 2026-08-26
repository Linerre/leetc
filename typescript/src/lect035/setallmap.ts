type TimestampedValue = [number, number];

export class SetAllMap {
    private map = new Map<number, TimestampedValue>();
    private setAllValue = 0;
    private setAllTimestamp = -1;
    private cnt = 0;

    put(k: number, v: number): void {
        if (this.map.has(k)) {
            const value: TimestampedValue | undefined = this.map.get(k);
            if (value) {
                value[0] = v;
                value[1]++;
            }
        } else {
            this.map.set(k, [v, this.cnt++]);
        }
    }

    setAll(v: number): void {
        this.setAllValue = v;
        this.setAllTimestamp = this.cnt++;
    }

    get(k: number): number {
        if (!this.map.has(k)) return -1;
        const value: TimestampedValue | undefined = this.map.get(k);
        if (value && value[1] > this.setAllTimestamp) return value[0];
        else return this.setAllValue;
    }
}
