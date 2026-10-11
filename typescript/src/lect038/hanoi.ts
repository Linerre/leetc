function hanoi(n: number): void {
  if (n > 0)
    f(n, 'A', 'C', 'B');
}

function f(pile: number, from: string, to: string, other: string): void {
  if (pile === 1) {
    console.log(`Move pile 1 from ${from} to ${to}`);
  } else {
    f(pile - 1, from, other, to);
    console.log(`Move pile ${pile} from ${from} to ${to}`);
    f(pile - 1, other, to, from);
  }
}

function main(): void {
  const n = 3;
  hanoi(3);
}

main();
