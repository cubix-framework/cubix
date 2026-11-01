// Interfaces and object types: separators, trailing separators.
export interface Student {
  name: string;
  scores: number[];
}

type Point = { x: number, y: number, };
type Fn = (a: string, b?: number) => void;
let u: string | undefined = undefined;
const n: null = null;
