// Template strings keep their text (hidden external _template_chars).
function greet(who: string): string {
  const s = `hello ${who}\n!`;
  const r = /ab+c/gi;
  const t = this;
  return s ?? who?.trim() ?? `${t}`;
}

for (let i = 0; i < 3; i++) {
  console.log(i > 1 ? "big" : "small")
}
