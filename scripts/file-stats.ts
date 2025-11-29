#!/usr/bin/env bun

const paths = process.argv.slice(2);

const files = await Promise.all(
    paths.map(async path => {
        const text = await Bun.file(`prolog/${path}`)
            .text();
        const filtered = text.replace(/[\t ]/g, "");
        return {
            path,
            characters: filtered.length,
            lines: filtered.split("\n").length
        };
    })
);
const total = files.reduce(
    (acc, f) => ({
        characters: acc.characters + f.characters,
        lines: acc.lines + f.lines
    }),
    { characters: 0, lines: 0 }
);

console.log(JSON.stringify({ files, total }, null, 2));

