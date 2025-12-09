import fs from "node:fs";

const coordinates = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [x, y] = line.split(",").map(Number);
    return {x, y};
});

// Looking at the drawn SVG, it makes a circle with a “trench” along the middle.
//   ______
//  /      \
// /        \
// -------+ |
// -------+ |
// \        /
//  \______/
//
// The largest contained rectangle must start from the either of the right corners of that trench.
// Here we find those corners (lower and upper):
const sorted = coordinates
    .map((coordinate, index) => ({coordinate, xDistance: Math.abs(coordinate.x - coordinates[(index+1)%coordinates.length].x)}))
    .toSorted((a, b) => b.xDistance - a.xDistance)
    ;
const extreme = sorted[0];
const extremeIndex = coordinates.findIndex(coordinate => coordinate.x === extreme.coordinate.x && coordinate.y === extreme.coordinate.y);
const lower = coordinates[extremeIndex + 1];
const upper = coordinates[extremeIndex + 2];

const xs = coordinates.map(({x}) => x);
const ys = coordinates.map(({y}) => y);

const minX = Math.min(...xs);
const maxX = Math.max(...xs);
const minY = Math.min(...ys);
const maxY = Math.max(...ys);

const svg = `
<svg id="svg" viewBox="${minX} ${minY} ${maxX - minX} ${maxY - minY}" xmlns="http://www.w3.org/2000/svg" style="width: 95vw; height: 95vh;">
    <rect id="rect" x="0" y="0" width="0" height="0" fill="magenta" />
    <polygon id="polygon" points="${coordinates.map(({x, y}) => `${x},${y}`)}" />
</svg>
`.trim();
        
const html = `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Tiles</title>
</head>
<body>
    ${svg}
    <script>
        const coordinates = ${JSON.stringify(coordinates)};
        const lower = ${JSON.stringify(lower)};
        const upper = ${JSON.stringify(upper)};
        const candidates = [];
        for (const coordinate of coordinates) {
            if (coordinate.x < lower.x && coordinate.y > lower.y) {
                setRect(lower, coordinate);
                if (isContained()) {
                    candidates.push([lower, coordinate]);
                }
            } else if (coordinate.x < upper.x && coordinate.y < upper.y) {
                setRect(upper, coordinate);
                if (isContained()) {
                    candidates.push([upper, coordinate]);
                }
            }
        };
        const withArea = candidates.map(([a, b]) => ({a, b, area: area(a, b)})).sort((a, b) => b.area - a.area);
        const largest = withArea[0];
        setRect(largest.a, largest.b);
        svg.append(rect);
        document.body.prepend(largest.area);

        function setRect(a, b) {
            rect.setAttribute("x", Math.min(a.x, b.x));
            rect.setAttribute("y", Math.min(a.y, b.y));
            rect.setAttribute("width", Math.abs(a.x - b.x));
            rect.setAttribute("height", Math.abs(a.y - b.y));
        }

        function isContained() {
            const r = rect.getBoundingClientRect();
            const corners = [
                [Math.ceil(r.left), Math.ceil(r.top)],
                [Math.floor(r.right), Math.ceil(r.top)],
                [Math.floor(r.right), Math.floor(r.bottom)],
                [Math.ceil(r.left), Math.floor(r.bottom)],
            ];
            return corners.every(([x, y]) => document.elementFromPoint(x, y) === polygon);
        }

        function area(a, b) {
            return (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1);
        }
    </script>
</body>
</html>
`.trim();
        
console.log(html);
