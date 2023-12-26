package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "sort"
)

type coord struct { x, y int }

func (p coord) add(q coord) coord {
    return coord{p.x+q.x, p.y+q.y}
}

var (
    origin  = coord{0, 0}
    up      = coord{0, -1}
    down    = coord{0, 1}
    left    = coord{-1, 0}
    right   = coord{1, 0}
    dirs    = []coord{right, down, left, up}
)

type instruction struct {
    dir coord
    n int
}

type line struct {
    from, to coord
}

var (
    minX = math.MaxInt32
    minY = math.MaxInt32
    maxX = math.MinInt32
    maxY = math.MinInt32
)

func drawBoundary(instrs []instruction) []line {
    var lines []line
    c := origin
    for _, instr := range instrs {
        next := c.add(coord{instr.dir.x * instr.n, instr.dir.y * instr.n})
        // guarantee vertical lines point downwards
        if instr.dir == up {
            lines = append(lines, line{next, c})
        }
        if instr.dir == down {
            lines = append(lines, line{c, next})
        }
        c = next
    }
    return lines
}

// takes vertical lines, calculates area
func fillInterior(lines []line) int {
    sum := 0
    for len(lines) > 0 {
        // find all lines with minX
        sort.Slice(lines, func(i, j int) bool {
            return lines[i].from.x < lines[j].from.x
        })
        // find next smallest X (call it nsX)
        smallest, nsX, rest := findLeftmostLines(lines)
        if rest == nil {
            break
        }
        // push all found lines up to nsX, add area
        delta := nsX - smallest[0].from.x
        var pushed []line
        for _, l := range smallest {
            sum += (l.to.y - l.from.y + 1) * (delta + 1)
            pushed = append(pushed, line{from:coord{l.from.x + delta, l.from.y}, to:coord{l.to.x + delta, l.to.y}})
        }
        // add/subtract lines at nsX, repeat
        nextSmallest, _, rem := findLeftmostLines(rest)
        joined, _ := joinLines(pushed, nextSmallest)
        _, cutLen := joinLines(joined, smallest)
        fmt.Println(sum, cutLen)
        sum -= cutLen
        lines = append(rem, joined...)
    }
    return sum
}

func lineLengths(lines []line) int {
    sum := 0
    for _, l := range lines {
        sum += l.to.y - l.from.y + 1
    }
    return sum
}

func findLeftmostLines(lines []line) (leftmost []line, nsX int, rest []line) {
    minX := lines[0].from.x
    for i, l := range lines {
        if l.from.x == minX {
            leftmost = append(leftmost, l)
            continue
        }
        nsX = l.from.x
        rest = lines[i:]
        break
    }
    return leftmost, nsX, rest
}

// all have same x coord, but there might be overlap along the y axis
// a and b internally dont overlap, only overlap is between them
// we need to take the overlap out and return the rest, more like a zip
// also returns length of overlap cut
func joinLines(a, b []line) ([]line, int) {
    sort.Slice(a, func(i, j int) bool {
        return a[i].from.y < a[j].from.y
    })
    sort.Slice(b, func(i, j int) bool {
        return b[i].from.y < b[j].from.y
    })
    var joined []line
    cut := 0
    for len(a) > 0 && len(b) > 0 {
        switch {
        // bfrom - bto - afrom - ato
        case b[0].to.y < a[0].from.y:
            joined = append(joined, b[0])
            b = b[1:]
        // afrom - ato - bfrom - bto
        case b[0].from.y > a[0].to.y:
            joined = append(joined, a[0])
            a = a[1:]
        // afrom - bfrom - ato - bto
        case b[0].to.y >= a[0].to.y && b[0].from.y >= a[0].from.y:
            cut += a[0].to.y - b[0].from.y + 1
            if a[0].from.y != b[0].from.y {
                joined = append(joined, line{a[0].from, b[0].from})
            }
            if a[0].to != b[0].to {
                b = append([]line{{a[0].to, b[0].to}}, b[1:]...)
            } else {
                b = b[1:]
            }
            a = a[1:]
        // afrom - bfrom - bto - ato
        case b[0].to.y <= a[0].to.y && b[0].from.y >= a[0].from.y:
            cut += b[0].to.y - b[0].from.y + 1
            if a[0].from.y != b[0].from.y {
                joined = append(joined, line{a[0].from, b[0].from})
            }
            if b[0].to.y != a[0].to.y {
                a = append([]line{{b[0].to, a[0].to}}, a[1:]...)
            } else {
                a = a[1:]
            }
            b = b[1:]
        // bfrom - afrom - bto - ato
        case b[0].to.y <= a[0].to.y && b[0].from.y <= a[0].from.y:
            cut += b[0].to.y - a[0].from.y + 1
            if b[0].from.y != a[0].from.y {
                joined = append(joined, line{b[0].from, a[0].from})
            }
            if b[0].to.y != a[0].to.y {
                a = append([]line{{b[0].to, a[0].to}}, a[1:]...)
            } else {
                a = a[1:]
            }
            b = b[1:]
        // bfrom - afrom - ato - bto
        case b[0].to.y >= a[0].to.y && b[0].from.y <= a[0].from.y:
            cut += a[0].to.y - a[0].from.y + 1
            if b[0].from.y != a[0].from.y {
                joined = append(joined, line{b[0].from, a[0].from})
            }
            if a[0].to.y != b[0].to.y {
                b = append([]line{{a[0].to, b[0].to}}, b[1:]...)
            } else {
                b = b[1:]
            }
            a = a[1:]
        }
    }
    joined = append(joined, a...)
    joined = append(joined, b...)
    return simplify(joined), cut
}

func max(x, y int) int { if x > y { return x }; return y }

func simplify(lines []line) []line {
    if len(lines) < 2 {
        return lines
    }
    sort.Slice(lines, func(i, j int) bool {
        return lines[i].from.y < lines[j].from.y
    })
    var out []line
    candidate := lines[0]
    for _, l := range lines[1:] {
        if candidate.to.y == l.from.y {
            candidate.to.y = l.to.y
            continue
        }
        out = append(out, candidate)
        candidate = l
    }
    out = append(out, candidate)
    return out
}

func findMinMax() {
    minY, maxY = 0, 9
    minX, maxX = 0, 6
}

func main() {
    //f, err := os.Open("test")
    f, err := os.Open("day18.input")
    if err != nil {
        panic(err)
    }
    scanner := bufio.NewScanner(f)
    var instrs []instruction
    for scanner.Scan() {
        var instr rune
        var n, trueN, trueDir int
        fmt.Sscanf(scanner.Text(), "%c %d (#%5x%x)", &instr, &n, &trueN, &trueDir)
        var dir coord
        switch instr {
        case 'U':
            dir = up
        case 'D':
            dir = down
        case 'L':
            dir = left
        case 'R':
            dir = right
        }
        dir, n = dirs[trueDir], trueN
        instrs = append(instrs, instruction{dir, n})
    }

    lines := drawBoundary(instrs)
    findMinMax()
    ans := fillInterior(lines)
    fmt.Println(ans)
}
