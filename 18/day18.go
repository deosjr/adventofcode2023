package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
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
)

type instruction struct {
    dir coord
    n int
    c int
}

type edge uint8

const (
    none edge = iota
    horizontal
    vertical
    northwest
    northeast
    southwest
    southeast
    fill
)

var (
    m = map[coord]edge{}
    colours = map[coord]int{}
    minX = math.MaxInt32
    minY = math.MaxInt32
    maxX = math.MinInt32
    maxY = math.MinInt32
)

func drawBoundary(instrs []instruction) {
    pos := origin
    dir := origin // as null value
    for _, instr := range instrs {
        prevdir := dir
        prevpos := pos
        dir = instr.dir
        for i:=0; i<instr.n; i++ {
            pos = pos.add(instr.dir)
            if dir == up || dir == down {
                m[pos] = vertical
                continue
            }
            m[pos] = horizontal
            colours[pos] = instr.c
        }
        if prevdir == origin {
            continue
        }
        m[prevpos] = getCorner(prevdir, dir)
    }
    m[origin] = getCorner(dir, instrs[0].dir)
}

func getCorner(prevdir, dir coord) edge {
    switch {
    case prevdir == up && dir == right:
        return southeast
    case prevdir == up && dir == left:
        return southwest
    case prevdir == down && dir == right:
        return northeast
    case prevdir == down && dir == left:
        return northwest
    case prevdir == left && dir == up:
        return northeast
    case prevdir == left && dir == down:
        return southeast
    case prevdir == right && dir == up:
        return northwest
    case prevdir == right && dir == down:
        return southwest
    }
    panic("corner")
}

func fillInterior() {
    for y:=minY; y<=maxY; y++ {
        crossed := 0
        corner := none
        for x:=minX; x<=maxX; x++ {
            if edge, ok := m[coord{x,y}]; ok {
                if corner == northeast {
                    switch edge {
                    case horizontal:
                        continue
                    case northwest:
                        break
                    case southwest:
                        crossed++
                    default:
                        panic("unexpected edge")
                    }
                    corner = none
                    continue
                }
                if corner == southeast {
                    switch edge {
                    case horizontal:
                        continue
                    case northwest:
                        crossed++
                    case southwest:
                        break
                    default:
                        panic("unexpected edge")
                    }
                    corner = none
                    continue
                }
                switch edge {
                case vertical:
                    crossed++
                case northeast, southeast:
                    corner = edge
                    continue
                default:
                    panic("unexpected edge")
                }
                continue
            }
            if crossed % 2 == 0 {
                continue
            }
            m[coord{x,y}] = fill
        }
    }
}

func findMinMax() {
    for c, _ := range m {
        if c.x < minX {
            minX = c.x
        }
        if c.x > maxX {
            maxX = c.x
        }
        if c.y < minY {
            minY = c.y
        }
        if c.y > maxY {
            maxY = c.y
        }
    }
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
        var n, colour int
        fmt.Sscanf(scanner.Text(), "%c %d (#%x)", &instr, &n, &colour)
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
        instrs = append(instrs, instruction{dir, n, colour})
    }

    drawBoundary(instrs)
    findMinMax()
    fillInterior()

    sum := 0
    /*
    minX = 80
    maxX = 120
    maxY = -200
    */
    for y:=minY; y<=maxY; y++ {
        for x:=minX; x<=maxX; x++ {
            if edge, ok := m[coord{x,y}]; ok {
                switch edge {
                case horizontal:
                    fmt.Print("-")
                case vertical:
                    fmt.Print("|")
                case northwest:
                    fmt.Print("J")
                case northeast:
                    fmt.Print("L")
                case southwest:
                    fmt.Print("7")
                case southeast:
                    fmt.Print("F")
                case fill:
                    fmt.Print("#")
                }
                sum++
                continue
            }
            fmt.Print(".")
        }
        fmt.Println()
    }
    fmt.Println(sum)
}
