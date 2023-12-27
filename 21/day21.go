package main

import (
    "bufio"
    "fmt"
    "os"
)

var (
    up      = coord{0, -1}
    down    = coord{0, 1}
    left    = coord{-1, 0}
    right   = coord{1, 0}
    dirs    = []coord{right, down, left, up}
    grid    = map[coord]bool{}
    m       = map[coord]int{}
)

type coord struct { x, y int }

func (p coord) add(q coord) coord {
    return coord{p.x+q.x, p.y+q.y}
}

func check(c, dir coord) (coord, bool) {
    p := c.add(dir)
    if pass, ok := grid[p]; !pass || !ok {
        return coord{}, false
    }
    if _, ok := m[p]; ok {
        return coord{}, false
    }
    return p, true
}

func main() {
    //f, err := os.Open("test")
    f, err := os.Open("day21.input")
    if err != nil {
        panic(err)
    }
    y := 0
    var start coord
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        line := scanner.Text()
        for x, c := range line {
            switch c {
            case '.':
                grid[coord{x,y}] = true
            case '#':
                grid[coord{x,y}] = false
            case 'S':
                start = coord{x,y}
            }
        }
        y++
    }
    m[start] = 0
    fringe := []coord{start}
    for i:=1; i<=64; i++ {
        newfringe := []coord{}
        for _, c := range fringe {
            for _, dir := range dirs {
                if p, ok := check(c, dir); ok {
                    m[p] = i
                    newfringe = append(newfringe, p)
                }
            }
        }
        fringe = newfringe
    }
    sum := 0
    for _, n := range m {
        if n % 2 == 0 {
            sum++
        }
    }
    fmt.Println(sum)
}
