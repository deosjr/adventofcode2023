package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "strconv"

    "github.com/deosjr/Pathfinding/path"
)

type coord struct { x, y int }

func (p coord) add(q coord) coord {
    return coord{p.x+q.x, p.y+q.y}
}

func left(dir coord) coord {
    return coord{dir.y, -1*dir.x}
}

func right(dir coord) coord {
    return coord{-1*dir.y, dir.x}
}

type grid struct {
    m map[coord]float64
}

func (g *grid) Neighbours(n node) []node {
    nodes := []node{}
    d := n.dir
    l := left(d)
    r := right(d)
    leftnode := node{n.pos.add(l), l, 1}
    rightnode := node{n.pos.add(r), r, 1}
    straightnode := node{n.pos.add(n.dir), n.dir, n.steps+1}
    if n.steps < 4 {
        if !g.outOfBounds(straightnode.pos) {
            nodes = append(nodes, straightnode)
        }
        return nodes
    }
    if !g.outOfBounds(leftnode.pos) {
        nodes = append(nodes, leftnode)
    }
    if !g.outOfBounds(rightnode.pos) {
        nodes = append(nodes, rightnode)
    }
    if n.steps == 10 {
        return nodes
    }
    if !g.outOfBounds(straightnode.pos) {
        nodes = append(nodes, straightnode)
    }
    return nodes
}

func (g *grid) G(n, neighbour node) float64 {
    return g.m[neighbour.pos]
}

func (g *grid) H(p, q node) float64 {
    return math.Abs(float64(q.pos.x-p.pos.x)) + math.Abs(float64(q.pos.y-p.pos.y))
}

func (g *grid) outOfBounds(c coord) bool {
    _, ok := g.m[c]
    return !ok
}

type node struct {
    pos coord
    dir coord
    steps int
}

func main() {
    //f, err := os.Open("test")
    f, err := os.Open("day17.input")
    if err != nil {
        panic(err)
    }
    m := &grid{map[coord]float64{}}
    y := 0
    maxX := 0
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        line := scanner.Text()
        maxX = len(line)-1
        for x, c := range line {
            n, err := strconv.ParseInt(string(c), 10, 64)
            if err != nil {
                panic(err)
            }
            m.m[coord{x,y}] = float64(n)
        }
        y++
    }
    maxY := y-1
    start := node{pos:coord{0,0}, dir:coord{1,0}, steps:0}
    goal := node{pos:coord{maxX, maxY}}
    p, err := path.FindRouteWithGoalFunc[node](m, start, goal, func(c, g node) bool {
        return c.pos == g.pos && c.steps >= 4
    })
    if err != nil {
        panic(err)
    }
    sum := 0
    for _, n := range p[1:len(p)-1] {
        sum += int(m.m[n.pos])
    }
    fmt.Println(sum)
}
