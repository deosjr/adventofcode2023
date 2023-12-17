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

func (g *grid) Neighbours(pn path.Node) []path.Node {
    nodes := []path.Node{}
    n := pn.(node)
    d := n.dir
    l := left(d)
    leftnode := node{n.pos.add(l), l, 1}
    if !g.outOfBounds(leftnode.pos) {
        nodes = append(nodes, leftnode)
    }
    r := right(d)
    rightnode := node{n.pos.add(r), r, 1}
    if !g.outOfBounds(rightnode.pos) {
        nodes = append(nodes, rightnode)
    }
    if n.steps == 3 {
        return nodes
    }
    straightnode := node{n.pos.add(n.dir), n.dir, n.steps+1}
    if !g.outOfBounds(straightnode.pos) {
        nodes = append(nodes, straightnode)
    }
    return nodes
}

func (g *grid) G(n, neighbour path.Node) float64 {
    return g.m[neighbour.(node).pos]
}

func (g *grid) H(n, goal path.Node) float64 {
    p, q := n.(node), goal.(node)
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
    start := node{coord{0,0}, coord{1,0}, 0}
    // TODO: separate goal-matching func ignoring fields in eq
    goals := []node{
        {coord{maxX, maxY}, coord{0,1}, 1},
        {coord{maxX, maxY}, coord{0,1}, 2},
        {coord{maxX, maxY}, coord{0,1}, 3},
        {coord{maxX, maxY}, coord{1,0}, 1},
        {coord{maxX, maxY}, coord{1,0}, 2},
        {coord{maxX, maxY}, coord{1,0}, 3},
    }
    ans := -1
    for _, g := range goals {
        p, err := path.FindRoute(m, start, g)
        if err != nil {
            continue
        }
        sum := 0
        for _, n := range p[:len(p)-1] {
            sum += int(m.m[n.(node).pos])
        }
        if ans == -1 {
            ans = sum
            continue
        }
        if ans > sum {
            ans = sum
        }
    }
    fmt.Println(ans)
}
