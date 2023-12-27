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
)

type coord struct { x, y int }

func (p coord) add(q coord) coord {
    return coord{p.x+q.x, p.y+q.y}
}

func check(c, dir coord, m map[coord]int) (coord, bool) {
    p := c.add(dir)
    if pass, ok := grid[p]; !pass || !ok {
        return coord{}, false
    }
    if _, ok := m[p]; ok {
        return coord{}, false
    }
    return p, true
}

func plotsReachable(start coord, steps int, odds bool) int {
    m := map[coord]int{}
    m[start] = 0
    fringe := []coord{start}
    for i:=1; i<=steps; i++ {
        newfringe := []coord{}
        for _, c := range fringe {
            for _, dir := range dirs {
                if p, ok := check(c, dir, m); ok {
                    m[p] = i
                    newfringe = append(newfringe, p)
                }
            }
        }
        fringe = newfringe
    }
    sum := 0
    for _, n := range m {
        if (odds && n % 2 == 1) || (!odds && n % 2 == 0) {
            sum++
        }
    }
    return sum
}

// Assumption: input is an uneven length square grid
// and is surrounded by garden plots at the edges
func main() {
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
            c1 := coord{x,y}
            var passable bool
            switch c {
            case '.':
                passable = true
            case '#':
                passable = false
            case 'S':
                start = c1
                passable = true
            }
            grid[c1] = passable
        }
        y++
    }
    ans1 := plotsReachable(start, 64, false)
    fmt.Println("Part 1: ", ans1)

    steps := 26501365
    n := steps / 131
    //r := steps % 131
    nMin1 := n - 1

    startParity := plotsReachable(start, 5000, true)
    reverseParity := plotsReachable(start, 5000, false)
    pointUp := plotsReachable(coord{65, 130}, 130, false)
    pointDown := plotsReachable(coord{65, 0}, 130, false)
    pointLeft := plotsReachable(coord{130, 65}, 130, false)
    pointRight := plotsReachable(coord{0, 65}, 130, false)
    cornerBigNW := plotsReachable(coord{130, 130}, 131+64, true)
    cornerSmallNW := plotsReachable(coord{130, 130}, 64, false)
    cornerBigNE := plotsReachable(coord{0, 130}, 131+64, true)
    cornerSmallNE := plotsReachable(coord{0, 130}, 64, false)
    cornerBigSW := plotsReachable(coord{130, 0}, 131+64, true)
    cornerSmallSW := plotsReachable(coord{130, 0}, 64, false)
    cornerBigSE := plotsReachable(coord{0, 0}, 131+64, true)
    cornerSmallSE := plotsReachable(coord{0, 0}, 64, false)

    ans2 := n*n*reverseParity + nMin1*nMin1*startParity
    ans2 += pointUp + pointDown + pointLeft + pointRight
    ans2 += nMin1*cornerBigNW + n*cornerSmallNW
    ans2 += nMin1*cornerBigNE + n*cornerSmallNE
    ans2 += nMin1*cornerBigSW + n*cornerSmallSW
    ans2 += nMin1*cornerBigSE + n*cornerSmallSE
    fmt.Println("Part 2: ", ans2)
}
