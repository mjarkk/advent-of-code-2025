package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"
)

const (
	TOTAL_BUCKETS = 10
	BUCKET_SIZE   = 100_000 / TOTAL_BUCKETS
)

type Network struct {
	Parent *Network
}

func (n *Network) Root() *Network {
	if n.Parent == nil {
		return n
	}

	root := n.Parent.Root()
	if root != n.Parent {
		n.Parent = root
	}
	return root
}

type JunctionBox struct {
	X       uint64
	Cord    [3]float64
	Network *Network
}

type Link struct {
	Distance float64
	A        *JunctionBox
	B        *JunctionBox
}

func main() {
	start := time.Now()
	mainStart := start
	contents, err := os.ReadFile("puzzle.txt")
	if err != nil {
		panic(err)
	}

	boxes := [TOTAL_BUCKETS][TOTAL_BUCKETS][TOTAL_BUCKETS][]*JunctionBox{}
	totalNetworks := uint16(0)
	links := make([]Link, 0, 6_000)
	for line := range strings.SplitSeq(string(contents), "\n") {
		if line == "" {
			break
		}

		off := 0
		cord := [3]uint64{}
		for numStr := range strings.SplitSeq(line, ",") {
			num, _ := strconv.ParseUint(numStr, 10, 64)
			cord[off] = num
			off++
		}

		box := &JunctionBox{
			X: cord[0],
			Cord: [3]float64{
				float64(cord[0]),
				float64(cord[1]),
				float64(cord[2]),
			},
			Network: &Network{},
		}

		xk, yk, zk := boxKey(cord)
		for i := xk - 1; i <= xk+1; i++ {
			if i < 0 || i >= TOTAL_BUCKETS {
				continue
			}
			for j := yk - 1; j <= yk+1; j++ {
				if j < 0 || j >= TOTAL_BUCKETS {
					continue
				}
				for k := zk - 1; k <= zk+1; k++ {
					if k < 0 || k >= TOTAL_BUCKETS {
						continue
					}
					for _, boxNeedle := range boxes[i][j][k] {
						distance := distrance(box, boxNeedle)
						if distance < 15_000.0 {
							links = append(links, Link{
								Distance: distance,
								A:        box,
								B:        boxNeedle,
							})
						}
					}
				}
			}
		}

		boxes[xk][yk][zk] = append(boxes[xk][yk][zk], box)
		totalNetworks++
	}

	slices.SortFunc(links, func(a Link, b Link) int {
		if a.Distance < b.Distance {
			return -1
		}
		if a.Distance > b.Distance {
			return 1
		}
		return 0
	})

	for _, link := range links {
		if link.A.Network.Root() != link.B.Network.Root() {
			totalNetworks--

			link.B.Network.Root().Parent = link.A.Network.Root()
			if totalNetworks == 1 {
				answer := link.A.X * link.B.X
				fmt.Println("part2:", answer, answer == uint64(9003685096))
				break
			}
		}
	}

	fmt.Println("in", time.Since(mainStart))
}

func distrance(a *JunctionBox, b *JunctionBox) float64 {
	dx := a.Cord[0] - b.Cord[0]
	dy := a.Cord[1] - b.Cord[1]
	dz := a.Cord[2] - b.Cord[2]
	return math.Sqrt(dx*dx + dy*dy + dz*dz)
}

func boxKey(cord [3]uint64) (int8, int8, int8) {
	x := int8(cord[0] / BUCKET_SIZE)
	y := int8(cord[1] / BUCKET_SIZE)
	z := int8(cord[2] / BUCKET_SIZE)
	return x, y, z
}
