package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

type Node struct {
	Links         []*Node
	CachedResults map[*Node]int
}

func main() {
	start := time.Now()
	contents, err := os.ReadFile("puzzle.txt")
	if err != nil {
		panic(err)
	}

	nodes := map[string]*Node{}
	for line := range strings.SplitSeq(string(contents), "\n") {
		if line == "" {
			continue
		}

		nameAndChildren := strings.SplitN(line, ": ", 2)
		name := nameAndChildren[0]
		node := nodes[name]
		if node == nil {
			node = &Node{CachedResults: map[*Node]int{}}
			nodes[name] = node
		}

		for childName := range strings.SplitSeq(nameAndChildren[1], " ") {
			childNode := nodes[childName]
			if childNode == nil {
				childNode = &Node{CachedResults: map[*Node]int{}}
				nodes[childName] = childNode
			}
			node.Links = append(node.Links, childNode)
		}
	}

	v := &Visitor{
		OUT: nodes["out"],
		DAC: nodes["dac"],
		FFT: nodes["fft"],
	}

	you := nodes["you"]
	if you != nil {
		answerP1 := v.FindOutsViaDac(you, v.OUT)
		fmt.Println("p1:", answerP1)
	}

	svr := nodes["svr"]
	if svr != nil {
		answerP2 := v.FindOutsViaDac(svr, nil)
		fmt.Println("p2:", answerP2)
	}

	fmt.Println("in", time.Since(start))
}

type Visitor struct {
	OUT *Node
	DAC *Node
	FFT *Node
}

func (v *Visitor) FindOutsViaDac(node *Node, findNext *Node) int {
	resultsCache, ok := node.CachedResults[findNext]
	if ok {
		return resultsCache
	}

	resp := 0
	for _, link := range node.Links {
		switch link {
		case v.DAC:
			switch findNext {
			case nil:
				resp += v.FindOutsViaDac(link, v.FFT)
			case v.DAC:
				resp += v.FindOutsViaDac(link, v.OUT)
			}
		case v.FFT:
			switch findNext {
			case nil:
				resp += v.FindOutsViaDac(link, v.DAC)
			case v.FFT:
				resp += v.FindOutsViaDac(link, v.OUT)
			}
		case v.OUT:
			if findNext == v.OUT {
				resp += 1
			}
		default:
			resp += v.FindOutsViaDac(link, findNext)
		}
	}

	node.CachedResults[findNext] = resp
	return resp
}
