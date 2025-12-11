package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

type Joltage = [10]uint16

type Machine struct {
	LightsSchema        uint16
	ButtonOptions       [][]uint16
	ButtonOptionsBinary []uint16
	JoltageRequirement  Joltage
}

func main() {
	data, err := os.ReadFile("puzzle.txt")
	if err != nil {
		panic(err)
	}

	var totalP1, totalP2 int
	machines := strings.Split(string(data), "\n")
	for idx, line := range machines {
		if line == "" {
			continue
		}

		fmt.Println("solving machine", idx+1, "of", len(machines))

		machine := Machine{}

		sections := strings.Split(line, " ")
		for idx, section := range sections {
			if idx == 0 {
				for j, c := range section[1:] {
					if c == '#' {
						machine.LightsSchema |= 1 << j
					}
				}
				continue
			}

			if idx == len(sections)-1 {
				for idx, num := range parseNumbers(section) {
					machine.JoltageRequirement[idx] = num
				}
				continue
			}

			machine.ButtonOptions = append(machine.ButtonOptions, parseNumbers(section))
			for _, options := range machine.ButtonOptions {
				var binaryOptions uint16
				for _, option := range options {
					binaryOptions ^= 1 << option
				}
				machine.ButtonOptionsBinary = append(machine.ButtonOptionsBinary, binaryOptions)
			}
		}

		p1, p2 := machine.Solve()
		totalP1 += p1
		totalP2 += p2
	}

	fmt.Println(totalP1, totalP2)
}

func parseNumbers(in string) []uint16 {
	nums := []uint16{}
	for numStr := range strings.SplitSeq(in[1:len(in)-1], ",") {
		num, err := strconv.ParseUint(numStr, 10, 16)
		if err != nil {
			panic(err)
		}
		nums = append(nums, uint16(num))
	}
	return nums
}

type Null struct{}

var null = Null{}

func (m *Machine) Solve() (int, int) {
	return m.SolveP1(map[uint16]Null{uint16(0): null}), m.SolveP2()
}

func (m *Machine) SolveP1(states map[uint16]Null) int {
	newStates := map[uint16]Null{}
	for _, option := range m.ButtonOptionsBinary {
		for state := range states {
			newState := state ^ option
			if newState == m.LightsSchema {
				return 1
			}
			newStates[newState] = null
		}
	}

	return 1 + m.SolveP1(newStates)
}

func (m *Machine) SolveP2() int {
	start := time.Now()
	states := map[Joltage]Null{{}: null}
	newStates := map[Joltage]Null{}

	it := 0
	for {
		it++

		if it > 15 {
			fmt.Println(it, len(states), time.Since(start))
		}

		for _, option := range m.ButtonOptions {
		outer1:
			for state := range states {
				newState := state
				for _, offset := range option {
					newState[offset] += 1
				}
				if newState == m.JoltageRequirement {
					return it
				}
				for idx, num := range newState {
					if num > m.JoltageRequirement[idx] {
						continue outer1
					}
				}
				newStates[newState] = null
			}
		}

		states = newStates
		newStates = make(map[Joltage]Null, len(states)*2)
	}
}
