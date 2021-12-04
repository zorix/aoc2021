package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

const boardSize = 5

type Board struct {
	fields [boardSize][boardSize]int
	marked [boardSize][boardSize]int
	win    bool
}

func (b *Board) mark(number int) {
main_loop:
	for y := range b.fields {
		for x := range b.fields[y] {
			if number == b.fields[y][x] {
				b.marked[y][x] = 1
				break main_loop
			}
		}
	}
}

func (b *Board) isDone() bool {
	for y := range b.marked {
		row := b.marked[y]
		sum := row[0] + row[1] + row[2] + row[3] + row[4]
		if sum == 5 {
			return true
		}
	}

	for x := range b.marked {
		sum := b.marked[0][x] + b.marked[1][x] + b.marked[2][x] + b.marked[3][x] + b.marked[4][x]
		if sum == 5 {
			return true
		}
	}

	return false
}

func (b *Board) sumUnmarked() int {
	sum := 0
	for y := range b.fields {
		for x := range b.fields[y] {
			if b.marked[y][x] == 0 {
				sum += b.fields[y][x]
			}
		}
	}

	return sum
}

func CreateBoard(lines [5]string) *Board {
	board := Board{
		fields: [5][5]int{},
		marked: [5][5]int{},
	}
	for y, l := range lines {
		board.fields[y] = [5]int{}
		board.marked[y] = [5]int{}
		fields := strings.Fields(l)
		//fmt.Printf("%v %v\n", y, fields)
		for x, f := range fields {
			val, _ := strconv.Atoi(f)
			board.fields[y][x] = val
		}
	}

	return &board
}

func main() {
	file, _ := ioutil.ReadFile("./input.txt")
	fileLines := strings.Split(string(file), "\n")

	lineReader := make(chan string)
	go func() {
		for _, l := range fileLines {
			lineReader <- l
		}
		close(lineReader)
	}()

	markers := strings.Split(<-lineReader, ",")

	boards := []*Board{}
	for {
		var ok bool
		if _, ok = <-lineReader; !ok {
			break
		}

		grid := [5]string{
			<-lineReader,
			<-lineReader,
			<-lineReader,
			<-lineReader,
			<-lineReader,
		}

		boards = append(boards, CreateBoard(grid))
	}

	//fmt.Printf("markers: %v\n", markers)
	fmt.Printf("%v\n", len(boards))
	var lastWin *Board = nil
	var lastNumber int = 0
	for _, m := range markers {
		for _, b := range boards {
			v, _ := strconv.Atoi(m)
			if !b.win {
				b.mark(v)
				if b.isDone() {
					b.win = true
					unmarkedSum := b.sumUnmarked()
					if lastWin == nil {
						fmt.Printf("%v %v %v PART1!\n", v, unmarkedSum, unmarkedSum*v)
					}

					lastWin = b
					lastNumber = v
				}
			}
		}
	}

	if lastWin != nil {
		unmarkedSum := lastWin.sumUnmarked()
		fmt.Printf("%v %v %v PART2!\n", lastNumber, unmarkedSum, unmarkedSum*lastNumber)
	}

	fmt.Println("hello world!")
}
