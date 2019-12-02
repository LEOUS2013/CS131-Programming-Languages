package main

import (
	"fmt"
	"container/list"
)

func main() {
	//build a list of ints from 0 - 999
	l := list.New()
	for i := 0; i < 1000; i++ {
		l.PushBack(i)
	}

	//define n
	n := 109

	//call everyNth, assign its return value to a variable
	nth_list := everyNth(l, n)

	//print nth_list
	for e := nth_list.Front(); e != nil; e = e.Next() {
		fmt.Println(e.Value)
	}
}

func everyNth(l *list.List, n int) *list.List {
	//index counter
	index := 0

	//new list for return value
	ret := list.New()

	//loop through the list
	for e := l.Front(); e != nil; e = e.Next() {
		if (index + 1) % n == 0 {
			ret.PushBack(e.Value)
		}

		index++
	}

	return ret
}