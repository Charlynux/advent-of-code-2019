package main

import (
        "bufio"
        "fmt"
        "io"
        "os"
        "strconv"
)

func fuelRequired(mass int) int {
        return ((mass / 3) - 2)
}

func main() {
        file, err := os.Open("input")
        if err != nil {
                panic(err)
        }
        
        defer file.Close()
        
        reader := bufio.NewReader(file)
        
        result := 0
        for {
                line, _, err := reader.ReadLine()
                
                if err == io.EOF {
                        break
                }
                
                value, _ := strconv.Atoi(string(line))
                result += fuelRequired(value)
        }
        fmt.Printf("%d \n", result)
}