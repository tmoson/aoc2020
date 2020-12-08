#!/usr/bin/env python3

def right_x_down_y(nav, horizontal_move, vertical_move):
    endx = len(nav[0])
    endy = len(nav)
    x = 0
    y = 0
    trees = 0
    while y < endy:
        if x < endx:
            if nav[y][x] == '#':
                trees += 1
                x += horizontal_move
                y += vertical_move
            else:
                x += horizontal_move
                y += vertical_move
        elif nav[y][x % endx] == '#':
            trees += 1
            x += horizontal_move
            y += vertical_move
        else:
            x += horizontal_move
            y += vertical_move
    return trees

def solution_1(path):
    return right_x_down_y(open(path).read().strip().split("\n"), 3, 1)

def solution_2(path):
    nav = open(path).read().strip().split("\n")
    one_one = right_x_down_y(nav, 1, 1)
    three_one = right_x_down_y(nav, 3, 1)
    five_one = right_x_down_y(nav, 5, 1)
    seven_one = right_x_down_y(nav, 7, 1)
    one_two = right_x_down_y(nav, 1, 2)
    return one_one * one_two * three_one * five_one * seven_one