#!/usr/bin/env python3

def solution_1(path):
    parsed = []
    numbers = open(path).read().split("\n")
    for n in numbers:
        current = int(n)
        compliment = 2020 - current
        if compliment in parsed:
            return compliment * current
        parsed.append(current)
    return 0

def two_sum(goal, nums):
    for n in nums:
        compliment = goal - n
        if compliment in nums:
            return n * compliment
    return 0

def solution_2(path):
    parsed = []
    nums = open(path).read().split("\n")
    for n in nums:
        current = int(n)
        compliment = 2020 - current
        pair = two_sum(compliment, parsed)
        if pair > 0:
            return pair * current
        parsed.append(current)
    return 0

