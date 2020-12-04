#!/usr/bin/env python3

def correct_password_1(line):
    rule_and_pass = line.split(" ")
    letter_min = int(rule_and_pass[0].split("-")[0])
    letter_max = int(rule_and_pass[0].split("-")[1])
    letter = rule_and_pass[1][0]
    password = rule_and_pass[2]
    counter = 0
    for l in password:
        if l == letter:
            if counter == letter_max:
                return False
            counter += 1
    return counter >= letter_min

def actual_correct_password(line):
    rule_and_pass = line.split(" ")
    index_1 = int(rule_and_pass[0].split("-")[0]) - 1
    index_2 = int(rule_and_pass[0].split("-")[1]) - 1
    letter = rule_and_pass[1][0]
    l1 = rule_and_pass[2][index_1]
    l2 = rule_and_pass[2][index_2]
    if l1 == letter:
        return l2 != letter
    else:
        return l2 == letter

def solution_1(path):
    passwords = open(path).read().split("\n")[0:-1]
    correct = 0
    for password in passwords:
        if correct_password_1(password):
            correct += 1
    return correct

def solution_2(path):
    passwords = open(path).read().split("\n")[0:-1]
    correct = 0
    for password in passwords:
        if actual_correct_password(password):
            correct += 1
    return correct
