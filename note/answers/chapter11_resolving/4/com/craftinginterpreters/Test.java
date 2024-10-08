package com.craftinginterpreters;

import com.craftinginterpreters.lox.Lox;

import java.io.IOException;

public class Test {
    public static void main(String[] args) throws IOException {
        Lox.runFile("test/closure/assign_to_shadowed_later.lox");
    }
}
