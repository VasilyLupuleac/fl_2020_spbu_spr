import pytest
from p_parser import try_parsing
from p_ast import *


def test_empty():
	input = "?-."
	err, pr = try_parsing(input)
	assert not err
	assert not pr.rels
	assert not pr.target


def test_atom():
	input = "?- f(A, g(X, y))."
	err, pr = try_parsing(input)
	assert not err
	assert not pr.rels
	assert len(pr.target) == 1
	assert pr.target[0].name == "f"
	assert len(pr.target[0].args) == 2


def test_multi_lines():
	input = "f (A, B) :- h.\n f (b(x), g(X, y)) :- t(x). ?-."
	err, pr = try_parsing(input)
	assert not err
	assert len(pr.rels.items()) == 1
	assert not pr.target
	assert len(pr.rels["f"]) == 2


def test_general_case_large():
	input = """eval(St, var(X), U) :- elem(X, St, U).
eval(St, conj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), and(V, W, U).
eval(St, disj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), or(V, W, U).
eval(St, not(X), U) :- eval(St, X, V), neg(U, V).

elem(zero, cons(H,T), H).
elem(succ(N), cons(H,T), V) :- elem(N, T, V).
nand(false, false, true).
nand(false, true, true).
nand(true, false, true).
nand(true, true, false).
neg(X, R) :- nand(X, X, R).
or(X, Y, R) :- nand(X, X, Xx), nand(Y, Y, Yy), nand(Xx, Yy, R).
and(X, Y, R) :- nand(X, Y,   Xy), nand(Xy, Xy, R).
?- eval(St, conj(disj(X,Y),not(var(Z))), true)."""
	err, pr = try_parsing(input)
	assert not err
	assert len(pr.rels.items()) > 3
	assert len(pr.target) == 1
	assert len(pr.rels["eval"]) == 4


if __name__ == "__main__":
    pytest.main()