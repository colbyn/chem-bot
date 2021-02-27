# pylint: disable=E1136
from __future__ import annotations
from pprint import pprint
import random
# import copy
# pylint: disable=unused-wildcard-import
import sys
from sympy import *
from sympy import Number, init_printing
from sympy.matrices import Matrix
from typing import Literal, Optional, Union, Iterable, List, Callable, TypeVar, Tuple, Set, Dict
import re
import operator
import functools

init_printing(use_unicode=True)

T = TypeVar('T')
U = TypeVar('U')


def test_example():
    A = Matrix([
        [-1, 0],
        [-2, -1],
        [1, 2],
        [2, 0],
        [0, -2]
    ])

    b = Matrix([
        [-2],
        [-3],
        [0],
        [4],
        [2],
    ])

    deltas = Matrix([
        -890,
        -566,
    ])

    path = A.solve(b)
    enthalpy_answer = path.dot(deltas)
    print("answer:", enthalpy_answer)


###############################################################################
# HELPERS
###############################################################################

def pretty_subscript(value: Number) -> str:
    (numerator, denominator) = value.as_numer_denom()
    if denominator == 1:
        subscript_patterns = str.maketrans("0123456789", "₀₁₂₃₄₅₆₇₈₉")
        return str(numerator).translate(subscript_patterns)
    return str(value)

def pretty_superscript(value: Number) -> str:
    (numerator, denominator) = value.as_numer_denom()
    if denominator == 1:
        subscript_patterns = str.maketrans("0123456789", "⁰¹²³⁴⁵⁶⁷⁸⁹")
        return str(numerator).translate(subscript_patterns)
    return str(value)

def pretty_coefficient(value: Number):
    (num, den) = abs(value).as_numer_denom()
    def pack(result: str) -> str:
        if value < 0:
            return "-{}".format(result)
        else:
            return result
    if num == 0 and den == 3:
        return pack("↉")
    if num == 1 and den == 10:
        return pack("⅒")
    if num == 1 and den == 9:
        return pack("⅑")
    if num == 1 and den == 8:
        return pack("⅛")
    if num == 1 and den == 7:
        return pack("⅐")
    if num == 1 and den == 6:
        return pack("⅙")
    if num == 1 and den == 5:
        return pack("⅕")
    if num == 1 and den == 4:
        return pack("¼")
    if num == 1 and den == 3:
        return pack("⅓")
    if num == 1 and den == 2:
        return pack("½")
    if num == 2 and den == 5:
        return pack("⅖")
    if num == 2 and den == 3:
        return pack("⅔")
    if num == 3 and den == 8:
        return pack("⅜")
    if num == 3 and den == 5:
        return pack("⅗")
    if num == 3 and den == 4:
        return pack("¾")
    if num == 4 and den == 5:
        return pack("⅘")
    if num == 5 and den == 8:
        return pack("⅝")
    if num == 5 and den == 6:
        return pack("⅚")
    if num == 7 and den == 8:
        return pack("⅞")
    if den == 1:
        return pack(str(num))
    else:
        top = pretty_superscript(num)
        bot = pretty_subscript(den)
        return "{}⁄{}".format(top, bot)


def flatten_lists(xs: List[List[T]]) -> List[T]:
    result = functools.reduce(operator.add, xs)
    assert isinstance(result, list)
    return result

###############################################################################
# ELEMENT
###############################################################################

class Element:
    def __init__(self, name: str):
        self.name = name
    def __repr__(self) -> str:
        return self.name
    def __str__(self) -> str:
        return self.name

###############################################################################
# NODE VARIANTS
###############################################################################

NodeType = Literal['Chunk', 'Parens', 'Unit']
CHUNK: NodeType = 'Chunk'
PARENS: NodeType = 'Parens'
UNIT: NodeType = 'Unit'

State = Literal['(g)', '(l)', '(aq)', '(s)']
GAS: State ='(g)'
LIQUID: State ='(l)'
SOLID: State ='(s)'
AQUEOUS: State ='(aq)'


class Unit:
    payload: Element
    subscript: Number
    def __init__(self, payload: Element, subscript: Number = 1):
        self.payload = payload
        if isinstance(subscript, int):
            subscript = Number(subscript)
        self.subscript = subscript
    def __str__(self) -> str:
        sub = pretty_subscript(self.subscript)
        return "{}{}".format(self.payload, sub)
    def __repr__(self) -> str:
        return "Unit({}, {})".format(self.payload, self.subscript)
    # def __hash__(self):
    #     return hash(repr(self))

class Chunk:
    payload: List[Node]
    coefficient: Number
    state: Optional[str]
    charge: Optional[Number]
    def __init__(
        self,
        payload: List[Node],
        coefficient: Number = 1,
        state: Optional[str] = None,
        charge: Optional[Number] = None,
    ):
        for x in payload:
            assert isinstance(x, Node)
        self.payload = payload
        if isinstance(coefficient, int):
            coefficient = Number(coefficient)
        self.coefficient = coefficient
        self.state = state
        self.charge = charge
    def __str__(self) -> str:
        payload_str = "".join(map(str, self.payload))
        coefficient_str = ""
        if self.coefficient != 1:
            coefficient_str = "{} ".format(pretty_coefficient(self.coefficient))
        state_str = ""
        if self.state != None:
            state_str = " {}".format(self.state)
        charge_str = ""
        if self.charge != None:
            charge_str = str(self.charge)
        return "".join([
            coefficient_str,
            payload_str,
            charge_str,
            state_str,
        ])
    def __repr__(self) -> str:
        payload_str = "".join(map(str, self.payload))
        coefficient_str = "{}".format(self.coefficient)
        state_str = ""
        if self.state != None:
            state_str = "{}".format(self.state)
        charge_str = ""
        if self.charge != None:
            charge_str = "{}".format(self.charge)
        xs = filter(
            lambda x: len(x) > 1
            ,[
                coefficient_str,
                payload_str,
                charge_str,
                state_str,
            ]
        )
        return "Chunk({})".format(", ".join(xs))
    # def __hash__(self):
    #     return hash(repr(self))

class Parens:
    payload: List[Node]
    subscript: Number
    def __init__(self, payload: List[Node], subscript: Number = 1):
        for x in payload:
            assert isinstance(x, Node)
        self.payload = payload
        self.subscript = subscript
    def __str__(self):
        return "({})".format(self.payload)
    def __repr__(self) -> str:
        return "Parens({}, {})".format(self.payload, self.subscript)
    # def __hash__(self):
    #     return hash(repr(self))



###############################################################################
# NODE TYPE
###############################################################################

class Node:
    variant: Union[Chunk, Parens, Unit]
    def __init__(self, variant: Union[Chunk, Parens, Unit]):
        self.variant = variant
    @classmethod
    def from_str(cls, source: str) -> Reaction:
        subscript_patterns = str.maketrans("₀₁₂₃₄₅₆₇₈₉", "0123456789")
        source = source.translate(subscript_patterns)
        return init_node_parser().parse(source)
    def copy_node(self) -> Node:
        if self.is_unit():
            assert isinstance(self.variant, Unit)
            return Node(Unit(
                self.variant.payload,
                subscript=self.variant.subscript
            ))
        if self.is_parens():
            assert isinstance(self.variant, Parens)
            parens = self.variant
            payload = []
            for x in parens.payload:
                assert isinstance(x, Node)
                payload.append(x.copy_node())
            return Node(Parens(
                payload,
                subscript=parens.subscript
            ))
        if self.is_chunk():
            assert isinstance(self.variant, Chunk)
            chunk = self.variant
            payload = []
            for x in chunk.payload:
                assert isinstance(x, Node)
                payload.append(x.copy_node())
            return Node(Chunk(
                payload,
                coefficient=chunk.coefficient,
                state=chunk.state,
                charge=chunk.charge
            ))
        raise SystemError()
    def is_chunk(self) -> bool:
        return isinstance(self.variant, Chunk)
    def is_parens(self) -> bool:
        return isinstance(self.variant, Parens)
    def is_unit(self) -> bool:
        return isinstance(self.variant, Unit)
    def coefficient(self) -> Optional[Number]:
        if self.is_chunk():
            assert isinstance(self.variant, Chunk)
            return self.variant.coefficient
        return None
    def coefficient_safe(self) -> Number:
        if self.is_chunk():
            assert isinstance(self.variant, Chunk)
            return self.variant.coefficient
        return 1
    def set_coefficient_mut(self, new_coefficient: Number):
        assert isinstance(self.variant, Chunk)
        self.variant.coefficient = new_coefficient
    def set_coefficient_pure(self, new_coefficient: Number) -> Node:
        assert isinstance(self.variant, Chunk)
        return_node = self.copy_node()
        return_node.set_coefficient_mut(new_coefficient)
        return return_node
    def set_coefficient_safe(self, new_coefficient: Number) -> Node:
        return_node = self.copy_node()
        if isinstance(self.variant, Chunk):
            return_node.set_coefficient_mut(new_coefficient)
            return return_node
        else:
            return Node(Chunk(
                [return_node],
                coefficient=new_coefficient
            ))
    def nodes(self) -> Optional[List[Node]]:
        if self.is_chunk():
            assert isinstance(self.variant, Chunk)
            return self.variant.payload
        if self.is_parens():
            assert isinstance(self.variant, Parens)
            return self.variant.payload
        return None
    def foreach(
        self,
        unit: Optional[Callable[[Unit], T]] = None,
        chunk: Optional[Callable[[Chunk], T]] = None,
        parens: Optional[Callable[[Parens], T]] = None,
    ) -> Union[T, Node]:
        def pack(x: T) -> Union[T, Node]:
            if isinstance(x, Unit):
                return Node(x)
            if isinstance(x, Chunk):
                return Node(x)
            if isinstance(x, Parens):
                return Node(x)
            return x
        if self.is_unit() and unit != None:
            assert callable(unit)
            assert isinstance(self.variant, Unit)
            return pack(unit(self.variant))
        if self.is_parens() and parens != None:
            assert callable(parens)
            assert isinstance(self.variant, Parens)
            return pack(parens(self.variant))
        if self.is_chunk() and chunk != None:
            assert callable(chunk)
            assert isinstance(self.variant, Chunk)
            return pack(chunk(self.variant))
        raise ValueError()
    def __str__(self):
        return self.variant.__str__()
    def __repr__(self) -> str:
        return self.variant.__repr__()
    def elements(self) -> List[Element]:
        def for_unit(unit: Unit) -> List[Element]:
            return [unit.payload] * unit.subscript
        def for_chunk(chunk: Chunk) -> List[Element]:
            result = functools.reduce(
                operator.add,
                map(lambda x: x.elements(), chunk.payload)
            )
            assert isinstance(result, list)
            for x in result:
                assert isinstance(x, Element)
            return result * chunk.coefficient
        def for_parens(parens: Parens) -> List[Element]:
            result = functools.reduce(
                operator.add,
                map(lambda x: x.elements(), parens.payload)
            )
            assert isinstance(result, list)
            for x in result:
                assert isinstance(x, Element)
            return result * parens.subscript
        result = self.foreach(
            unit = for_unit,
            chunk = for_chunk,
            parens = for_parens,
        )
        assert isinstance(result, list)
        return result
    def element_name_map(self) -> Dict[str, int]:
        results: Dict[str, int] = {}
        for atom in self.elements():
            name = atom.name
            if name in results:
                results[name] = results[name] + 1
            else:
                results[name] = 1
        return results

def identity(x: T) -> T:
    return x

###############################################################################
# REACTION
###############################################################################

class Reaction:
    def __init__(self, reactants: List[Node], products: List[Node]):
        assert isinstance(reactants, list)
        assert isinstance(products, list)
        for x in reactants + products:
            assert isinstance(x, Node)
        self.reactants = reactants
        self.products = products
    def __str__(self) -> str:
        reactants = " + ".join(map(lambda x: str(x), self.reactants))
        products = " + ".join(map(lambda x: str(x), self.products))
        return "{} -> {}".format(reactants, products)
    def __repr__(self) -> str:
        return "Reaction({}, {})".format(self.reactants, self.products)
    # def __hash__(self):
    #     return hash(repr(self))
    @classmethod
    def from_str(cls, source: str) -> Reaction:
        return init_reaction_parser.parse(source)
    def all_elements(self) -> Set[str]:
        all_elements = set()
        for term in self.reactants + self.products:
            for atom in term.elements():
                all_elements.add(atom.name)
        return all_elements
    def balance(self):
        all_elements: Set[str] = self.all_elements()
        rows = []
        def term_to_row(term: Node, mult: int):
            term_dict: Dict[str, int] = term.element_name_map()
            row = []
            for element in all_elements:
                if element in term_dict:
                    row.append(term_dict[element] * mult)
                else:
                    row.append(0)
            rows.append(row)
        for term in self.reactants:
            term_to_row(term, -1)
        for term in self.products:
            term_to_row(term, 1)
        co_matrix = Matrix(rows).transpose()
        co_nullspace = co_matrix.nullspace(simplify=True)[0]
        balanced_coefficients = co_nullspace / gcd(tuple(co_nullspace))
        def update_terms(terms: List[Node], new_coefs: List[int]):
            assert len(terms) == len(new_coefs)
            for co, term in zip(new_coefs, terms):
                term.set_coefficient_mut(co)
        reactant_coefficients = balanced_coefficients[:len(self.reactants)]
        product_coefficients = balanced_coefficients[len(self.reactants):]
        update_terms(self.reactants, reactant_coefficients)
        update_terms(self.products, product_coefficients)
    def get_term(self, pattern: Node) -> Optional[Node]:
        left = str(pattern.set_coefficient_pure(1))
        for term in self.reactants:
            right = str(term.set_coefficient_pure(1))
            if left == right:
                co = term.coefficient_safe() * Number(-1)
                return term.set_coefficient_pure(co)
        for term in self.products:
            right = str(term.set_coefficient_pure(1))
            if left == right:
                return term
        return None
    def is_balanced(self):
        def process(terms: List[Node]):
            results: Dict[str, int] = {}
            for term in terms:
                for element in term.elements():
                    if element.name in results:
                        results[element.name] = results[element.name] + 1
                    else:
                        results[element.name] = 1
            return results
        left = process(self.reactants)
        right = process(self.products)
        if set(left.keys()) != set(right.keys()):
            return False
        for key in left.keys():
            left_count = left[key]
            right_count = right[key]
            if left_count != right_count:
                return False
        return True


###############################################################################
# PARSER
###############################################################################

import parsec

def init_node_parser():
    whitespace = parsec.regex(r'\s+', re.MULTILINE)
    ignore = parsec.many(whitespace)
    lexeme = lambda p: p << ignore
    lparen = lexeme(parsec.string('('))
    rparen = lexeme(parsec.string(')'))
    ws = parsec.optional(whitespace)
    @parsec.generate
    def parse_state():
        gas = parsec.string('(g)')
        solid = parsec.string('(s)')
        liquid = parsec.string('(l)')
        aqueous = parsec.string('(aq)')
        result = yield lexeme(gas ^ solid ^ liquid ^ aqueous)
        return result
    @parsec.generate
    def parse_integer():
        val = yield parsec.optional(parsec.regex('[0-9]+'), default_value='1')
        return int(val)
    @parsec.generate
    def parse_fraction():
        num = yield parsec.regex('[0-9]+')
        yield parsec.string("/")
        den = yield parsec.regex('[0-9]+')
        return Rational(str(num), str(den))
    @parsec.generate
    def parse_coefficient():
        result = yield parse_fraction ^ parse_integer
        return result
    @parsec.generate
    def parse_chunk():
        coefficient = yield parsec.optional(parse_coefficient, default_value=1) << ws
        payload = yield parsec.many1(parse_parens | parse_unit)
        state = yield ws >> parsec.optional(parse_state)
        for x in payload:
            assert isinstance(x, Node)
        return Node(Chunk(
            payload,
            coefficient=coefficient,
            state=state,
        ))
    @parsec.generate
    def parse_parens():
        yield lparen
        xs = yield parsec.many1(parse_unit)
        yield rparen
        subscript = yield parsec.optional(parsec.regex('[0-9]+'), default_value='1')
        for x in xs:
            assert isinstance(x, Node)
        return Node(Parens(xs, subscript=int(subscript)))
    @parsec.generate
    def parse_unit():
        e = yield parsec.regex('[A-Z]([a-z]+)?')
        subscript = yield parsec.optional(parsec.regex('[0-9]+'), default_value='1')
        return Node(Unit(Element(e), subscript=int(subscript)))
    return parse_parens | parse_chunk | parse_unit

@parsec.generate
def init_term_parser():
    whitespace = parsec.regex(r'\s+', re.MULTILINE)
    ignore = parsec.many(whitespace)
    lexeme = lambda p: p << ignore
    plus_sym = lexeme(parsec.string('+'))
    terms = yield parsec.sepBy1(lexeme(init_node_parser()), plus_sym)
    for x in terms:
        assert isinstance(x, Node)
    return terms


@parsec.generate
def init_reaction_parser():
    whitespace = parsec.regex(r'\s+', re.MULTILINE)
    ignore = parsec.many(whitespace)
    lexeme = lambda p: p << ignore
    reaction_symbol = lexeme(parsec.string('->') | parsec.string('='))
    left = yield lexeme(init_term_parser)
    yield reaction_symbol
    right = yield lexeme(init_term_parser)
    for x in left + right:
        assert isinstance(x, Node)
    return Reaction(left, right)

###############################################################################
# UNITS
###############################################################################

class KiloJoulePerMol:
    def __init__(self, value: Number):
        self.value = value
    def __str__(self) -> str:
        return "{} kJ ㏖⁻¹".format(self.value)

###############################################################################
# THERMOCHEMICAL EQUATION
###############################################################################

class ThermochemicalEquation:
    def __init__(self, reaction: Reaction, enthalpy_change: KiloJoulePerMol):
        self.reaction = reaction
        self.enthalpy_change = enthalpy_change
    def __str__(self) -> str:
        return "{} where ΔH = {}".format(
            self.reaction,
            self.enthalpy_change
        )

def enthalpy_of_reaction(
    unknown_reaction: Reaction,
    known_reactions: List[ThermochemicalEquation],
) -> KiloJoulePerMol:
    all_reactions = (
        list(map(lambda x: x.reaction, known_reactions)) +
        [unknown_reaction]
    )
    all_terms: Set[str] = set()
    for reaction in all_reactions:
        for term in reaction.reactants + reaction.products:
            all_terms.add(str(term.set_coefficient_pure(1)))
    for x in all_terms:
        assert x == str(Node.from_str(x))
    def init_column(reaction: Reaction) -> List[Number]:
        assert isinstance(reaction, Reaction)
        column = []
        for term_str in all_terms:
            node = Node.from_str(term_str)
            assert isinstance(node, Node)
            term_opt = reaction.get_term(node)
            if isinstance(term_opt, Node):
                column.append(term_opt.coefficient_safe())
            else:
                column.append(0)
        return column
    def init_a():
        rows = []
        for reaction in list(map(lambda x: x.reaction, known_reactions)):
            rows.append(init_column(reaction))
        return Matrix(rows).transpose()
    def init_b():
        row = init_column(unknown_reaction)
        return Matrix(row)
    def init_deltas():
        row = list(map(lambda x: x.enthalpy_change.value, known_reactions))
        return Matrix(row).transpose()
    path = init_a().solve(init_b())
    enthalpy_answer = KiloJoulePerMol(path.dot(init_deltas()))
    return enthalpy_answer



###############################################################################
# UTILS
###############################################################################

def program(func):
    if __name__ == '__main__':
        func()


###############################################################################
# DEV
###############################################################################

# @program
def dev1():
    reaction = Reaction.from_str(
        "C3H8 + O2 -> H2O + CO2"
    )
    reaction.balance()
    print(str(reaction))


# @program
def dev2():
    reaction1 = ThermochemicalEquation(
        Reaction.from_str("CH4(g) + 2O2(g) = CO2(g) + 2H2O(l)"),
        KiloJoulePerMol(-890),
    )
    reaction2 = ThermochemicalEquation(
        Reaction.from_str("2CO(g) + O2(g) = 2CO2(g)"),
        KiloJoulePerMol(-566),
    )
    reaction3 = Reaction.from_str("2CH4(g) + 3O2(g) = 2CO(g) + 4H2O(l)")
    print(enthalpy_of_reaction(
        reaction3,
        [reaction1, reaction2]
    ))


# @program
def dev3():
    reaction1 = ThermochemicalEquation(
        Reaction.from_str(
            "2C2H2(g) + 5O2(g) = 4CO2(g) + 2H2O(l)"
        ),
        KiloJoulePerMol(-2600.0),
    )
    reaction2 = ThermochemicalEquation(
        Reaction.from_str(
            "2C2H6(g) + 7O2(g) = 4CO2(g) + 6H2O(l)"
        ),
        KiloJoulePerMol(-3210.0),
    )
    reaction3 = ThermochemicalEquation(
        Reaction.from_str(
            "H2(g) + 1/2 O2(g) = H2O(l)"
        ),
        KiloJoulePerMol(-286.0),
    )
    reaction4 = Reaction.from_str(
        "C2H2(g) + 2H2(g) = C2H6(g)"
    )
    answer = enthalpy_of_reaction(
        reaction4,
        [reaction1, reaction2, reaction3]
    )
    print(answer)

@program
def dev4():
    reaction = Reaction.from_str("C2 + H2O -> H2O")
    print(reaction.is_balanced())
