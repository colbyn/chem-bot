# pylint: disable=E1136
from __future__ import annotations
from pprint import pprint
from sympy import Number
from sympy.matrices import Matrix
from typing import Literal, Optional, Union, Iterable, List, Callable, TypeVar, Tuple
import re

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
        self.subscript = subscript
    def __str__(self) -> str:
        return "{}{}".format(self.payload, self.subscript)
    def __repr__(self) -> str:
        return "Unit({}, {})".format(self.payload, self.subscript)

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
        self.payload = payload
        self.coefficient = coefficient
        self.state = state
        self.charge = charge
    def __str__(self) -> str:
        payload_str = "".join(map(str, self.payload))
        subscript_str = ""
        if self.coefficient != 1:
            subscript_str = str(self.coefficient)
        state_str = ""
        if self.state != None:
            state_str = " {}".format(self.state)
        charge_str = ""
        if self.charge != None:
            charge_str = str(self.charge)
        return "".join([
            subscript_str,
            payload_str,
            charge_str,
            state_str,
        ])
    def __repr__(self) -> str:
        payload_str = ", ".join(map(str, self.payload))
        coefficient_str = ""
        if self.coefficient != 1:
            coefficient_str = "{}, ".format(self.coefficient)
        state_str = ""
        if self.state != None:
            state_str = "{}, ".format(self.state)
        charge_str = ""
        if self.charge != None:
            charge_str = "{}, ".format(self.charge)
        return "Chunk({}{}{}{})".format(
            coefficient_str,
            payload_str,
            charge_str,
            state_str,
        )

class Parens:
    payload: List[Node]
    subscript: Number
    def __init__(self, payload: List[Node], subscript: Number = 1):
        self.payload = payload
        self.subscript = subscript
    def __str__(self):
        return "({})".format(self.payload)
    def __repr__(self) -> str:
        return "Parens({}, {})".format(self.payload, self.subscript)



###############################################################################
# NODE TYPE
###############################################################################

class Node:
    variant: Union[Chunk, Parens, Unit]
    def __init__(self, variant: Union[Chunk, Parens, Unit]):
        self.variant = variant
    @classmethod
    def from_str(cls, source: str) -> Reaction:
        return init_node_parser().parse(source)
    def is_chunk(self) -> bool:
        return isinstance(self.variant, Chunk)
    def is_parens(self) -> bool:
        return isinstance(self.variant, Parens)
    def is_unit(self) -> bool:
        return isinstance(self.variant, Unit)
    # @property
    # def coefficient(self) -> Optional[Number]:
    #     if self.is_chunk():
    #         return self.__coefficient
    #     return None
    # @coefficient.setter
    # def coefficient(self, new_value: Number):
    #     if self.is_chunk():
    #         return self.__coefficient
    #     else:
    #         raise RuntimeError('Invalid NodeType')
    # @property
    # def subscript(self) -> Optional[Number]:
    #     valid_types: List[NodeType] = ['Parens', 'Unit']
    #     if self.__node_type in valid_types:
    #         assert self.coefficient != 0
    #         return self.coefficient
    #     return None
    # @subscript.setter
    # def subscript(self, new_value: Number):
    #     if self.is_parens() or self.is_unit():
    #         return self.__subscript
    #     else:
    #         raise RuntimeError('Invalid NodeType')
    # @property
    # def nodes(self) -> Optional[List[Node]]:
    #     valid_types: List[NodeType] = ['Chunk', 'Parens']
    #     if self.__node_type in valid_types:
    #         assert isinstance(self.__payload, list)
    #         return self.__payload
    #     return None
    # @property
    # def element(self) -> Optional[Element]:
    #     if self.is_unit():
    #         assert isinstance(self.__payload, Element)
    #         return self.__payload
    #     return None
    # # def for_each(
    # #     self,
    # #     unit: Callable[]
    # # ):
    # #     pass
    def foreach(
        self,
        unit: Optional[Callable[[Unit], T]] = None,
        chunk: Optional[Callable[[Chunk], T]] = None,
        parens: Optional[Callable[[Parens], T]] = None,
    ) -> Optional[T]:
        if self.is_unit() and unit != None:
            assert callable(unit)
            assert isinstance(self.variant, Unit)
            return unit(self.variant)
        if self.is_parens() and parens != None:
            assert callable(parens)
            assert isinstance(self.variant, Parens)
            return parens(self.variant)
        if self.is_chunk() and chunk != None:
            assert callable(chunk)
            assert isinstance(self.variant, Chunk)
            return chunk(self.variant)
        return None
    def __str__(self):
        return self.variant.__str__()
    def __repr__(self) -> str:
        return self.variant.__repr__()

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
    def __repr__(self) -> str:
        return "Reaction({}, {})".format(self.reactants, self.products)
    @classmethod
    def from_str(cls, source: str) -> Reaction:
        return init_reaction_parser.parse(source)

###############################################################################
# UNITS
###############################################################################

class KiloJoulePerMol:
    def __init__(self, value: Number):
        self.value = value

###############################################################################
# THERMOCHEMICAL EQUATION
###############################################################################

class ThermochemicalEquation:
    def __init__(self, reaction: Reaction, enthalpy_change: KiloJoulePerMol):
        self.reaction = reaction
        self.enthalpy_change = enthalpy_change

###############################################################################
# PARSER
###############################################################################

import parsec

def init_node_parser():
    whitespace = parsec.regex(r'\s+', re.MULTILINE)
    ignore = parsec.many(whitespace)
    lexeme = lambda p: p << ignore  # skip all ignored characters.
    lparen = lexeme(parsec.string('('))
    rparen = lexeme(parsec.string(')'))
    @parsec.generate
    def parse_chunk():
        coefficient = yield parsec.optional(parsec.regex('[0-9]+'), default_value='1')
        payload = yield parsec.many1(parse_parens | parse_unit)
        return Node(Chunk(
            [payload],
            coefficient=int(coefficient),
        ))
    @parsec.generate
    def parse_parens():
        yield lparen
        xs = yield parsec.many1(parse_unit)
        yield rparen
        subscript = yield parsec.optional(parsec.regex('[0-9]+'), default_value='1')
        return Node(Parens(xs, subscript=int(subscript)))
    @parsec.generate
    def parse_unit():
        e = yield parsec.regex('[A-Z]([a-z]+)?')
        subscript = yield parsec.optional(parsec.regex('[0-9]+'), default_value='1')
        return Unit(Element(e), subscript=int(subscript))
    return parse_parens | parse_chunk | parse_unit

@parsec.generate
def init_term_parser():
    whitespace = parsec.regex(r'\s+', re.MULTILINE)
    ignore = parsec.many(whitespace)
    lexeme = lambda p: p << ignore  # skip all ignored characters.
    plus_sym = lexeme(parsec.string('+'))
    terms = yield parsec.sepBy1(lexeme(init_node_parser()), plus_sym)
    return terms


@parsec.generate
def init_reaction_parser():
    whitespace = parsec.regex(r'\s+', re.MULTILINE)
    ignore = parsec.many(whitespace)
    lexeme = lambda p: p << ignore  # skip all ignored characters.
    reaction_symbol = lexeme(parsec.string('->'))
    left = yield lexeme(init_term_parser)
    yield reaction_symbol
    right = yield lexeme(init_term_parser)
    return Reaction(left, right)


###############################################################################
# DEV
###############################################################################

def program(func):
    if __name__ == '__main__':
        func()


@program
def test():
    node = Node.from_str("2H2O")
    pprint(node)


