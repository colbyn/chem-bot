module Chem

using Printf
using Base
using LinearAlgebraX
using LinearAlgebra
using AutoHashEquals



###############################################################################
# CONSTANTS
###############################################################################

const DEBUG_MODE = false

###############################################################################
# COMMON HELPER TYPES
###############################################################################

# const Option{T} = Union{T,Nothing}

# map_option{T}()

###############################################################################
# NODE VARIANT TYPES
###############################################################################

@auto_hash_equals struct IChunk{T}
    coefficient::Number
    nodes::Array{T,1}
    state::String
    charge::Union{Int,Nothing}
end

@auto_hash_equals struct IParens{T}
    nodes::Array{T,1}
    subscript::Number
end

@auto_hash_equals struct IUnit
    element::String
    subscript::Number
end


###############################################################################
# NODE
###############################################################################

@auto_hash_equals struct Node
    data::Union{IChunk{Node}, IParens{Node}, IUnit}
end

function Chunk(coefficient::Number, nodes::Array{Node,1})::Node
    charge::Union{Number,Nothing} = nothing
    return Node(IChunk(
        coefficient,
        nodes,
        "",
        charge
    ))
end
function Chunk(coefficient::Number, nodes::Array{Node,1}, state::String)::Node
    charge::Union{Number,Nothing} = nothing
    return Node(IChunk(
        coefficient,
        nodes,
        state,
        charge
    ))
end
function Chunk(
    coefficient::Number;
    nodes::Array{Node,1} = [],
    state::String = "",
    charge::Union{Int,Nothing} = Nothing
)::Node
    @assert nodes != []
    charge::Union{Number,Nothing} = nothing
    return Node(IChunk(
        coefficient,
        nodes,
        state,
        charge
    ))
end
Parens(nodes::Array{Node,1}, subscript::Number)::Node = Node(IParens(nodes, subscript))
Unit(element::String, subscript::Number)::Node = Node(IUnit(element, subscript))

is_node(x) = typeof(x) == Node
is_chunk(node) = is_node(node) && typeof(node.data) == IChunk{Node}
is_parens(node) = is_node(node) && typeof(node.data) == IParens{Node}
is_unit(node) = is_node(node) && typeof(node.data) == IUnit

# get_element(node::Node)::String = node.data.element
# get_nodes(node::Node)::Array{Node,1} = node.data.nodes
# get_subscript(node::Node)::Number = node.data.subscript

function match_node(node::Node; unit = identity, chunk = identity, parens = identity)
    if is_unit(node)
        unit(node.data)
    elseif is_chunk(node)
        chunk(node.data)
    elseif is_parens(node)
        parens(node.data)
    end
end

for_unit(func::Function, node::Node) = match_node(node, unit = func)

function traverse(func, node::Node)
    function process(original, result)::Node
        if is_parens(result) || is_unit(result) || is_chunk(result)
            return result
        elseif typeof(result) == IChunk{Node}
            return Node(result)
        elseif typeof(result) == IParens{Node}
            return Node(result)
        elseif typeof(result) == IUnit
            return Node(result)
        else
            return original
        end
    end
    function for_chunk(chunk::IChunk)::Node
        new_nodes = map(x -> process(x, traverse(func, x)), chunk.nodes)
        new_chunk = Chunk(chunk.coefficient, new_nodes)
        result = func(new_chunk)
        return process(Node(chunk), result)
    end
    function for_parens(parens::IParens)::Node
        new_nodes = map(x -> process(x, traverse(func, x)), parens.nodes)
        new_parens = Parens(new_nodes, parens.subscript)
        result = func(new_parens)
        return process(Node(parens), result)
    end
    function for_unit(unit::IUnit)::Node
        new_node = Node(unit)
        result = func(new_node)
        return process(Node(unit), result)
    end
    match_node(
        node,
        chunk = for_chunk,
        parens = for_parens,
        unit = for_unit,
    )
end

function get_atoms(node::Node)::Array{String,1}
    function go(atoms::Array{Node,1}, mult::Number)::Array{String,1}
        xs = collect(Iterators.flatten(map(x -> get_atoms(x), atoms)))
        return repeat(xs, mult)
    end
    if is_unit(node)
        return repeat([node.data.element], node.data.subscript)
    elseif is_chunk(node)
        return go(node.data.nodes, node.data.coefficient)
    elseif is_parens(node)
        return go(node.data.nodes, node.data.subscript)
    end
end

function count_elements(node::Node)::Dict{String,Number}
    atoms = get_atoms(node)
    xs = Dict()
    for atom in atoms
        if haskey(xs, atom)
            xs[atom] = xs[atom] + 1
        else
            xs[atom] = 1
        end
    end
    return xs
end

function get_coefficient_map(node::Node, total_elements::Array{String,1})
    counter = count_elements(node)
    results = map(total_elements) do element
        count = get(counter, element, 0)
        return (element, count)
    end
    return Dict(results)
end

function get_coefficient_row(node::Node, total_elements::Array{String,1})::Array{Number,1}
    coefficient_map = get_coefficient_map(node, total_elements)
    entries = coefficient_map |> keys |> collect |> Base.sort
    return map(entries) do key
        return coefficient_map[key]
    end
end

function pretty_subscripts(text::String; filter_one = false)::String
    if filter_one && text == "1"
        return ""
    else
        map(text) do char
            if char == '0'
                return '₀'
            end
            if char == '1'
                return '₁'
            end
            if char == '2'
                return '₂'
            end
            if char == '3'
                return '₃'
            end
            if char == '4'
                return '₄'
            end
            if char == '5'
                return '₅'
            end
            if char == '6'
                return '₆'
            end
            if char == '7'
                return '₇'
            end
            if char == '8'
                return '₈'
            end
            if char == '9'
                return '₉'
            end
            return char
        end
    end
end
function pretty_superscripts(text::String; filter_one = false)::String
    if filter_one && text == "1"
        return ""
    else
        map(text) do char
            if char == '0'
                return '⁰'
            end
            if char == '1'
                return '¹'
            end
            if char == '2'
                return '²'
            end
            if char == '3'
                return '³'
            end
            if char == '4'
                return '⁴'
            end
            if char == '5'
                return '⁵'
            end
            if char == '6'
                return '⁶'
            end
            if char == '7'
                return '⁷'
            end
            if char == '8'
                return '⁸'
            end
            if char == '9'
                return '⁹'
            end
            return char
        end
    end
end
function pretty_fraction(value::Rational{Int})::String
    is_negative = value < 0//1
    neg_text = if is_negative
        "-"
    else
        ""
    end
    value = abs(value)
    if value == 0//3
        return @sprintf("%s↉", neg_text)
    end
    if value == 1//10
        return @sprintf("%s⅒", neg_text)
    end
    if value == 1//9
        return @sprintf("%s⅑", neg_text)
    end
    if value == 1//8
        return @sprintf("%s⅛", neg_text)
    end
    if value == 1//7
        return @sprintf("%s⅐", neg_text)
    end
    if value == 1//6
        return @sprintf("%s⅙", neg_text)
    end
    if value == 1//5
        return @sprintf("%s⅕", neg_text)
    end
    if value == 1//4
        return @sprintf("%s¼", neg_text)
    end
    if value == 1//3
        return @sprintf("%s⅓", neg_text)
    end
    if value == 1//2
        return @sprintf("%s½", neg_text)
    end
    if value == 2//5
        return @sprintf("%s⅖", neg_text)
    end
    if value == 2//3
        return @sprintf("%s⅔", neg_text)
    end
    if value == 3//8
        return @sprintf("%s⅜", neg_text)
    end
    if value == 3//5
        return @sprintf("%s⅗", neg_text)
    end
    if value == 3//4
        return @sprintf("%s¾", neg_text)
    end
    if value == 4//5
        return @sprintf("%s⅘", neg_text)
    end
    if value == 5//8
        return @sprintf("%s⅝", neg_text)
    end
    if value == 5//6
        return @sprintf("%s⅚", neg_text)
    end
    if value == 7//8
        return @sprintf("%s⅞", neg_text)
    end
    if denominator(value) == 1
        return @sprintf("%s%s", neg_text, numerator(value))
    else
        num = numerator(value) |> string |> pretty_superscripts
        dem = denominator(value) |> string |> pretty_subscripts
        return @sprintf("%s%s⁄%s", neg_text, num, dem)
    end
end
function node_to_string(node::Node)::String
    if is_unit(node)
        element = node.data.element
        subscript = node.data.subscript
        return @sprintf("%s%s", element, subscript |> string |> (x -> pretty_subscripts(x, filter_one=true)))
    elseif is_parens(node)
        subscript = node.data.subscript
        nodes = join(map(node_to_string, node.data.nodes), ",")
        return @sprintf("(%s)%s", nodes, subscript |> string |> (x -> pretty_subscripts(x, filter_one=true)))
    elseif is_chunk(node)
        coefficient = node.data.coefficient
        nodes = join(map(node_to_string, node.data.nodes), "")
        state = node.data.state
        coefficient_str = if coefficient == 1
            ""
        elseif typeof(coefficient) == Rational{Int}
            @sprintf("%s ", pretty_fraction(coefficient))
        else
            @sprintf("%s ", string(coefficient))
        end
        state_str = if state == ""
            ""
        else
            @sprintf(" %s", state)
        end
        return @sprintf("%s%s%s", coefficient_str, nodes, state_str)
    end
end

function nodes_to_string(nodes::Array{Node,1})::String
    return map(node_to_string, nodes) |> (x -> join(x, " + "))
end

function show_node_ast(node::Node)::String
    if is_unit(node)
        element = node.data.element
        subscript = node.data.subscript
        return @sprintf("Unit(%s, %d)", element, subscript)
    elseif is_chunk(node)
        coefficient = node.data.coefficient
        nodes = join(map(show_node_ast, node.data.nodes), ",")
        state = node.data.state
        if state != ""
            return @sprintf("Chunk(%d, %s, \"%s\")", coefficient, nodes, state)
        else
            return @sprintf("Chunk(%d, %s)", coefficient, nodes)
        end
    elseif is_parens(node)
        subscript = node.data.subscript
        nodes = join(map(show_node_ast, node.data.nodes), ",")
        return @sprintf("Chunk(%s, %d)", subscript, nodes)
    end
end

function show_nodes_ast(nodes::Array{Node,1})
    nodes = map(show_node_ast, nodes) |> (x -> join(x, ","))
    return @sprintf("[%s]", nodes)
end

if DEBUG_MODE == false
    function Base.show(io::IO, m::Node)
        if DEBUG_MODE
            write(io, show_node_ast(m))
        else
            show(io, node_to_string(m))
        end
    end

    function Base.show(io::IO, m::Array{Node,1})
        if DEBUG_MODE
            write(io, show_nodes_ast(m))
        else
            show(io, nodes_to_string(m))
        end
    end
end

function negate_coefficient(node::Node)::Node
    if is_unit(node)
        return Chunk(-1, node)
    elseif is_parens(node)
        return return Chunk(-1, node)
    elseif is_chunk(node)
        return Chunk(-1 * node.data.coefficient, node.data.nodes, node.data.state)
    end
end

function upsert_coefficient(node::Node, new_coefficient::Number)::Node
    is_unit_or_parens = is_unit(node) || is_parens(node)
    if is_unit_or_parens && new_coefficient == 1
        return node
    elseif is_unit(node)
        return Chunk(new_coefficient, node)
    elseif is_parens(node)
        return return Chunk(new_coefficient, node)
    elseif is_chunk(node)
        return Chunk(new_coefficient, node.data.nodes, node.data.state)
    end
end

function get_root_coefficient(node::Node)::Number
    if is_chunk(node)
        return node.data.coefficient
    elseif is_unit(node)
        return 1
    elseif is_parens(node)
        return 1
    end
end

###############################################################################
# REACTION
###############################################################################

@auto_hash_equals mutable struct Reaction
    reactants::Array{Node,1}
    products::Array{Node,1}
end

if DEBUG_MODE == false
    function Base.show(io::IO, reaction::Reaction)
        if DEBUG_MODE
            left = show_nodes_ast(reaction.reactants)
            right = show_nodes_ast(reaction.products)
            reaction_str = @sprintf(
                "Reaction(\n\t%s,\n\t%s\n)",
                left,
                right,
            )
            write(io, reaction_str)
        else
            left = nodes_to_string(reaction.reactants)
            right = nodes_to_string(reaction.products)
            reaction_str = @sprintf(
                "%s -> %s",
                left,
                right,
            )
            write(io, reaction_str)
        end
    end
end

###############################################################################
# REACTION OPERATIONS
###############################################################################

function balance(reaction::Reaction)
    total_elements = vcat(reaction.reactants, reaction.products) |>
        (x -> map(get_atoms, x)) |>
        Iterators.flatten |>
        collect |>
        unique
    function init_matrix(terms::Array{Node,1})::Matrix{Number}
        total_rows = length(terms)
        total_columns = length(total_elements)
        matrix = Matrix{Number}(undef, total_columns, total_rows)
        for (index, term) in enumerate(terms)
            coefs = get_coefficient_row(term, total_elements)
            matrix[:, index] = coefs
        end
        return matrix
    end
    (num, mat, results) = let
        all_terms = vcat(reaction.reactants, reaction.products)
        reaction_matrix = all_terms |> init_matrix
        mat = -LinearAlgebraX.nullspacex(reaction_matrix)
        mat ./= gcd(mat)
        num = size(mat, 2)
        results = zip(all_terms, mat)
        (num, mat, results)
    end
    if num == 1
        new_reactants = []
        new_products = []
        for (term, co) in results
            if co > 0
                push!(new_reactants, upsert_coefficient(term, convert(Number, co)))
            else
                push!(new_products, upsert_coefficient(term, convert(Number, abs(co))))
            end
        end
        return Reaction(new_reactants, new_products)
    elseif num > 1
        error("Chemical equation $equation can be balanced in infinitely many ways")
    else
        error("Chemical equation $equation cannot be balanced")
    end
end

function get_limiting_reagent(reaction::Reaction)
    reaction = balance(reaction)
end

function new_reaction()::Reaction
    balance_test = true
    yield_test = true
    # C3H8 + O2 = CO2 + H20
    if balance_test
        reactants = [
            Chunk(1, [
                Unit("C", 3),
                Unit("H", 8),
            ]),
            Chunk(1, [Unit("O", 2)]),
        ]
        products = [
            Chunk(1, [
                Unit("H", 2),
                Unit("O", 1),
            ]),
            Chunk(1, [
                Unit("C", 1),
                Unit("O", 2),
            ]),
        ]
    else
        reactants = [
            Chunk(2, [
                Unit("H", 2),
                Unit("O", 1),
            ]),
        ]
        products = [
            Chunk(1, [
                Unit("H", 2),
                Unit("O", 1),
            ]),
            Chunk(1, [
                Unit("H", 2),
                Unit("O", 1),
            ]),
        ]
    end
    return Reaction(reactants, products)
end


###############################################################################
# INTERNAL REACTION UTILS
###############################################################################


"""
Transforms the AST representation of a sum of reactants or products into a matrix. 
"""
function term_ast_to_matrix(
        total_elements::Array{String},
        terms::Array{Node,1}
    )::Matrix{Number}
    total_rows = length(terms)
    total_columns = length(total_elements)
    matrix = Matrix{Number}(undef, total_columns, total_rows)
    for (index, term) in enumerate(terms)
        coefs = get_coefficient_row(term, total_elements)
        matrix[:, index] = coefs
    end
    return matrix
end

"""
Transforms the reaction AST representation into a matrix. Use `total_elements` for multiple reactions.
"""
function reaction_ast_to_matrix(
        reaction::Reaction;
        total_elements = Nothing
    )::Array{Number,2}
    all_terms = vcat(reaction.reactants, reaction.products)
    if total_elements == Nothing
        total_elements = (
            vcat(reaction.reactants, reaction.products)
                |> (x -> map(get_atoms, x))
                |> Iterators.flatten
                |> collect
                |> unique
        )
    end
    return all_terms |>  x -> term_ast_to_matrix(total_elements, x)
end


###############################################################################
# UNIT TYPES (AD-HOCK FOR NOW)
###############################################################################

@auto_hash_equals struct KiloJoulePerMol
    value::Number
end

if DEBUG_MODE == false
    function Base.show(io::IO, value::KiloJoulePerMol)
        value_str = @sprintf(
            "%s kJ ㏖⁻¹",
            value.value,
        )
        write(io, value_str)
    end
end


###############################################################################
# THERMOCHEMICAL EQUATIONS
###############################################################################

@auto_hash_equals struct ThermochemicalEquation
    reaction::Reaction
    enthalpy_change::KiloJoulePerMol
end

if DEBUG_MODE == false
    function Base.show(io::IO, therm::ThermochemicalEquation)
        equation = therm.reaction
        enthalpy_change = therm.enthalpy_change
        thermochemical_equation_str = @sprintf(
            "%s where ∆H°=%s",
            equation,
            enthalpy_change
        )
        write(io, thermochemical_equation_str)
    end
end

function enthalpy_of_reaction(
        reactions::Array{ThermochemicalEquation,1},
        equation::Reaction
    )::KiloJoulePerMol
    total_reactions::Array{Reaction} = vcat(map(x -> x.reaction, reactions), [equation])
    total_terms::Array{Node,1} = let
        function update_coefficient(term::Node)::Node
            return upsert_coefficient(term, 1)
        end
        terms = map(total_reactions) do reaction
            (vcat(reaction.reactants, reaction.products)
                |> xs -> map(update_coefficient, xs)
                |> unique)
        end
        terms |> Iterators.flatten |> collect |> unique
    end
    total_columns = length(total_reactions)
    total_rows = length(total_terms)
    v_matrix = LinearAlgebraX.Matrix{Rational}(undef, total_rows, total_columns)
    for (index, term_key) in enumerate(total_terms)
        v_row = []
        function get_term_co(reaction::Reaction)::Rational
            matches = []
            for term in reaction.reactants
                if upsert_coefficient(term, 1) == term_key
                    push!(matches, -1//1 * get_root_coefficient(term))
                end
            end
            for term in reaction.products
                if upsert_coefficient(term, 1) == term_key
                    push!(matches, 1//1 * get_root_coefficient(term))
                end
            end
            push!(matches, 0)
            return sum(matches)
        end
        for reaction in total_reactions
            push!(v_row, get_term_co(reaction))
        end
        v_matrix[index, :] = v_row
    end
    println(v_matrix)
    numat = LinearAlgebraX.rrefx(v_matrix)
    answers = []
    for entry in numat[:, total_columns]
        if entry != 0
            push!(answers, entry)
        end
    end
    answer_matrix = reshape(answers, :, 1)
    deltas = map(r -> r.enthalpy_change.value, reactions)
    return KiloJoulePerMol(dot(answer_matrix, deltas))
end


###############################################################################
# Thermochemical Equation Examples
###############################################################################

function thermochemical_equation_example_1()
    equation1 = ThermochemicalEquation(
        let reactants = [
                Chunk(1, [
                    Unit("C", 1),
                    Unit("H", 4),
                ], "(g)"),
                Chunk(2, [
                    Unit("O", 2),
                ], "(g)"),
            ]
            products = [
                Chunk(1, [
                    Unit("C", 1),
                    Unit("O", 2),
                ], "(g)"),
                Chunk(2, [
                    Unit("H", 2),
                    Unit("O", 1),
                ], "(l)"),
            ]
            Reaction(reactants, products)
        end,
        KiloJoulePerMol(-890),
    )

    equation2 = ThermochemicalEquation(
        let reactants = [
                Chunk(2, [
                    Unit("C", 1),
                    Unit("O", 1),
                ], "(g)"),
                Chunk(1, [
                    Unit("O", 2),
                ], "(g)"),
            ]
            products = [
                Chunk(2, [
                    Unit("C", 1),
                    Unit("O", 2),
                ], "(g)"),
            ]
            Reaction(reactants, products)
        end,
        KiloJoulePerMol(-566),
    )

    equation3 = let reactants = [
            Chunk(2, [
                Unit("C", 1),
                Unit("H", 4),
            ], "(g)"),
            Chunk(3, [
                Unit("O", 2),
            ], "(g)"),
        ]
        products = [
            Chunk(2, [
                Unit("C", 1),
                Unit("O", 1),
            ], "(g)"),
            Chunk(4, [
                Unit("H", 2),
                Unit("O", 1),
            ], "(l)"),
        ]
        Reaction(reactants, products)
    end
    println("Using:")
    println(equation1)
    println(equation2)
    println(equation3)
    println("-----------")
    enthalpy = enthalpy_of_reaction(
        [equation1,equation2],
        equation3
    )
    println(enthalpy)
end


function thermochemical_equation_example_2()
    equation1 = ThermochemicalEquation(
        let reactants = [
                Chunk(2, [
                    Unit("C", 2),
                    Unit("H", 2),
                ], "(g)"),
                Chunk(5, [
                    Unit("O", 2),
                ], "(g)"),
            ]
            products = [
                Chunk(4, [
                    Unit("C", 1),
                    Unit("O", 2),
                ], "(g)"),
                Chunk(2, [
                    Unit("H", 2),
                    Unit("O", 1),
                ], "(l)"),
            ]
            Reaction(reactants, products)
        end,
        KiloJoulePerMol(-2600.0),
    )
    equation2 = ThermochemicalEquation(
        let reactants = [
                Chunk(2, [
                    Unit("C", 2),
                    Unit("H", 6),
                ], "(g)"),
                Chunk(7, [
                    Unit("O", 2),
                ], "(g)"),
            ]
            products = [
                Chunk(4, [
                    Unit("C", 1),
                    Unit("O", 2),
                ], "(g)"),
                Chunk(6, [
                    Unit("H", 2),
                    Unit("O", 1),
                ], "(l)"),
            ]
            Reaction(reactants, products)
        end,
        KiloJoulePerMol(-3210.0),
    )

    equation3 = ThermochemicalEquation(
        let reactants = [
                Chunk(1, [
                    Unit("H", 2),
                ], "(g)"),
                Chunk(1//2, [
                    Unit("O", 2),
                ], "(g)"),
            ]
            products = [
                Chunk(1, [
                    Unit("H", 2),
                    Unit("O", 1),
                ], "(l)"),
            ]
            Reaction(reactants, products)
        end,
        KiloJoulePerMol(-286.0),
    )

    equation4 = let
        reactants = [
            Chunk(1, [
                Unit("C", 2),
                Unit("H", 2),
            ], "(g)"),
            Chunk(2, [
                Unit("H", 2),
            ], "(g)"),
        ]
        products = [
            Chunk(1, [
                Unit("C", 2),
                Unit("H", 6),
            ], "(g)"),
        ]
        Reaction(reactants, products)
    end

    println("Using:")
    println("\t", equation1)
    println("\t", equation2)
    println("\t", equation3)
    println("For:")
    println("\t", equation4)
    println("-----------")
    enthalpy = enthalpy_of_reaction(
        [equation1,equation2, equation3],
        equation4
    )
    println("Enthalpy of reaction: ", enthalpy)
end


###############################################################################
# PARSER
###############################################################################

module Parser


end

###############################################################################
# DEV
###############################################################################

# balance(new_reaction())
# println("************")
# thermochemical_equation_example_1()
# println("^^^^^^^^^^^")
thermochemical_equation_example_2()






end