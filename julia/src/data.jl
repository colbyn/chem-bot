using Printf
using Base
using AutoHashEquals

###############################################################################
# CONSTANTS
###############################################################################

const DEBUG_MODE = false

###############################################################################
# COMMON HELPER TYPES
###############################################################################

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

