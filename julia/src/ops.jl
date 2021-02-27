using Base
using LinearAlgebraX
using LinearAlgebra



###############################################################################
# NODE
###############################################################################

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

# function get_limiting_reagent(reaction::Reaction)
#     reaction = balance(reaction)
# end



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
# UNIT TYPES
###############################################################################


###############################################################################
# THERMOCHEMICAL EQUATIONS
###############################################################################

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

