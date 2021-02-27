module ChemBot

include("data.jl")
include("ops.jl")
include("parser.jl")

using Printf
using Base
using LinearAlgebraX
using LinearAlgebra
using AutoHashEquals


###############################################################################
# NODE
###############################################################################


###############################################################################
# REACTION
###############################################################################


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
# UNIT TYPES (AD-HOCK FOR NOW)
###############################################################################


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
# DEV
###############################################################################

# balance(new_reaction())
# println("************")
# thermochemical_equation_example_1()
# println("^^^^^^^^^^^")
# thermochemical_equation_example_2()


end