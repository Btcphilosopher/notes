###############################################################
# RHINO PRESSCRAFT
# Bespoke / Heritage Printing Press Quality Optimizer
#
# Julia 1.10+
#
# Designed for:
#   - letterpress
#   - hand-fed presses
#   - cylinder presses
#   - restored mechanical presses
#   - bespoke fine-art printing
#   - archival / luxury stationery
#
# PRIMARY OBJECTIVE:
#
#        MAXIMUM PRINT QUALITY
#
# rather than:
#
#        MAXIMUM THROUGHPUT
#
###############################################################

using Statistics
using LinearAlgebra
using Printf
using Random


###############################################################
# 1. PRESS MODEL
###############################################################

struct Press

    name::String

    maximum_pressure_kN::Float64

    nominal_pressure_kN::Float64

    platen_alignment_mm::Float64

    roller_speed_mm_s::Float64

    bed_temperature_C::Float64

    ambient_temperature_C::Float64

    paper_moisture_pct::Float64

    ink_viscosity_cP::Float64

end


###############################################################
# 2. PAPER
###############################################################

struct Paper

    name::String

    gsm::Float64

    thickness_mm::Float64

    moisture_pct::Float64

    absorbency::Float64

    surface_roughness::Float64

    compressibility::Float64

end


###############################################################
# 3. INK
###############################################################

struct Ink

    name::String

    viscosity_cP::Float64

    tack::Float64

    pigment_density::Float64

    drying_rate::Float64

end


###############################################################
# 4. PRINT QUALITY TARGET
###############################################################

struct QualityTarget

    registration_target_mm::Float64

    pressure_uniformity_target::Float64

    ink_density_target::Float64

    ink_density_tolerance::Float64

    maximum_dot_gain_pct::Float64

    maximum_surface_damage::Float64

end


###############################################################
# 5. PRESS SETTINGS
###############################################################

struct PressSettings

    pressure_kN::Float64

    roller_speed_mm_s::Float64

    ink_feed_pct::Float64

    impression_time_ms::Float64

    roller_pressure_kN::Float64

    dwell_time_ms::Float64

end


###############################################################
# 6. PRINT MEASUREMENT
###############################################################

struct PrintMeasurement

    registration_error_mm::Float64

    pressure_uniformity::Float64

    ink_density::Float64

    dot_gain_pct::Float64

    surface_damage::Float64

    ink_smearing::Float64

    emboss_depth_mm::Float64

end


###############################################################
# 7. QUALITY SCORE
###############################################################

function quality_score(
    measurement::PrintMeasurement,
    target::QualityTarget
)

    # Registration score
    registration_score =
        max(
            0.0,
            1.0 -
            measurement.registration_error_mm /
            target.registration_target_mm
        )

    # Pressure score
    pressure_score =
        min(
            measurement.pressure_uniformity /
            target.pressure_uniformity_target,
            1.0
        )

    # Ink density score
    density_error =
        abs(
            measurement.ink_density -
            target.ink_density_target
        )

    density_score =
        max(
            0.0,
            1.0 -
            density_error /
            target.ink_density_tolerance
        )

    # Dot gain
    dot_gain_score =
        max(
            0.0,
            1.0 -
            measurement.dot_gain_pct /
            target.maximum_dot_gain_pct
        )

    # Surface preservation
    surface_score =
        max(
            0.0,
            1.0 -
            measurement.surface_damage /
            target.maximum_surface_damage
        )

    # Smearing
    smear_score =
        max(
            0.0,
            1.0 -
            measurement.ink_smearing
        )

    score =
        100.0 *
        (
            0.25 * registration_score +
            0.20 * pressure_score +
            0.25 * density_score +
            0.10 * dot_gain_score +
            0.10 * surface_score +
            0.10 * smear_score
        )

    return clamp(score, 0.0, 100.0)
end


###############################################################
# 8. PRESS PHYSICS MODEL
###############################################################

"""
Approximate pressure distribution across the platen.

A restored press will rarely have perfectly uniform pressure.
"""

function pressure_uniformity(
    settings::PressSettings,
    press::Press
)

    pressure_ratio =
        settings.pressure_kN /
        press.nominal_pressure_kN

    alignment_error =
        abs(
            press.platen_alignment_mm
        )

    pressure_penalty =
        0.10 *
        alignment_error

    overload_penalty =
        max(
            pressure_ratio - 1.0,
            0.0
        ) * 0.5

    uniformity =
        1.0 -
        pressure_penalty -
        overload_penalty

    return clamp(
        uniformity,
        0.0,
        1.0
    )
end


###############################################################
# 9. REGISTRATION MODEL
###############################################################

function registration_error(
    settings::PressSettings,
    press::Press
)

    # Faster operation increases mechanical registration
    # variability.

    speed_factor =
        settings.roller_speed_mm_s /
        max(
            press.roller_speed_mm_s,
            1.0
        )

    pressure_factor =
        abs(
            settings.pressure_kN -
            press.nominal_pressure_kN
        ) /
        press.nominal_pressure_kN

    base_error =
        press.platen_alignment_mm

    dynamic_error =
        0.02 *
        speed_factor

    pressure_error =
        0.04 *
        pressure_factor

    return (
        base_error +
        dynamic_error +
        pressure_error
    )
end


###############################################################
# 10. INK TRANSFER MODEL
###############################################################

function ink_density(
    settings::PressSettings,
    ink::Ink,
    paper::Paper
)

    feed =
        settings.ink_feed_pct /
        100.0

    viscosity_ratio =
        ink.viscosity_cP /
        100.0

    transfer =
        feed *
        (
            0.70 +
            0.30 *
            clamp(
                viscosity_ratio,
                0.5,
                1.5
            )
        )

    absorption_loss =
        paper.absorbency *
        0.15

    density =
        transfer *
        ink.pigment_density *
        (1.0 - absorption_loss)

    return density
end


###############################################################
# 11. DOT GAIN MODEL
###############################################################

function dot_gain(
    settings::PressSettings,
    ink::Ink,
    paper::Paper
)

    pressure =
        settings.pressure_kN /
        10.0

    dwell =
        settings.impression_time_ms /
        100.0

    absorption =
        paper.absorbency

    viscosity =
        ink.viscosity_cP /
        100.0

    gain =
        2.0 *
        pressure +
        1.2 *
        dwell +
        3.0 *
        absorption -
        1.0 *
        viscosity

    return max(
        gain,
        0.0
    )
end


###############################################################
# 12. SURFACE DAMAGE MODEL
###############################################################

function surface_damage(
    settings::PressSettings,
    paper::Paper
)

    pressure =
        settings.pressure_kN /
        10.0

    compressibility =
        paper.compressibility

    damage =
        pressure *
        compressibility *
        0.08

    return clamp(
        damage,
        0.0,
        1.0
    )
end


###############################################################
# 13. SMEAR MODEL
###############################################################

function ink_smearing(
    settings::PressSettings,
    ink::Ink,
    paper::Paper
)

    speed =
        settings.roller_speed_mm_s

    drying =
        max(
            ink.drying_rate,
            0.01
        )

    feed =
        settings.ink_feed_pct /
        100.0

    smear =
        (
            speed /
            100.0
        ) *
        feed /
        drying

    return clamp(
        smear,
        0.0,
        1.0
    )
end


###############################################################
# 14. SIMULATE PRINT
###############################################################

function simulate_print(
    press::Press,
    paper::Paper,
    ink::Ink,
    settings::PressSettings
)

    registration =
        registration_error(
            settings,
            press
        )

    uniformity =
        pressure_uniformity(
            settings,
            press
        )

    density =
        ink_density(
            settings,
            ink,
            paper
        )

    gain =
        dot_gain(
            settings,
            ink,
            paper
        )

    damage =
        surface_damage(
            settings,
            paper
        )

    smear =
        ink_smearing(
            settings,
            ink,
            paper
        )

    emboss =
        settings.pressure_kN *
        0.003 *
        paper.compressibility

    return PrintMeasurement(

        registration,

        uniformity,

        density,

        gain,

        damage,

        smear,

        emboss
    )
end


###############################################################
# 15. OPTIMIZER
###############################################################

function optimize_press(
    press::Press,
    paper::Paper,
    ink::Ink,
    target::QualityTarget
)

    candidates =
        Vector{Tuple{
            PressSettings,
            PrintMeasurement,
            Float64
        }}()

    ###########################################################
    # Search deliberately favours gentle operation.
    ###########################################################

    for pressure in
        range(
            press.nominal_pressure_kN * 0.70,
            press.nominal_pressure_kN * 1.10,
            length=15
        )

        for speed in
            range(
                press.roller_speed_mm_s * 0.30,
                press.roller_speed_mm_s,
                length=10
            )

            for feed in
                range(
                    50.0,
                    110.0,
                    length=13
                )

                for dwell in
                    range(
                        20.0,
                        150.0,
                        length=7
                    )

                    settings =
                        PressSettings(

                            pressure,

                            speed,

                            feed,

                            dwell,

                            pressure * 0.25,

                            dwell * 0.50
                        )

                    measurement =
                        simulate_print(
                            press,
                            paper,
                            ink,
                            settings
                        )

                    score =
                        quality_score(
                            measurement,
                            target
                        )

                    push!(
                        candidates,
                        (
                            settings,
                            measurement,
                            score
                        )
                    )
                end
            end
        end
    end

    sort!(
        candidates,
        by = x -> x[3],
        rev = true
    )

    return candidates
end


###############################################################
# 16. REPORT
###############################################################

function print_report(
    result,
    target
)

    settings,
    measurement,
    score = result

    println()
    println("=" ^ 65)
    println(" RHINOPRESS QUALITY OPTIMIZATION")
    println("=" ^ 65)

    println()
    println("OPTIMAL SETTINGS")
    println("-" ^ 65)

    @printf(
        "Impression pressure : %.2f kN\n",
        settings.pressure_kN
    )

    @printf(
        "Roller speed        : %.2f mm/s\n",
        settings.roller_speed_mm_s
    )

    @printf(
        "Ink feed            : %.1f %%\n",
        settings.ink_feed_pct
    )

    @printf(
        "Impression time     : %.1f ms\n",
        settings.impression_time_ms
    )

    @printf(
        "Roller pressure     : %.2f kN\n",
        settings.roller_pressure_kN
    )

    @printf(
        "Dwell time          : %.1f ms\n",
        settings.dwell_time_ms
    )

    println()
    println("PREDICTED PRINT")
    println("-" ^ 65)

    @printf(
        "Registration error  : %.4f mm\n",
        measurement.registration_error_mm
    )

    @printf(
        "Pressure uniformity : %.4f\n",
        measurement.pressure_uniformity
    )

    @printf(
        "Ink density         : %.4f\n",
        measurement.ink_density
    )

    @printf(
        "Dot gain            : %.2f %%\n",
        measurement.dot_gain_pct
    )

    @printf(
        "Surface damage      : %.4f\n",
        measurement.surface_damage
    )

    @printf(
        "Ink smearing        : %.4f\n",
        measurement.ink_smearing
    )

    @printf(
        "Emboss depth        : %.4f mm\n",
        measurement.emboss_depth_mm
    )

    println()
    println("=" ^ 65)

    @printf(
        "QUALITY SCORE: %.2f / 100\n",
        score
    )

    println("=" ^ 65)
end


###############################################################
# 17. MACHINE CALIBRATION
###############################################################

function calibrate_press(
    press::Press,
    measurements::Vector{PrintMeasurement}
)

    if isempty(measurements)

        return Dict(
            "status" => "NO_DATA"
        )
    end

    return Dict(

        "status" => "CALIBRATED",

        "mean_registration_error" =>
            mean(
                m.registration_error_mm
                for m in measurements
            ),

        "mean_pressure_uniformity" =>
            mean(
                m.pressure_uniformity
                for m in measurements
            ),

        "mean_ink_density" =>
            mean(
                m.ink_density
                for m in measurements
            ),

        "mean_dot_gain" =>
            mean(
                m.dot_gain_pct
                for m in measurements
            ),

        "mean_surface_damage" =>
            mean(
                m.surface_damage
                for m in measurements
            )
    )
end


###############################################################
# 18. QUALITY CONTROL
###############################################################

function quality_control(
    measurement::PrintMeasurement,
    target::QualityTarget
)

    failures = String[]

    if measurement.registration_error_mm >
       target.registration_target_mm

        push!(
            failures,
            "REGISTRATION"
        )
    end

    if measurement.pressure_uniformity <
       target.pressure_uniformity_target

        push!(
            failures,
            "PRESSURE_UNIFORMITY"
        )
    end

    if abs(
        measurement.ink_density -
        target.ink_density_target
    ) >
       target.ink_density_tolerance

        push!(
            failures,
            "INK_DENSITY"
        )
    end

    if measurement.dot_gain_pct >
       target.maximum_dot_gain_pct

        push!(
            failures,
            "DOT_GAIN"
        )
    end

    if measurement.surface_damage >
       target.maximum_surface_damage

        push!(
            failures,
            "SURFACE_DAMAGE"
        )
    end

    return failures
end


###############################################################
# 19. EXAMPLE PRESS
###############################################################

press = Press(

    "Bespoke Heritage Press",

    30.0,       # maximum pressure

    18.0,       # nominal pressure

    0.08,       # platen alignment

    80.0,       # roller speed

    22.0,       # bed temperature

    21.0,       # ambient temperature

    7.5,        # paper moisture

    450.0       # ink viscosity
)


###############################################################
# 20. EXAMPLE PAPER
###############################################################

paper = Paper(

    "Cotton Rag 300gsm",

    300.0,

    0.45,

    7.5,

    0.25,

    0.18,

    0.65
)


###############################################################
# 21. EXAMPLE INK
###############################################################

ink = Ink(

    "Traditional Letterpress Black",

    420.0,

    0.75,

    1.25,

    0.85
)


###############################################################
# 22. QUALITY TARGET
###############################################################

target = QualityTarget(

    0.10,       # registration tolerance

    0.95,       # pressure uniformity

    0.85,       # target ink density

    0.08,       # density tolerance

    8.0,        # max dot gain

    0.20        # max surface damage
)


###############################################################
# 23. OPTIMIZE
###############################################################

println()
println("Starting quality optimization...")

solutions =
    optimize_press(
        press,
        paper,
        ink,
        target
    )


###############################################################
# 24. BEST SOLUTION
###############################################################

best =
    first(solutions)

print_report(
    best,
    target
)


###############################################################
# 25. QUALITY CONTROL
###############################################################

settings,
measurement,
score = best

failures =
    quality_control(
        measurement,
        target
    )

println()

if isempty(failures)

    println(
        "QC RESULT: PASS"
    )

else

    println(
        "QC RESULT: FAIL"
    )

    println(
        "Failed parameters:"
    )

    for failure in failures

        println(
            "  - ",
            failure
        )

    end
end


###############################################################
# 26. TOP 10 SETTINGS
###############################################################

println()
println("=" ^ 65)
println(" TOP 10 QUALITY SETTINGS")
println("=" ^ 65)

for (i, solution) in
    enumerate(
        first(
            solutions,
            min(10, length(solutions))
        )
    )

    settings,
    measurement,
    score = solution

    @printf(
        "%2d | Score %.2f | Pressure %.2fkN | " *
        "Speed %.1f | Ink %.1f%% | " *
        "Registration %.3fmm\n",

        i,

        score,

        settings.pressure_kN,

        settings.roller_speed_mm_s,

        settings.ink_feed_pct,

        measurement.registration_error_mm
    )
end


###############################################################
# 27. END
###############################################################
