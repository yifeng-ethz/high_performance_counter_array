package require Tcl 8.5

set script_dir [file dirname [info script]]
set helper_file [file normalize [file join $script_dir .. dashboard_infra cmsis_svd lib mu3e_cmsis_svd.tcl]]
source $helper_file

namespace eval ::mu3e::cmsis::spec {}

proc ::mu3e::cmsis::spec::build_device {} {
    return [::mu3e::cmsis::svd::device MU3E_COUNTER_AVMM \
        -version 1.4.1 \
        -description "CMSIS-SVD description of the counter_avmm value aperture. This first-pass contract exposes the 32-word relative window as read-only counter words; write-side clear semantics can be added later by the IP author if they are meant to be operator-visible." \
        -peripherals [list \
            [::mu3e::cmsis::svd::peripheral COUNTER_AVMM_VALUE 0x0 \
                -description "Relative 32-word counter-value aperture." \
                -groupName MU3E_DATA_PATH \
                -addressBlockSize 0x80 \
                -registers [::mu3e::cmsis::svd::word_window_registers 32 \
                    -descriptionPrefix "Counter AVMM word" \
                    -fieldDescriptionPrefix "Raw counter AVMM word" \
                    -access read-only]]]]
}

if {[info exists ::argv0] &&
    [file normalize $::argv0] eq [file normalize [info script]]} {
    set out_path [file join $script_dir counter_avmm.svd]
    if {[llength $::argv] >= 1} {
        set out_path [lindex $::argv 0]
    }
    ::mu3e::cmsis::svd::write_device_file \
        [::mu3e::cmsis::spec::build_device] $out_path
}
