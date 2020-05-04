object PrintHelp {
  def apply() = {
    println("""
        |NAME
        |       soctool - A tool for porting SOC (Sonic Object Configuration) files.
        |
        |SYNOPSIS
        |       soctool [options]
        |
        |DESCRIPTION
        |        Selectors
        |        ---------
        |        All SOC input first must pass the selectors to be further processed.
        |
        |        Selectors (`--<entity>--ids`) filter the input SOC to only the desired entities (and by default, their
        |        local dependencies and freeslot declarations). `soctool` discards entities that don't match one of the
        |        selectors (they will not be processed, and will not appear in the output).
        |
        |        Selectors are useful for extracting specific entities from an otherwise unwieldy SOC.
        |
        |        They are **additive** filters: the more IDs you specify, the more you will select. You can combine them
        |        to select groups of unrelated entities, or omit them entirely to read and process the entire SOC.
        |
        |        To achieve *subtractive* filtering, chain multiple invocations of `soctool` together (pipe I/O).
        |
        |        Port mode
        |        ---------
        |        In port mode (`--make-portable`), any selected entity definitions that install themselves to hard-coded
        |        slots (e.g. `Object 230`, `State 1041`, `Sound 150`) will be migrated to generated freeslots. Further,
        |        any hard-coded sprite IDs referenced by a selected state will also be allocated freeslots, as long as
        |        at least one referencing state will migrate to a freeslot as well.
        |
        |        Freeslot names (constants) are generated by combining the appropriate prefix for the resource type with
        |        as much of the user-provided freeslot prefix (`--freeslot-prefix`) as possible, followed by the
        |        original hard-coded slot number. The original slot number is included to guarantee uniqueness within
        |        the selection, and to help porters understand what's been migrated from where.
        |
        |        For example, if `--freeslot-prefix` is `GHOUL`:
        |
        |        | type   | slot | freeslot    |
        |        |--------|------|-------------|
        |        | Object | 123  | MT_GHOUL123 |
        |        | State  | 2443 | S_GHOUL2443 |
        |        | Sound  | 433  | sfx_gho433  |
        |        | Sprite | 231  | SPR_GH6F    |
        |
        |        Note that for spites, the slot number is base-36 encoded, due its 4 character limit.
        |
        |        !!! Important !!!
        |        After porting SOC entities, be sure to examine the dependency listing to determine which asset files
        |        are expected by the ported code.
        |
        |        Dependency listing
        |        ------------------
        |        By default, the generated output (SOC or Lua code) includes a listing of all external references and
        |        required assets. If your output is missing dependencies from the original input SOC file, they will be
        |        listed as external references here, along with any other external references.
        |
        |        The listing includes:
        |
        |        External <entity> references:
        |          Any references to entities not declared in the output. This includes hard-coded slot IDs (unless they
        |          have been overridden within the selection).
        |
        |        Required sound files:
        |          Any sound file names expected by the output.
        |
        |        Required sprite files:
        |          Any sprite file names expected, excluding the rotation. Rotation is omitted because not all sprites
        |          need files for all rotations. For more information on this, see https://wiki.srb2.org/wiki/Sprite.
        |
        |        Linedef executors:
        |          The IDs of any Linedef executors called.
        |
        |OPTIONS
        |        This section describes the available command-line arguments. They're separated into two categories
        |        below only to help you find what you need.
        |
        |        Selector arguments
        |        ------------------
        |        These arguments filter the input SOC to only the listed entities (and by default, their local
        |        dependencies).
        |
        |        They are **additive** filters: the more IDs you specify, the more you will select.
        |
        |        arg                           short   type     description
        |        -------------------------------------------------------------------------------------------------------------------------------------
        |        --no-recurse                  -R      flag     Select only the entities identified, without recursively selecting their dependencies.
        |                                                       Only applicable if at least one entity selector argument is specified.
        |        --object-ids <id>[,<id>]...   -o      csv      Select these objects.
        |        --state-ids <id>[,<id>]...    -s      csv      Select these states.
        |        --sound-ids <id>[,<id>]...    -d      csv      Select these sounds.
        |        --level-ids <id>[,<id>]...    -l      csv      Select these levels.
        |
        |        Action arguments
        |        ----------------
        |        These arguments specify the processing behavior to apply to the selection.
        |
        |        arg                    short   type     description                                                                                                                                                                                                                                                                                                                                           |
        |        -------------------------------------------------------------------------------------------------------------------------------------
        |        --input-soc            -i      path     When provided, the input SOC's contents will be read from this file instead of STDIN.
        |
        |        --make-portable        -p      flag     When provided, all selected SOC entity definitions using hard-coded slots will be upgraded to
        |                                                freeslots. Any local references (within the selection) will also be upgraded to point to the
        |                                                new freeslot locations.
        |
        |        --freeslot-prefix      -f      string   When in port mode (--make-portable), specifies a friendly name prefix to use for generated
        |                                                freeslot names. As much of this prefix will be used as possible, though due to length
        |                                                restrictions for certain slot types, it will likely be truncated. For best results, provide at
        |                                                most 4 characters that will help you to identify related entities.
        |
        |        --to-lua               -S      flag     Emit results as Lua code. Keep in mind that this tool cannot read Lua, so if you plan to do
        |                                                multiple passes by chaining invocations, save this parameter for your final invocation.
        |
        |        --from-old-srb2        -u      flag     [Not very useful at the moment] Attempt to upgrade old versions of actions (e.g. if they've
        |                                                been renamed or had parameter changes). Currently, this just changes A_RingExplode calls to
        |                                                A_OldRingExplode if the state's var1 contains an object ID.
        |
        |        --no-describe          -D      flag     Omit the default block comment with dependency information from the output.
        |
        |        --no-inline-comments   -I      flag     Suppress the generation of warnings as inline comments. Inline comments are generated by default
        |                                                with information specific to particular entities that soctool thinks may be helpful to porters.
        |                                                Once you're done porting, this flag might be a good idea for a final pass to clean things up.
        |
        |        --no-attribution       -A      flag     Suppress the generation of the attribution message.
        |
        |        --help                 -?      flag     Print this help message.
        |
        |""".stripMargin
    )
  }
}
