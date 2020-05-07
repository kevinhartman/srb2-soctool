<p align="center">
  <a href="https://wiki.srb2.org/wiki/File:CREDIT05HQ.png">
  <img src="/banner.png" alt="Official artwork from credits of tails fixing a Robot." height="314" width="600">
  </a>
</p>

<h1 align="center">soctool (for SRB2)</h1>
A CLI tool designed to assist in the porting of SOC (Sonic Object Configuration) files.

Given valid SOC, emits valid SOC (or Lua!).

## Features
- **Port mode.** Automatically migrate old SOC files that use hard-coded slot numbers to freeslots, making them portable.
- **Generate Lua.** Upgrade SOC definitions to Lua.
- **Dependency info.** Generates a listing of all external references made by the SOC, as well as required asset files.
- **Selection and extraction.** Recursively extract only specific entities (by ID), as well as their local dependencies.

# Usage

## Selectors
All SOC input first must pass the selectors to be further processed.

Selectors (`--<entity>--ids`) filter the input SOC to only the desired entities (and by default, their local dependencies and freeslot declarations). `soctool` discards entities that don't match one of the selectors (they will not be processed, and will not appear in the output).

Selectors are useful for extracting specific entities from an otherwise unwieldy SOC.

They are **additive** filters: the more IDs you specify, the more you will select. You can combine them to select groups of unrelated entities, or omit them entirely to read and process the entire SOC.

To achieve *subtractive* filtering, chain multiple invocations of `soctool` together (pipe I/O).

## Port mode
In port mode (`--make-portable`), any selected entity definitions that install themselves to hard-coded slots (e.g. `Object 230`, `State 1041`, `Sound 150`) will be migrated to generated freeslots. Further, any hard-coded sprite IDs referenced by a selected state will also be allocated freeslots, as long as at least one referencing state will migrate to a freeslot as well.

Freeslot names (constants) are generated by combining the appropriate prefix for the resource type with as much of the user-provided freeslot prefix (`--freeslot-prefix`) as possible, followed by the original hard-coded slot number. The original slot number is included to guarantee uniqueness within the selection, and to help porters understand what's been migrated from where.

For example, if `--freeslot-prefix` is `GHOUL`:

| type   | slot | freeslot    |
|--------|------|-------------|
| Object | 123  | MT_GHOUL123 |
| State  | 2443 | S_GHOUL2443 |
| Sound  | 433  | sfx_gho433  |
| Sprite | 231  | SPR_GH6F    |

Note that for spites, the slot number is base-36 encoded, due its 4 character limit.

#### Important
After porting SOC entities, be sure to examine the dependency listing to determine which asset files are expected by the ported code.

## Dependency listing
By default, the generated output (SOC or Lua code) includes a listing of all external references** and required assets. If your output is missing dependencies from the original input SOC file, they will be listed as external references here, along with any other external references.

** There are some limitations to `soctool`'s dependency analysis that prevent certain dependencies from being recognized. See the Limitations section below for details.

The listing includes:

#### External <entity> references
Any references to entities not declared in the output. This includes hard-coded slot IDs (unless they have been overridden within the selection).

Mathematical expressions used in an entity reference context will appear here as well, even if the expression would resolve to a locally declared entity. This is because `soctool` does not evaluate expressions. You can use this information to manually augment your selectors to include these references.

#### Required sound files
Any sound lump names expected by locally* declared sounds.

\* Currently, this does not include hard-coded slot numbers, but it should.

#### Required sprite files
Any sprite lump names expected by locally declared sprites, excluding the rotation*.

A sprite is considered local if the selection contains a matching freeslot declaration. For example, if a State references a sprite by a hard-coded slot number and port mode is off, this slot number is considered to be external. If the same state is processed with port mode on, the sprite would be migrated to a freeslot**, making it local.

\* Rotation is omitted because not all sprites need files for all rotations. For more information on this, see [Sprites](https://wiki.srb2.org/wiki/Sprite).

\** Sprites are only migrated to freeslots if at least one corresponding state in the same selection is also migrated.

#### Linedef executors
The IDs of any Linedef executors called.

## Arguments
This section describes the available command-line arguments. They're separated into two categories below only to help you find what you need.

### Selector arguments
These arguments filter the input SOC to only the listed entities (and by default, their local dependencies).

They are **additive** filters: the more IDs you specify, the more you will select.

| arg                           | short | type   | description                                                                                                                                                   |
|-------------------------------|-------|--------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--no-recurse`                | -R    | flag   | Select only the entities identified, without recursively selecting their dependencies. Only applicable if at least one entity selector argument is specified. |
| `--object-ids <id>[,<id>]...` | -o    | csv    | Select these objects.                                                                                                                                         |
| `--state-ids <id>[,<id>]...`  | -s    | csv    | Select these states.                                                                                                                                          |
| `--sound-ids <id>[,<id>]...`  | -d    | csv    | Select these sounds.                                                                                                                                          |
| `--level-ids <id>[,<id>]...`  | -l    | csv    | Select these levels.                                                                                                                                          |

### Action arguments
These arguments specify the processing behavior to apply to the selection.

| arg                    | short | type   | description                                                                                                                                                                                                                                                                                                                                           |
|------------------------|-------|--------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--input-soc`          | -i    | path   | When provided, the input SOC's contents will be read from this file instead of `STDIN`.                                                                                                                                                                                                                                                               |
| `--make-portable`      | -p    | flag   | When provided, all selected SOC entity definitions using hard-coded slots will be upgraded to freeslots. Any local references (within the selection) will also be upgraded to point to the new freeslot locations.                                                                                                                                    |
| `--freeslot-prefix`    | -f    | string | When in port mode (`--make-portable`), specifies a friendly name prefix to use for generated freeslot names. As much of this prefix will be used as possible, though due to length restrictions for certain slot types, it will likely be truncated. For best results, provide at most 4 characters that will help you to identify related entities.  |
| `--to-lua`             | -S    | flag   | Emit results as Lua code. Keep in mind that this tool cannot read Lua, so if you plan to do multiple passes by chaining invocations, save this parameter for your final invocation.                                                                                                                                                                   |
| `--from-old-srb2`      | -u    | flag   | [Not very useful at the moment] Attempt to upgrade old versions of actions (e.g. if they've been renamed or had parameter changes). Currently, this just changes A_RingExplode calls to A_OldRingExplode if the state's `var1` contains an object ID.                                                                                                 |
| `--no-describe`        | -D    | flag   | Omit the default block comment with dependency information from the output.                                                                                                                                                                                                                                                                           |
| `--no-inline-comments` | -I    | flag   | Suppress the generation of warnings as inline comments. Inline comments are generated by default with information specific to particular entities that `soctool` thinks may be helpful to porters. Once you're done porting, this flag might be a good idea for a final pass to clean things up.                                                      |
| `--no-attribution`     | -A    | flag   | Suppress the generation of the attribution message.                                                                                                                                                                                                                                                                                                   |
| `--help`               | -?    | flag   | Print the help text.                                                                                                                                                                                                                                                                                                                                  |

## Limitations
### Port mode
Slot number references cannot be ported unless they are numeric constants. This means that `soctool` won't port slot numbers that are involved in mathematical expressions.

This is because `soctool` does not evaluate expressions. To do this, it would need to know all constants defined in SRB2 (some of which may only be known at runtime). This just isn't worth doing, since old SOC code that hard-codes slot numbers is unlikely to be using mathematical expressions anyway.

### Dependency analysis
#### Expression contexts
Because `soctool` does not evaluate expressions, it cannot follow references calculated by them. This means that selectors cannot recursively include entities that are referenced within a mathematical expression.

However, **these expressions will be included in the dependency listing**, even if the referenced entities are declared locally. You can use this information to manually augment your selectors to include these references.

#### Actions
As of SRB2 2.2, `soctool` is aware of all documented built-in `Actions` which accept an entity reference through either `Var1` or `Var2`. Almost all of these references are recognized, and will be recursively expanded by selectors (if possible) and included in the dependency listing.

However, some `Actions` are not supported due to non-standard implementations.

Partially supported:
- `A_CheckThingCount` and `A_MultiShot` use the upper 16 bits of `Var1` to store object references. Recursive selections will never follow these references, and they will never be ported. Their `Var1` expression will however always be included in the dependency reference listing.

No support:

- `A_DropMine` and `A_ShootBullet` use the `RaiseState` field on their actor's object to store an ***object*** reference. This is not supported by `soctool`'s current design. In fact, if an object's `RaiseState` contains an object reference, there's a possibility the object will be interpreted as a state reference (but only if there exists a locally declared State with the same name).
- `A_RandomStateRange` is not supported, since it would require runtime evaluation of `Var1` and `Var2`.
- `A_CusValAction`, `A_RelayCustomValue`, `A_SetCustomValue`, `A_UseCusValMemo` would require runtime evaluation to properly support.

If `soctool` encounters one of these completely unsupported actions, it will generate an appropriate warning.
# Prerequisite software
`soctool` is written in Scala, and therefore requires the JVM to run.
