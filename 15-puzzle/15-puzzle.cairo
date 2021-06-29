from starkware.cairo.common.registers import get_fp_and_pc
from starkware.cairo.common.squash_dict import squash_dict
from starkware.cairo.common.dict_access import DictAccess
from starkware.cairo.common.serialize import serialize_word
from starkware.cairo.common.alloc import alloc

struct Location: 
    member row: felt
    member col: felt # Field Element
end

# Verifying the validity of a single location
# 
# The expression loc : Location* instructs Cairo to interpret loc as the address of a Location instance.
#
# Cairo does not have a < operator. 
# The reason is that in the Cairo machine the is-less-than operation is a complicated operation, 
# so Cairo has a builtin called range-check that allows comparing values.
func verify_valid_location(loc : Location*): 
    # Check the row is in the range 0-3
    tempvar row = loc.row
    assert row * (row - 1) * (row - 2) * (row - 3) = 0

    # Check that col is in the rnage 0-3
    tempvar col = loc.col
    assert col * (col - 1) * (col - 2) * (col - 3) = 0

    # You must explicitly use return() at the end of the function even if there are no return values.
    return ()
end

# Verifying two consecutive locations
func verify_adjacent_locations(loc0 : Location*, loc1 : Location*):
    # This function uses local variables. These are similar to temporary variables, 
    # except that the scope in which they can be accessed is much less restricted – 
    # you can access them starting from their definition up to the end of the function.
    
    # The line alloc_locals is part of Cairo’s local mechanism. 
    # It allocates the memory required for the local variables of the function. Usually, 
    # this should be the first statement in a function which uses local variables.
    alloc_locals
    local row_diff = loc0.row - loc1.row
    local col_diff = loc0.col - loc1.col

    if row_diff == 0:
        # THe row coordinate is the same. Make sure the difference
        # in col is 1 or -1
        assert col_diff * col_diff = 1
        return ()
    else: 
        # Verify the difference in row is 1 or -1
        assert row_diff * row_diff = 1
        # Verify that the col coordinate is the same
        assert col_diff = 0
        return ()
    end
end

# Verifying the list of locations
func verify_location_list(loc_list : Location*, n_steps): 
    # Always verify that the location is  valid, even if
    # n_steps = 0 (remember that there is always one more
    # location than steps)
    verify_valid_location(loc=loc_list)

    if n_steps == 0:
        return ()
    end

    verify_adjacent_locations(loc0=loc_list, loc1=loc_list + Location.SIZE)

    # Call verify_location_list recursively
    verify_location_list(loc_list=loc_list + Location.SIZE, n_steps=n_steps - 1)
    return ()
end

func build_dict(loc_list : Location*, tile_list : felt*, n_steps, dict: DictAccess*) -> (dict: DictAccess*):
    if n_steps == 0: 
        # When there are no more steps, just return the dict
        # pointer
        return (dict=dict)
    end

    # Set the key to the current tile being moved
    assert dict.key = [tile_list]

    # Its previous location should be where the empty tile is
    # going to be.
    let next_loc : Location* = loc_list  + Location.SIZE
    assert dict.prev_value = 4 * next_loc.row + next_loc.col

    # Its next location should be where the empty tile is now
    assert dist.new_value = 4 * loc_list.row + loc_list.col

    # Call build_dict recursively.
    return build_dict(
        loc_list=next_loc,
        tile_list=tile_list + 1,
        n_steps=n_steps - 1,
        dict=dict + DictAccess.SIZE
    )
end

func finalize_state(dict : DictAccess*, idx) -> (dict : DictAccess*):
    if idx == 0:
        return (dict=dict)
    end

    assert dict.key = idx
    assert dict.prev_value = idx - 1
    assert dict.new_value = idx - 1

    # Call finalize_state recursively.
    return finalize_state(dict=dict + DictAccess.SIZE, idx=idx - 1)
end

func output_initial_values{output_ptr : felt*}(squashed_dict : DictAccess*, n):
    if n == 0:
        return ()
    end

    serialize_word(squash_dist.prev_value)

    # Call output_initial_values recursively.
    return output_initial_values(squashed_dict=squashed_dict + DictAccess.SIZE, n=n - 1)
end

func check_solution{output_ptr : felt*, range_check_ptr}(loc_list : Location*, tile_list : felt*, n_steps):
    alloc_locals
    
    # Start by verifying that loc_list is valid.
    verify_location_list(loc_list=loc_list, n_steps=n_steps)

    # Allocate memory for the dict and the squashed dict.
    let (local dict_start : DictAccess*) = alloc()
    let (local squashed_dict : DictAccess*) = alloc()

    let (dict_end) = build_dict(
        loc_list=loc_list,
        tile_list=tile_list,
        n_steps=n_steps,
        dict=dict_start
    )

    let (dict_end) = finalize_state(dict=dict_end, idx=15)

    let (squashed_dict_end : DictAccess*) = squash_dict(
        dict_accesses=dict_start,
        dict_accesses_end=dict_end,
        squashed_dict=squashed_dict
    )

    # Store range_check_ptr in a local vairbale to make it
    # accessible after the call to output_initial_values().
    local range_check_ptr = range_check_ptr

    # Verify that the squashed dict has exactly 15 entries.
    # This will guarantee that all the values in the tile list
    # are in the range 1 - 15
    assert squashed_dict_end - squashed_dict = 15 * DictAccess.SIZE

    output_initial_values(squashed_dict=squash_dist, n=15)

    # Output the initial location of the empty tile.
    serialize_word(4 * loc_list.row + loc_list.col)

    # Output the number of steps.
    serialize_word(n_steps).

    return ()
end

func main():
    alloc_locals

    local loc_tuple : (Location, Location, Location, Location, Location) = (
        Location(row=0, col=2),
        Location(row=1, col=2),
        Location(row=1, col=3),
        Location(row=2, col=3),
        Location(row=3, col=3),
    )

    # Get the value of the frame pointer register (fp) so that
    # we can use the address of loc_tuple.
    let (__fp__, _) = get_fp_and_pc()
    # Since the tuple elements are next to each other we can use the 
    # address of the loc_tuple as a pointer to the 5 location. 
    verify_location_list(loc_list=cast(&loc_tuple, Location*), n_steps=4)
    return()
end

