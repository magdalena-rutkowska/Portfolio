# clean_names.py
# Modified by: Magdalena Rutkowska
# Modifications:
#   1. Added input validation to reject non-list input and non-string elements
#   2. Added case normalisation before deduplication (fixes silent bug in original)
#   3. Applied title case to output for consistent, human-readable formatting
#   4. Added error handling to surface problems clearly rather than silently misbehave


def process_names(names):
    # --- Input validation ---
    # Validate that the argument is actually a list; reject anything else explicitly.
    # Without this check, passing None or a string would cause a misleading error
    # deeper in the function, making debugging harder.
    if not isinstance(names, list):
        raise TypeError(f"Expected a list, got {type(names).__name__}.")

    # Reject an empty list early. An empty input is likely a caller error
    # (e.g. a database query returned nothing) and should be surfaced, not silently
    # returned as an empty result.
    if len(names) == 0:
        raise ValueError("The names list must not be empty.")

    # Validate that every element in the list is a string.
    # Mixed-type lists (e.g. None values from a database) would cause a silent
    # failure or a cryptic AttributeError during normalisation.
    for item in names:
        if not isinstance(item, str):
            raise TypeError(f"All list elements must be strings. Found: {type(item).__name__}.")

    # --- Case normalisation (modification 1 — fixes embedded bug) ---
    # The original script used set() directly on the raw input, which treated
    # "Alice", "alice", and "ALICE" as three distinct entries rather than one.
    # Normalising to lowercase before deduplication ensures case variants of the
    # same name are correctly recognised as duplicates.
    normalised = [name.lower() for name in names]

    # --- Deduplication ---
    # set() removes duplicates. Now that all entries are lowercase, this works
    # correctly. The original set() call was structurally correct but operated
    # on the wrong data (un-normalised strings).
    unique_names = list(set(normalised))

    # --- Sorting ---
    # Alphabetical sort. Operates consistently now that all strings are lowercase,
    # avoiding the original behaviour where uppercase letters sorted before lowercase
    # (e.g. "BOB" appeared before "alice" due to ASCII ordering).
    unique_names.sort()

    # --- Apply title case to output (modification 2) ---
    # Returns names in Title Case (e.g. "alice" → "Alice") for human-readable,
    # professionally formatted output. This is applied after deduplication and
    # sorting to avoid re-introducing case inconsistency into the set operation.
    return [name.title() for name in unique_names]


# --- Script entry point ---
# Only runs when the script is executed directly, not when imported as a module.
# This is a Python best practice that makes the function reusable in other scripts.
if __name__ == "__main__":
    test_names = ["Alice", "bob", "alice", "Charlie", "BOB", "dave", "Eve", "charlie"]

    print("Original list:", test_names)
    print("Processed list:", process_names(test_names))
