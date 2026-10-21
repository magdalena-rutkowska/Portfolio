// cleanNames.js
// Modified by: Magdalena Rutkowska
// Modifications:
//   1. Added input validation to reject non-array input and non-string elements
//   2. Added case normalisation before deduplication (fixes silent bug in original)
//   3. Applied title case to output for consistent, human-readable formatting
//   4. Added error handling to surface problems clearly rather than silently misbehave


// --- Helper function: toTitleCase ---
// JavaScript has no built-in title case method (unlike Python's str.title()).
// This helper capitalises the first character and lowercases the remainder.
// Defined separately to keep processNames readable and to allow independent reuse.
function toTitleCase(str) {
    return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
}


function processNames(names) {

    // --- Input validation ---
    // Validate that the argument is an array. In JavaScript, typeof [] === "object",
    // so Array.isArray() is the correct check. Without this, passing a string would
    // iterate over individual characters, producing silent nonsense output.
    if (!Array.isArray(names)) {
        throw new TypeError(`Expected an array, got ${typeof names}.`);
    }

    // Reject an empty array early. An empty input is likely a caller error and
    // should be surfaced explicitly rather than silently returning an empty result.
    if (names.length === 0) {
        throw new Error("The names array must not be empty.");
    }

    // Validate that every element in the array is a string.
    // Mixed-type arrays (e.g. null values from an API response) would cause a
    // silent failure or a misleading error during normalisation.
    names.forEach((item, index) => {
        if (typeof item !== "string") {
            throw new TypeError(
                `All array elements must be strings. Found ${typeof item} at index ${index}.`
            );
        }
    });

    // --- Case normalisation (modification 1 — fixes embedded bug) ---
    // The original script passed raw strings directly into Set(), which treated
    // "Alice", "alice", and "ALICE" as three distinct entries rather than one.
    // Normalising to lowercase before deduplication ensures case variants of the
    // same name are correctly recognised as duplicates.
    const normalised = names.map(name => name.toLowerCase());

    // --- Deduplication ---
    // Spread syntax converts the Set back into an array. Now that all entries are
    // lowercase, deduplication works correctly. The original [...new Set(names)]
    // was structurally correct but operated on un-normalised strings.
    const unique = [...new Set(normalised)];

    // --- Sorting ---
    // Alphabetical sort. Operates consistently now that all strings are lowercase,
    // avoiding the original behaviour where uppercase letters sorted before lowercase
    // due to Unicode/ASCII ordering.
    unique.sort();

    // --- Apply title case to output (modification 2) ---
    // Returns names in Title Case (e.g. "alice" → "Alice") for human-readable,
    // professionally formatted output. Applied after deduplication and sorting
    // to avoid re-introducing case inconsistency into the Set operation.
    return unique.map(toTitleCase);
}


// --- Script entry point ---
// Equivalent to Python's if __name__ == "__main__" pattern.
// Node.js does not have a native guard, but this module.exports pattern
// allows the function to be imported cleanly in test files without side effects.
if (require.main === module) {
    const testNames = ["Alice", "bob", "alice", "Charlie", "BOB", "dave", "Eve", "charlie"];

    console.log("Original list:", testNames);
    console.log("Processed list:", processNames(testNames));
}

module.exports = { processNames };
