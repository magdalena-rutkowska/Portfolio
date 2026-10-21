// cleanNames.test.js
// Tests for cleanNames.js — covers core behaviour, edge cases, and validation

const { processNames } = require('../src/cleanNames');


describe('processNames', () => {

    test('removes case-insensitive duplicates', () => {
        // Core fix: 'alice', 'Alice', 'ALICE' should produce one entry.
        expect(processNames(['Alice', 'alice', 'ALICE'])).toEqual(['Alice']);
    });

    test('returns sorted title case output', () => {
        // Output should be sorted alphabetically in Title Case.
        expect(processNames(['Charlie', 'alice', 'BOB'])).toEqual(['Alice', 'Bob', 'Charlie']);
    });

    test('processes original tutor test data correctly', () => {
        // The original test data should now return 5 unique names, not 8.
        const names = ['Alice', 'bob', 'alice', 'Charlie', 'BOB', 'dave', 'Eve', 'charlie'];
        expect(processNames(names)).toEqual(['Alice', 'Bob', 'Charlie', 'Dave', 'Eve']);
    });

    test('throws TypeError for non-array input', () => {
        // Should throw if input is not an array.
        expect(() => processNames('Alice')).toThrow(TypeError);
    });

    test('throws TypeError for null input', () => {
        expect(() => processNames(null)).toThrow(TypeError);
    });

    test('throws Error for empty array', () => {
        // Should throw if list is empty.
        expect(() => processNames([])).toThrow(Error);
    });

    test('throws TypeError for non-string elements', () => {
        // Should throw if any element is not a string.
        expect(() => processNames(['Alice', 42, 'Bob'])).toThrow(TypeError);
    });

});
