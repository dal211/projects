// Pinyin validation utilities
// Handles tone marks, syllable comparison, and detailed feedback

// Tone mark mappings
const TONE_MAP = {
    'ā': { base: 'a', tone: 1 }, 'á': { base: 'a', tone: 2 }, 'ǎ': { base: 'a', tone: 3 }, 'à': { base: 'a', tone: 4 },
    'ē': { base: 'e', tone: 1 }, 'é': { base: 'e', tone: 2 }, 'ě': { base: 'e', tone: 3 }, 'è': { base: 'e', tone: 4 },
    'ī': { base: 'i', tone: 1 }, 'í': { base: 'i', tone: 2 }, 'ǐ': { base: 'i', tone: 3 }, 'ì': { base: 'i', tone: 4 },
    'ō': { base: 'o', tone: 1 }, 'ó': { base: 'o', tone: 2 }, 'ǒ': { base: 'o', tone: 3 }, 'ò': { base: 'o', tone: 4 },
    'ū': { base: 'u', tone: 1 }, 'ú': { base: 'u', tone: 2 }, 'ǔ': { base: 'u', tone: 3 }, 'ù': { base: 'u', tone: 4 },
    'ǖ': { base: 'ü', tone: 1 }, 'ǘ': { base: 'ü', tone: 2 }, 'ǚ': { base: 'ü', tone: 3 }, 'ǜ': { base: 'ü', tone: 4 },
};

const TONE_NAMES = {
    1: '1st tone (flat, high)',
    2: '2nd tone (rising)',
    3: '3rd tone (dipping)',
    4: '4th tone (falling)',
    5: '5th tone (neutral/light)',
};

/**
 * Strip tone marks from a pinyin string, returning base syllable
 */
export function stripTones(pinyin) {
    let result = pinyin.toLowerCase();
    for (const [marked, { base }] of Object.entries(TONE_MAP)) {
        result = result.replaceAll(marked, base);
    }
    return result;
}

/**
 * Get the tone number from a pinyin syllable
 * Returns 5 for neutral tone (no mark)
 */
export function getTone(syllable) {
    for (const char of syllable) {
        if (TONE_MAP[char]) {
            return TONE_MAP[char].tone;
        }
    }
    return 5; // neutral
}

/**
 * Check if a pinyin string has any tone marks
 */
export function hasToneMarks(pinyin) {
    for (const char of pinyin) {
        if (TONE_MAP[char]) return true;
    }
    return false;
}

/**
 * Normalize pinyin for comparison
 * - lowercase
 * - trim whitespace
 * - normalize multiple spaces
 * - remove punctuation
 */
export function normalizePinyin(pinyin) {
    return pinyin
        .toLowerCase()
        .trim()
        .replace(/[.,!?，。！？、：；]/g, '')
        .replace(/\s+/g, ' ');
}

/**
 * Split pinyin string into individual syllable tokens
 */
export function splitSyllables(pinyin) {
    return normalizePinyin(pinyin).split(' ').filter(s => s.length > 0);
}

/**
 * Compare two pinyin syllables
 * @returns {{ match: 'correct'|'partial'|'incorrect', feedback: string, toneMismatch?: boolean }}
 */
export function compareSyllables(input, expected) {
    const inputNorm = normalizePinyin(input);
    const expectedNorm = normalizePinyin(expected);

    // Exact match
    if (inputNorm === expectedNorm) {
        return { match: 'correct', feedback: 'Perfect!' };
    }

    // Same base syllable, different tone - NOW COUNTED AS CORRECT
    const inputBase = stripTones(inputNorm);
    const expectedBase = stripTones(expectedNorm);

    if (inputBase === expectedBase) {
        const inputTone = getTone(inputNorm);
        const expectedTone = getTone(expectedNorm);

        if (inputTone === 5 && expectedTone !== 5) {
            // Missing tone mark
            return {
                match: 'correct',
                toneMismatch: true,
                feedback: `Correct syllable! Note: "${expected}" uses the ${TONE_NAMES[expectedTone]}.`
            };
        }

        return {
            match: 'correct',
            toneMismatch: true,
            feedback: `Correct syllable! Note: "${expected}" uses the ${TONE_NAMES[expectedTone]}.`
        };
    }

    // Completely wrong
    return {
        match: 'incorrect',
        feedback: `Incorrect. The correct answer is "${expected}".`
    };
}

/**
 * Validate a full pinyin answer against expected
 * @returns {{ result: 'correct'|'partial'|'incorrect', details: array, feedback: string }}
 */
export function validatePinyin(input, expected) {
    const inputSyllables = splitSyllables(input);
    const expectedSyllables = splitSyllables(expected);

    if (inputSyllables.length === 0) {
        return {
            result: 'incorrect',
            details: [],
            feedback: 'Please enter your answer in pinyin.'
        };
    }

    // If different number of syllables, try best-effort matching
    const maxLen = Math.max(inputSyllables.length, expectedSyllables.length);
    const details = [];
    let correctCount = 0; // strict correct
    let toneMismatchCount = 0; // correct but wrong tone
    let partialCount = 0;

    for (let i = 0; i < maxLen; i++) {
        if (i >= inputSyllables.length) {
            details.push({ expected: expectedSyllables[i], input: '', match: 'incorrect', feedback: 'Missing syllable' });
        } else if (i >= expectedSyllables.length) {
            details.push({ expected: '', input: inputSyllables[i], match: 'incorrect', feedback: 'Extra syllable' });
        } else {
            const comparison = compareSyllables(inputSyllables[i], expectedSyllables[i]);
            details.push({
                expected: expectedSyllables[i],
                input: inputSyllables[i],
                ...comparison
            });
            if (comparison.match === 'correct') {
                correctCount++;
                if (comparison.toneMismatch) toneMismatchCount++;
            } else if (comparison.match === 'partial') {
                partialCount++;
            }
        }
    }

    let result;
    if (correctCount === expectedSyllables.length) {
        // All correct (possibly with tone mismatches)
        result = 'correct';
    } else if (correctCount + partialCount > 0 && correctCount + partialCount >= expectedSyllables.length * 0.5) {
        result = 'partial';
    } else {
        result = 'incorrect';
    }

    // Collect feedback from incorrect items AND correct items with tone mismatches
    const feedback = details
        .filter(d => d.match !== 'correct' || d.toneMismatch)
        .map(d => d.feedback)
        .filter(f => f !== 'Perfect!') // Don't show generic perfect
        .join(' ');

    return { result, details, feedback: feedback || 'Perfect!' };
}

/**
 * Available tone-marked vowels for the input helper
 */
export const TONE_VOWELS = [
    ['ā', 'á', 'ǎ', 'à'],
    ['ē', 'é', 'ě', 'è'],
    ['ī', 'í', 'ǐ', 'ì'],
    ['ō', 'ó', 'ǒ', 'ò'],
    ['ū', 'ú', 'ǔ', 'ù'],
    ['ǖ', 'ǘ', 'ǚ', 'ǜ'],
];
