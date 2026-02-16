// SM-2 Spaced Repetition Algorithm
// Adapted for vocabulary learning with partial credit support

const DEFAULT_EASE_FACTOR = 2.5;
const MIN_EASE_FACTOR = 1.3;

/**
 * Create initial SRS state for a vocabulary item
 */
export function createSRSItem(vocabId) {
    return {
        vocabId,
        easeFactor: DEFAULT_EASE_FACTOR,
        interval: 0,       // days
        repetitions: 0,
        nextReview: Date.now(),
        lastReview: null,
        correctCount: 0,
        totalCount: 0,
    };
}

/**
 * Calculate quality score from quiz result
 * @param {string} result - 'correct', 'partial', 'incorrect'
 * @returns {number} quality 0-5
 */
export function qualityFromResult(result) {
    switch (result) {
        case 'correct': return 5;
        case 'partial': return 3;
        case 'incorrect': return 1;
        default: return 1;
    }
}

/**
 * Update SRS item after a review
 * @param {object} item - current SRS state
 * @param {number} quality - 0 to 5
 * @returns {object} updated SRS item
 */
export function calculateNextReview(item, quality) {
    const now = Date.now();
    const DAY_MS = 24 * 60 * 60 * 1000;

    let { easeFactor, interval, repetitions, correctCount, totalCount } = item;

    totalCount += 1;
    if (quality >= 4) correctCount += 1;

    if (quality >= 3) {
        // Correct or partially correct
        if (repetitions === 0) {
            interval = 1;
        } else if (repetitions === 1) {
            interval = 3;
        } else {
            interval = Math.round(interval * easeFactor);
        }
        repetitions += 1;
    } else {
        // Incorrect — reset
        repetitions = 0;
        interval = 1;
    }

    // Update ease factor (SM-2 formula)
    easeFactor = easeFactor + (0.1 - (5 - quality) * (0.08 + (5 - quality) * 0.02));
    if (easeFactor < MIN_EASE_FACTOR) easeFactor = MIN_EASE_FACTOR;

    return {
        ...item,
        easeFactor,
        interval,
        repetitions,
        nextReview: now + interval * DAY_MS,
        lastReview: now,
        correctCount,
        totalCount,
    };
}

/**
 * Get items due for review
 * @param {object} srsData - map of vocabId -> SRS item
 * @returns {array} items due for review, sorted by urgency
 */
export function getDueItems(srsData) {
    const now = Date.now();
    return Object.values(srsData)
        .filter(item => item.nextReview <= now)
        .sort((a, b) => a.nextReview - b.nextReview);
}

/**
 * Get mastery percentage for an item
 * @param {object} item - SRS item
 * @returns {number} 0-100
 */
export function getMastery(item) {
    if (!item || item.totalCount === 0) return 0;
    const accuracy = item.correctCount / item.totalCount;
    const repetitionBonus = Math.min(item.repetitions / 5, 1);
    return Math.round((accuracy * 0.6 + repetitionBonus * 0.4) * 100);
}

/**
 * Get accuracy percentage for an item
 */
export function getAccuracy(item) {
    if (!item || item.totalCount === 0) return 0;
    return Math.round((item.correctCount / item.totalCount) * 100);
}

/**
 * Prioritize items for difficulty adaptation
 * Items with lower accuracy appear more frequently
 */
export function getAdaptivePriority(srsData) {
    return Object.values(srsData)
        .filter(item => item.totalCount > 0)
        .sort((a, b) => {
            const accA = a.correctCount / a.totalCount;
            const accB = b.correctCount / b.totalCount;
            return accA - accB; // lowest accuracy first
        });
}
