// localStorage persistence layer
// All progress is stored as JSON under a single key

const STORAGE_KEY = 'chinese_practice_progress';

const INITIAL_STATE = {
    level: 1,
    levels: {
        1: {
            currentLesson: 1,
            completedLessons: [],
            srsData: {},       // vocabId -> SRS item
            lessonScores: {},   // lessonNum -> { correct, partial, incorrect, total }
        },
        2: {
            currentLesson: 1,
            completedLessons: [],
            srsData: {},
            lessonScores: {},
        }
    },
    settings: {
        audioEnabled: true,
    },
    lastActive: null,
};

/**
 * Load progress from localStorage
 * @returns {object} saved state or default
 */
export function loadProgress() {
    try {
        const json = localStorage.getItem(STORAGE_KEY);
        if (!json) return JSON.parse(JSON.stringify(INITIAL_STATE));

        const data = JSON.parse(json);

        // Migration: If old schema (no 'levels' object), migrate to level 1
        if (!data.levels) {
            console.log('Migrating legacy data to HSK Level 1...');
            return {
                level: 1,
                levels: {
                    ...INITIAL_STATE.levels,
                    1: {
                        completedLessons: data.completedLessons || [],
                        lessonScores: data.lessonScores || {},
                        currentLesson: data.currentLesson || 1,
                        srsData: data.srsData || {},
                    }
                },
                lastPlayed: data.lastPlayed || new Date().toISOString(),
            };
        }

        // Ensure all levels exist (for new levels added later)
        const levels = { ...INITIAL_STATE.levels, ...data.levels };
        return { ...data, levels };
    } catch (e) {
        console.error('Failed to load progress', e);
        return JSON.parse(JSON.stringify(INITIAL_STATE));
    }
}

/**
 * Save progress to localStorage
 */
export function saveProgress(progress) {
    try {
        const toSave = {
            ...progress,
            lastPlayed: new Date().toISOString(),
        };
        localStorage.setItem(STORAGE_KEY, JSON.stringify(toSave));
    } catch (e) {
        console.error('Failed to save progress', e);
    }
}

/**
 * Reset all progress
 */
export function resetProgress() {
    localStorage.removeItem(STORAGE_KEY);
}

/**
 * Export progress as JSON file
 */
export function exportProgress() {
    const data = loadProgress();
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `chinese-practice-hsk${data.level}-${new Date().toISOString().slice(0, 10)}.json`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
}

/**
 * Import progress from JSON file
 */
export function importProgress(file) {
    return new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.onload = (e) => {
            try {
                const data = JSON.parse(e.target.result);
                // Basic validation
                if (!data.levels && !data.completedLessons) throw new Error('Invalid format');

                // If importing legacy, wrap it
                const normalized = data.levels ? data : {
                    level: 1,
                    levels: {
                        ...INITIAL_STATE.levels,
                        1: { ...data }
                    }
                };

                saveProgress(normalized);
                resolve(normalized);
            } catch (err) {
                reject(err);
            }
        };
        reader.readAsText(file);
    });
}
