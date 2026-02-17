import hsk1Vocab, { getVocabByLesson as getHsk1Vocab } from './hsk1-vocab.js';
import hsk1Sentences, { getSentencesByLesson as getHsk1Sentences } from './hsk1-sentences.js';

import hsk2Vocab, { getVocabByLesson as getHsk2Vocab } from './hsk2-vocab.js';
import hsk2Sentences, { getSentencesByLesson as getHsk2Sentences } from './hsk2-sentences.js';

import hsk3Vocab, { getVocabByLesson as getHsk3Vocab } from './hsk3-vocab.js';
import hsk3Sentences, { getSentencesByLesson as getHsk3Sentences } from './hsk3-sentences.js';

import hsk4Vocab, { getVocabByLesson as getHsk4Vocab } from './hsk4-vocab.js';
import hsk4Sentences, { getSentencesByLesson as getHsk4Sentences } from './hsk4-sentences.js';

import hsk5Vocab, { getVocabByLesson as getHsk5Vocab } from './hsk5-vocab.js';
import hsk5Sentences, { getSentencesByLesson as getHsk5Sentences } from './hsk5-sentences.js';

import hsk6Vocab, { getVocabByLesson as getHsk6Vocab } from './hsk6-vocab.js';
import hsk6Sentences, { getSentencesByLesson as getHsk6Sentences } from './hsk6-sentences.js';

const DATA = {
    1: { vocab: hsk1Vocab, sentences: hsk1Sentences, getVocabByLesson: getHsk1Vocab, getSentencesByLesson: getHsk1Sentences },
    2: { vocab: hsk2Vocab, sentences: hsk2Sentences, getVocabByLesson: getHsk2Vocab, getSentencesByLesson: getHsk2Sentences },
    3: { vocab: hsk3Vocab, sentences: hsk3Sentences, getVocabByLesson: getHsk3Vocab, getSentencesByLesson: getHsk3Sentences },
    4: { vocab: hsk4Vocab, sentences: hsk4Sentences, getVocabByLesson: getHsk4Vocab, getSentencesByLesson: getHsk4Sentences },
    5: { vocab: hsk5Vocab, sentences: hsk5Sentences, getVocabByLesson: getHsk5Vocab, getSentencesByLesson: getHsk5Sentences },
    6: { vocab: hsk6Vocab, sentences: hsk6Sentences, getVocabByLesson: getHsk6Vocab, getSentencesByLesson: getHsk6Sentences },
};

export function getLevelData(level) {
    const data = DATA[level];
    if (!data) return { totalLessons: 0, getVocabByLesson: () => [], getSentencesByLesson: () => [] };

    // Calculate total lessons based on the vocab file
    // Assumes vocab items have a 'lesson' property
    const totalLessons = data.vocab.reduce((max, item) => Math.max(max, item.lesson), 0);

    return {
        ...data,
        totalLessons
    };
}

export function getAllLevels() {
    return Object.keys(DATA).map(Number);
}
