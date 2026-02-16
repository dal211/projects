import React, { useState, useMemo } from 'react';
import PinyinInput from './PinyinInput.jsx';
import { splitSyllables, compareSyllables } from '../engine/pinyin.js';
import { speakChinese, isAudioAvailable } from '../engine/audio.js';

export default function FillBlankExercise({ sentence, audioReady, onResult, onNext }) {
    const syllables = useMemo(() => splitSyllables(sentence.pinyin), [sentence.pinyin]);

    // Choose 1-2 random syllables to blank out
    const blankedIndices = useMemo(() => {
        if (syllables.length <= 2) return [0];
        const count = syllables.length > 4 ? 2 : 1;
        const indices = [];
        const available = syllables.map((_, i) => i);
        for (let n = 0; n < count; n++) {
            const pick = Math.floor(Math.random() * available.length);
            indices.push(available[pick]);
            available.splice(pick, 1);
        }
        return indices.sort((a, b) => a - b);
    }, [sentence.pinyin]);

    const [answers, setAnswers] = useState(blankedIndices.map(() => ''));
    const [submitted, setSubmitted] = useState(false);
    const [results, setResults] = useState([]);

    const canSpeak = audioReady && isAudioAvailable();

    const handleSubmit = () => {
        const res = blankedIndices.map((idx, i) => {
            return compareSyllables(answers[i], syllables[idx]);
        });
        setResults(res);
        setSubmitted(true);

        const allCorrect = res.every(r => r.match === 'correct');
        const anyPartial = res.some(r => r.match === 'partial');
        const overallResult = allCorrect ? 'correct' : anyPartial ? 'partial' : 'incorrect';
        onResult(overallResult);
    };

    const updateAnswer = (index, value) => {
        setAnswers(prev => {
            const next = [...prev];
            next[index] = value;
            return next;
        });
    };

    let blankCounter = 0;

    return (
        <div className="card" style={{ margin: 'var(--space-lg) 0' }}>
            <div className="text-center mb-lg">
                <div style={{
                    fontSize: '0.8rem',
                    color: 'var(--color-text-muted)',
                    textTransform: 'uppercase',
                    letterSpacing: '0.1em',
                    marginBottom: 'var(--space-sm)',
                }}>
                    Fill in the missing pinyin
                </div>
                <div style={{ fontSize: '1.2rem', fontWeight: 500, lineHeight: 1.5, marginBottom: 'var(--space-sm)' }}>
                    {sentence.english}
                </div>
                <div className="chinese-character" style={{ fontSize: '2rem' }}>{sentence.character}</div>
            </div>

            {/* Sentence with blanks */}
            <div className="fill-sentence" style={{ marginBottom: 'var(--space-lg)' }}>
                {syllables.map((syl, i) => {
                    if (blankedIndices.includes(i)) {
                        const answerIdx = blankedIndices.indexOf(i);
                        const showResult = submitted && results[answerIdx];
                        return (
                            <span key={i}>
                                {submitted ? (
                                    <span className={`fill-blank`} style={{
                                        color: showResult.match === 'correct' ? 'var(--color-correct)' :
                                            showResult.match === 'partial' ? 'var(--color-partial)' :
                                                'var(--color-incorrect)',
                                        borderColor: showResult.match === 'correct' ? 'var(--color-correct)' :
                                            showResult.match === 'partial' ? 'var(--color-partial)' :
                                                'var(--color-incorrect)',
                                    }}>
                                        {answers[answerIdx] || '—'} {showResult.match !== 'correct' && `(${syl})`}
                                    </span>
                                ) : (
                                    <input
                                        className="fill-input"
                                        value={answers[answerIdx]}
                                        onChange={(e) => updateAnswer(answerIdx, e.target.value)}
                                        placeholder="?"
                                        autoComplete="off"
                                        autoCorrect="off"
                                        spellCheck="false"
                                        style={{ width: `${Math.max(60, syl.length * 18)}px` }}
                                    />
                                )}
                                {' '}
                            </span>
                        );
                    }
                    return <span key={i}>{syl} </span>;
                })}
            </div>

            {/* Tone helper for fill exercises */}
            {!submitted && (
                <div style={{ textAlign: 'center', fontSize: '0.75rem', color: 'var(--color-text-muted)', marginBottom: 'var(--space-sm)' }}>
                    Tip: Use the tone buttons in a vocab quiz, or type tone marks directly
                </div>
            )}

            {!submitted && (
                <div className="text-center mt-md">
                    <button
                        className="btn btn-primary"
                        onClick={handleSubmit}
                        disabled={answers.some(a => !a.trim())}
                    >
                        Check Answer
                    </button>
                </div>
            )}

            {submitted && (
                <div className="animate-in">
                    <div className={`feedback feedback-${results.every(r => r.match === 'correct') ? 'correct' : results.some(r => r.match === 'partial') ? 'partial' : 'incorrect'}`}>
                        {results.every(r => r.match === 'correct' && !r.toneMismatch)
                            ? '✅ Perfect!'
                            : results.map((r, i) => `${r.match === 'correct' ? '✓' : '✗'} ${r.feedback}`).join(' ')}
                    </div>

                    <div className="text-center mt-sm">
                        <div className="chinese-pinyin">{sentence.pinyin}</div>
                    </div>

                    {canSpeak && (
                        <div className="text-center mt-sm">
                            <button className="btn-icon" onClick={() => speakChinese(sentence.character)} title="Listen">
                                🔊
                            </button>
                        </div>
                    )}

                    <div className="text-center mt-lg">
                        <button className="btn btn-primary" onClick={onNext}>Continue →</button>
                    </div>
                </div>
            )}
        </div>
    );
}
