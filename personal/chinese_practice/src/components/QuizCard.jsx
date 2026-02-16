import React, { useState } from 'react';
import PinyinInput from './PinyinInput.jsx';
import { validatePinyin } from '../engine/pinyin.js';
import { speakChinese, isAudioAvailable } from '../engine/audio.js';

export default function QuizCard({ word, audioReady, isSentence, onResult, onNext }) {
    const [userInput, setUserInput] = useState('');
    const [submitted, setSubmitted] = useState(false);
    const [result, setResult] = useState(null);
    const [validation, setValidation] = useState(null);

    const canSpeak = audioReady && isAudioAvailable();

    const handleSubmit = () => {
        if (!userInput.trim() || submitted) return;

        const v = validatePinyin(userInput, word.pinyin);
        setValidation(v);
        setResult(v.result);
        setSubmitted(true);
        onResult(v.result);
    };

    const handleNext = () => {
        setUserInput('');
        setSubmitted(false);
        setResult(null);
        setValidation(null);
        onNext();
    };

    return (
        <div className="card" style={{ margin: 'var(--space-lg) 0' }}>
            {/* Prompt */}
            <div className="text-center mb-lg">
                <div style={{
                    fontSize: '0.8rem',
                    color: 'var(--color-text-muted)',
                    textTransform: 'uppercase',
                    letterSpacing: '0.1em',
                    marginBottom: 'var(--space-sm)',
                }}>
                    {isSentence ? 'Translate to Pinyin' : 'Type the Pinyin'}
                </div>
                <div style={{
                    fontSize: isSentence ? '1.3rem' : '1.5rem',
                    fontWeight: 500,
                    color: 'var(--color-text)',
                    lineHeight: 1.5,
                }}>
                    {word.english}
                </div>
            </div>

            {/* Input */}
            <PinyinInput
                value={userInput}
                onChange={setUserInput}
                onSubmit={handleSubmit}
                status={result}
                disabled={submitted}
                placeholder={isSentence ? 'Type the full sentence in pinyin...' : 'Type pinyin with tones...'}
            />

            {/* Submit button */}
            {!submitted && (
                <div className="text-center mt-md">
                    <button
                        className="btn btn-primary"
                        onClick={handleSubmit}
                        disabled={!userInput.trim()}
                    >
                        Check Answer
                    </button>
                </div>
            )}

            {/* Feedback */}
            {submitted && validation && (
                <div className="animate-in">
                    <div className={`feedback feedback-${result}`}>
                        {result === 'correct' && '✅ '}
                        {result === 'partial' && '⚠️ '}
                        {result === 'incorrect' && '❌ '}
                        {validation.feedback}
                    </div>

                    {/* Show correct answer */}
                    <div className="chinese-display" style={{ padding: 'var(--space-md) 0' }}>
                        <div className="chinese-character">{word.character}</div>
                        <div className="chinese-pinyin">{word.pinyin}</div>
                        {(!canSpeak && word.phonetic) && (
                            <div className="chinese-phonetic">🔤 "{word.phonetic}"</div>
                        )}
                    </div>

                    {canSpeak && (
                        <div className="text-center mb-md">
                            <button className="btn-icon" onClick={() => speakChinese(word.character, word.phonetic)} title="Listen">
                                🔊
                            </button>
                        </div>
                    )}

                    {/* Per-syllable breakdown for partial/incorrect OR tone mismatch */}
                    {(result !== 'correct' || (validation.details && validation.details.some(d => d.toneMismatch))) && validation.details && validation.details.length > 0 && (
                        <div style={{
                            background: 'var(--color-bg-elevated)',
                            borderRadius: 'var(--radius-sm)',
                            padding: 'var(--space-md)',
                            marginTop: 'var(--space-sm)',
                        }}>
                            <div style={{ fontSize: '0.8rem', color: 'var(--color-text-muted)', marginBottom: 'var(--space-sm)' }}>
                                Syllable breakdown:
                            </div>
                            {validation.details.map((d, i) => (
                                <div key={i} style={{
                                    display: 'flex',
                                    gap: 'var(--space-md)',
                                    padding: 'var(--space-xs) 0',
                                    fontSize: '0.9rem',
                                }}>
                                    <span style={{
                                        color: d.match === 'correct' ? 'var(--color-correct)' :
                                            d.match === 'partial' ? 'var(--color-partial)' :
                                                'var(--color-incorrect)',
                                        minWidth: '20px',
                                    }}>
                                        {d.match === 'correct' ? '✓' : d.match === 'partial' ? '~' : '✗'}
                                    </span>
                                    <span style={{ color: 'var(--color-text-muted)', minWidth: '60px' }}>
                                        {d.input || '—'}
                                    </span>
                                    <span style={{ color: 'var(--color-text-secondary)' }}>→</span>
                                    <span style={{ color: 'var(--color-correct)' }}>
                                        {d.expected || '—'}
                                    </span>
                                    {d.toneMismatch && (
                                        <span style={{ color: 'var(--color-partial)', fontSize: '0.8rem', marginLeft: 'auto' }}>
                                            (Check tone)
                                        </span>
                                    )}
                                </div>
                            ))}
                        </div>
                    )}

                    <div className="text-center mt-lg">
                        <button className="btn btn-primary" onClick={handleNext}>
                            Continue →
                        </button>
                    </div>
                </div>
            )}
        </div>
    );
}
