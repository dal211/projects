import React, { useState, useMemo } from 'react';
import { splitSyllables } from '../engine/pinyin.js';

export default function ScrambleExercise({ sentence, onResult, onNext }) {
    const correctOrder = useMemo(() => splitSyllables(sentence.pinyin), [sentence.pinyin]);
    const scrambled = useMemo(() => {
        const arr = [...correctOrder];
        // Fisher-Yates shuffle
        for (let i = arr.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [arr[i], arr[j]] = [arr[j], arr[i]];
        }
        // Avoid producing the correct order by chance
        if (arr.join(' ') === correctOrder.join(' ') && arr.length > 1) {
            [arr[0], arr[1]] = [arr[1], arr[0]];
        }
        return arr.map((word, i) => ({ word, originalIndex: i }));
    }, [sentence.pinyin]);

    const [placed, setPlaced] = useState([]);
    const [remaining, setRemaining] = useState(scrambled);
    const [submitted, setSubmitted] = useState(false);
    const [result, setResult] = useState(null);

    const handleTileClick = (tile, index) => {
        if (submitted) return;
        setPlaced(prev => [...prev, tile]);
        setRemaining(prev => prev.filter((_, i) => i !== index));
    };

    const handlePlacedClick = (tile, index) => {
        if (submitted) return;
        setRemaining(prev => [...prev, tile]);
        setPlaced(prev => prev.filter((_, i) => i !== index));
    };

    const handleSubmit = () => {
        const userOrder = placed.map(t => t.word);
        const isCorrect = userOrder.join(' ') === correctOrder.join(' ');
        const res = isCorrect ? 'correct' : 'incorrect';
        setResult(res);
        setSubmitted(true);
        onResult(res);
    };

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
                    Arrange the pinyin in correct order
                </div>
                <div style={{ fontSize: '1.2rem', fontWeight: 500, lineHeight: 1.5 }}>
                    {sentence.english}
                </div>
            </div>

            {/* Drop zone — placed words */}
            <div className="scramble-zone drop-target" style={{ minHeight: '56px' }}>
                {placed.length === 0 && (
                    <span style={{ color: 'var(--color-text-muted)', fontSize: '0.85rem' }}>
                        Tap words below to build the sentence
                    </span>
                )}
                {placed.map((tile, i) => (
                    <span
                        key={`p-${i}`}
                        className={`word-tile placed ${submitted ? (tile.word === correctOrder[i] ? 'correct-tile' : 'incorrect-tile') : ''}`}
                        onClick={() => handlePlacedClick(tile, i)}
                    >
                        {tile.word}
                    </span>
                ))}
            </div>

            {/* Available tiles */}
            <div className="scramble-zone" style={{ borderColor: 'transparent' }}>
                {remaining.map((tile, i) => (
                    <span
                        key={`r-${i}`}
                        className="word-tile"
                        onClick={() => handleTileClick(tile, i)}
                    >
                        {tile.word}
                    </span>
                ))}
            </div>

            {!submitted && (
                <div className="text-center mt-md">
                    <button
                        className="btn btn-primary"
                        onClick={handleSubmit}
                        disabled={placed.length !== correctOrder.length}
                    >
                        Check Order
                    </button>
                </div>
            )}

            {submitted && (
                <div className="animate-in">
                    <div className={`feedback feedback-${result}`}>
                        {result === 'correct'
                            ? '✅ Perfect order!'
                            : `❌ Not quite. Correct order: ${correctOrder.join(' ')}`}
                    </div>

                    <div className="chinese-display" style={{ padding: 'var(--space-sm) 0' }}>
                        <div className="chinese-character" style={{ fontSize: '2rem' }}>{sentence.character}</div>
                        <div className="chinese-pinyin">{sentence.pinyin}</div>
                    </div>

                    <div className="text-center mt-md">
                        <button className="btn btn-primary" onClick={onNext}>Continue →</button>
                    </div>
                </div>
            )}
        </div>
    );
}
