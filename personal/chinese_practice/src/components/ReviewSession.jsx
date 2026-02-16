import React, { useState, useMemo } from 'react';
import QuizCard from './QuizCard.jsx';
import { getDueItems, calculateNextReview, qualityFromResult, getMastery } from '../engine/srs.js';
import { getVocabById } from '../data/hsk1-vocab.js';

export default function ReviewSession({ progress, updateProgress, onFinish, onBack, audioReady }) {
    const dueItems = useMemo(() => getDueItems(progress.srsData || {}), []);
    const [currentIndex, setCurrentIndex] = useState(0);
    const [scores, setScores] = useState({ correct: 0, partial: 0, incorrect: 0, total: 0 });

    if (dueItems.length === 0) {
        return (
            <div className="animate-in text-center" style={{ paddingTop: 'var(--space-3xl)' }}>
                <div style={{ fontSize: '4rem', marginBottom: 'var(--space-md)' }}>✅</div>
                <h2 style={{ fontSize: '1.5rem', marginBottom: 'var(--space-sm)' }}>All caught up!</h2>
                <p className="text-muted mb-lg">No words due for review right now. Come back later!</p>
                <button className="btn btn-primary" onClick={onFinish}>Back to Dashboard</button>
            </div>
        );
    }

    if (currentIndex >= dueItems.length) {
        // Review complete
        const { correct, partial, incorrect, total } = scores;
        const accuracy = total > 0 ? Math.round((correct / total) * 100) : 0;

        return (
            <div className="animate-in text-center" style={{ paddingTop: 'var(--space-xl)' }}>
                <div style={{ fontSize: '3rem', marginBottom: 'var(--space-md)' }}>
                    {accuracy >= 80 ? '🎉' : accuracy >= 50 ? '💪' : '📚'}
                </div>
                <h2 style={{ fontSize: '1.5rem', marginBottom: 'var(--space-sm)' }}>Review Complete</h2>
                <p className="text-muted mb-lg">
                    {accuracy >= 80 ? 'Great memory!' : 'Keep reviewing to build retention.'}
                </p>

                <div className="score-grid">
                    <div className="score-item">
                        <div className="score-value correct">{correct}</div>
                        <div className="score-label">Correct</div>
                    </div>
                    <div className="score-item">
                        <div className="score-value partial">{partial}</div>
                        <div className="score-label">Partial</div>
                    </div>
                    <div className="score-item">
                        <div className="score-value incorrect">{incorrect}</div>
                        <div className="score-label">Incorrect</div>
                    </div>
                </div>

                <button className="btn btn-primary mt-xl" onClick={onFinish}>Back to Dashboard</button>
            </div>
        );
    }

    const currentItem = dueItems[currentIndex];
    const word = getVocabById(currentItem.vocabId);

    if (!word) {
        // Skip unknown items
        setCurrentIndex(i => i + 1);
        return null;
    }

    const handleResult = (result) => {
        setScores(prev => ({
            correct: prev.correct + (result === 'correct' ? 1 : 0),
            partial: prev.partial + (result === 'partial' ? 1 : 0),
            incorrect: prev.incorrect + (result === 'incorrect' ? 1 : 0),
            total: prev.total + 1,
        }));

        // Update SRS
        updateProgress(prev => {
            const srsData = { ...prev.srsData };
            const quality = qualityFromResult(result);
            srsData[currentItem.vocabId] = calculateNextReview(srsData[currentItem.vocabId], quality);
            return { ...prev, srsData };
        });
    };

    return (
        <div className="animate-in" key={currentIndex}>
            <button className="back-button" onClick={onBack}>← Back to Dashboard</button>

            <div className="progress-container">
                <div className="progress-header">
                    <span>Review — {currentIndex + 1} of {dueItems.length}</span>
                    <span>{Math.round(((currentIndex) / dueItems.length) * 100)}%</span>
                </div>
                <div className="progress-bar">
                    <div className="progress-fill" style={{ width: `${(currentIndex / dueItems.length) * 100}%` }} />
                </div>
            </div>

            <QuizCard
                word={word}
                audioReady={audioReady}
                onResult={handleResult}
                onNext={() => setCurrentIndex(i => i + 1)}
            />
        </div>
    );
}
