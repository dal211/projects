import React from 'react';
import { getMastery } from '../engine/srs.js';

export default function LessonSummary({ lessonNum, scores, wordResults, vocab, srsData, onFinish }) {
    const { correct, partial, incorrect, total } = scores;
    const accuracy = total > 0 ? Math.round((correct / total) * 100) : 0;

    return (
        <div className="animate-in">
            <div className="text-center mb-lg">
                <div style={{ fontSize: '3rem', marginBottom: 'var(--space-sm)' }}>
                    {accuracy >= 80 ? '🎉' : accuracy >= 50 ? '💪' : '📚'}
                </div>
                <h2 style={{ fontSize: '1.5rem', fontWeight: 600 }}>Lesson {lessonNum} Complete</h2>
                <p className="text-muted mt-sm">
                    {accuracy >= 80 ? 'Excellent work!' : accuracy >= 50 ? 'Good effort! Keep practicing.' : 'Review this lesson again soon.'}
                </p>
            </div>

            {/* Score cards */}
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

            {/* Accuracy bar */}
            <div className="progress-container">
                <div className="progress-header">
                    <span>Accuracy</span>
                    <span>{accuracy}%</span>
                </div>
                <div className="progress-bar">
                    <div
                        className="progress-fill mastery"
                        style={{ width: `${accuracy}%` }}
                    />
                </div>
            </div>

            {/* Per-word mastery */}
            <h3 style={{
                fontSize: '0.9rem',
                color: 'var(--color-text-secondary)',
                marginTop: 'var(--space-xl)',
                marginBottom: 'var(--space-sm)',
            }}>
                Word Mastery
            </h3>
            <div className="mastery-list">
                {vocab.map(word => {
                    const srs = srsData[word.id];
                    const mastery = getMastery(srs);
                    const result = wordResults[word.id];
                    const color = mastery >= 80 ? 'var(--color-correct)' :
                        mastery >= 50 ? 'var(--color-partial)' :
                            mastery > 0 ? 'var(--color-incorrect)' :
                                'var(--color-text-muted)';

                    return (
                        <div key={word.id} className="mastery-item">
                            <span className="mastery-word">{word.character}</span>
                            <span className="mastery-pinyin">{word.pinyin}</span>
                            <div className="mastery-bar-container">
                                <div
                                    className="mastery-bar-fill"
                                    style={{ width: `${mastery}%`, background: color }}
                                />
                            </div>
                            <span className="mastery-percent">{mastery}%</span>
                        </div>
                    );
                })}
            </div>

            <div className="nav-row" style={{ marginTop: 'var(--space-2xl)' }}>
                <button className="btn btn-primary btn-block" onClick={onFinish}>
                    Back to Dashboard
                </button>
            </div>
        </div>
    );
}
