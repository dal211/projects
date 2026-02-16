import React, { useState, useEffect, useCallback } from 'react';
import { getVocabByLesson } from '../data/hsk1-vocab.js';
import { getSentencesByLesson } from '../data/hsk1-sentences.js';
import { createSRSItem, calculateNextReview, qualityFromResult, getMastery } from '../engine/srs.js';
import VocabCard from './VocabCard.jsx';
import QuizCard from './QuizCard.jsx';
import SentenceCard from './SentenceCard.jsx';
import ScrambleExercise from './ScrambleExercise.jsx';
import FillBlankExercise from './FillBlankExercise.jsx';
import LessonSummary from './LessonSummary.jsx';

// Lesson phases
const PHASES = ['vocab', 'quiz', 'sentences', 'sentence-quiz', 'summary'];

export default function Lesson({ lessonNum, progress, updateProgress, onComplete, onBack, audioReady }) {
    const [phase, setPhase] = useState(0); // index into PHASES
    const [vocabIndex, setVocabIndex] = useState(0);
    const [quizIndex, setQuizIndex] = useState(0);
    const [sentenceIndex, setSentenceIndex] = useState(0);
    const [sentenceQuizIndex, setSentenceQuizIndex] = useState(0);
    const [scores, setScores] = useState({ correct: 0, partial: 0, incorrect: 0, total: 0 });
    const [wordResults, setWordResults] = useState({}); // vocabId -> 'correct'|'partial'|'incorrect'

    const vocab = getVocabByLesson(lessonNum);
    const sentences = getSentencesByLesson(lessonNum);
    const currentPhase = PHASES[phase];

    // Shuffle vocab for quiz (but keep original for intro)
    const [quizOrder, setQuizOrder] = useState([]);
    const [sentenceQuizOrder, setSentenceQuizOrder] = useState([]);

    useEffect(() => {
        // Shuffle quiz order
        const shuffled = [...vocab].sort(() => Math.random() - 0.5);
        setQuizOrder(shuffled);

        // Create sentence quiz order — mix of exercise types
        const sentenceExercises = sentences.flatMap((s, i) => {
            const types = ['translate', 'scramble', 'fillblank'];
            return [{ sentence: s, type: types[i % types.length] }];
        });
        setSentenceQuizOrder(sentenceExercises.sort(() => Math.random() - 0.5));
    }, [lessonNum]);

    // Initialize SRS items for new vocab
    useEffect(() => {
        updateProgress(prev => {
            const srsData = { ...prev.srsData };
            let changed = false;
            vocab.forEach(v => {
                if (!srsData[v.id]) {
                    srsData[v.id] = createSRSItem(v.id);
                    changed = true;
                }
            });
            if (changed) {
                return { ...prev, srsData };
            }
            return prev;
        });
    }, [lessonNum]);

    const totalSteps = vocab.length + vocab.length + sentences.length + sentenceQuizOrder.length;
    const currentStep = (() => {
        switch (currentPhase) {
            case 'vocab': return vocabIndex;
            case 'quiz': return vocab.length + quizIndex;
            case 'sentences': return vocab.length * 2 + sentenceIndex;
            case 'sentence-quiz': return vocab.length * 2 + sentences.length + sentenceQuizIndex;
            case 'summary': return totalSteps;
            default: return 0;
        }
    })();

    const recordResult = useCallback((vocabId, result) => {
        setScores(prev => ({
            correct: prev.correct + (result === 'correct' ? 1 : 0),
            partial: prev.partial + (result === 'partial' ? 1 : 0),
            incorrect: prev.incorrect + (result === 'incorrect' ? 1 : 0),
            total: prev.total + 1,
        }));

        setWordResults(prev => ({ ...prev, [vocabId]: result }));

        // Update SRS
        updateProgress(prev => {
            const srsData = { ...prev.srsData };
            if (srsData[vocabId]) {
                const quality = qualityFromResult(result);
                srsData[vocabId] = calculateNextReview(srsData[vocabId], quality);
            }
            return { ...prev, srsData };
        });
    }, [updateProgress]);

    const recordSentenceResult = useCallback((result) => {
        setScores(prev => ({
            correct: prev.correct + (result === 'correct' ? 1 : 0),
            partial: prev.partial + (result === 'partial' ? 1 : 0),
            incorrect: prev.incorrect + (result === 'incorrect' ? 1 : 0),
            total: prev.total + 1,
        }));
    }, []);

    // ─── Phase: Vocabulary Introduction ───
    if (currentPhase === 'vocab') {
        return (
            <div className="animate-in">
                <button className="back-button" onClick={onBack}>← Back to Dashboard</button>
                <PhaseHeader phase={0} />
                <ProgressIndicator current={currentStep} total={totalSteps} label={`Lesson ${lessonNum} — New Words`} />

                <VocabCard
                    word={vocab[vocabIndex]}
                    audioReady={audioReady}
                />

                <div className="nav-row">
                    {vocabIndex > 0 && (
                        <button className="btn btn-secondary" onClick={() => setVocabIndex(i => i - 1)}>
                            ← Previous
                        </button>
                    )}
                    <button
                        className="btn btn-primary"
                        onClick={() => {
                            if (vocabIndex < vocab.length - 1) {
                                setVocabIndex(i => i + 1);
                            } else {
                                setPhase(1); // Move to quiz
                            }
                        }}
                    >
                        {vocabIndex < vocab.length - 1 ? 'Next Word →' : 'Start Quiz →'}
                    </button>
                </div>
            </div>
        );
    }

    // ─── Phase: Vocabulary Quiz ───
    if (currentPhase === 'quiz') {
        const currentWord = quizOrder[quizIndex];
        return (
            <div className="animate-in" key={`quiz-${quizIndex}`}>
                <button className="back-button" onClick={onBack}>← Back to Dashboard</button>
                <PhaseHeader phase={1} />
                <ProgressIndicator current={currentStep} total={totalSteps} label={`Lesson ${lessonNum} — Vocabulary Quiz`} />

                <QuizCard
                    word={currentWord}
                    audioReady={audioReady}
                    onResult={(result) => {
                        recordResult(currentWord.id, result);
                    }}
                    onNext={() => {
                        if (quizIndex < quizOrder.length - 1) {
                            setQuizIndex(i => i + 1);
                        } else {
                            setPhase(2); // Move to sentences
                        }
                    }}
                />
            </div>
        );
    }

    // ─── Phase: Sentence Introduction ───
    if (currentPhase === 'sentences') {
        return (
            <div className="animate-in">
                <button className="back-button" onClick={onBack}>← Back to Dashboard</button>
                <PhaseHeader phase={2} />
                <ProgressIndicator current={currentStep} total={totalSteps} label={`Lesson ${lessonNum} — Sentences`} />

                <SentenceCard
                    sentence={sentences[sentenceIndex]}
                    audioReady={audioReady}
                />

                <div className="nav-row">
                    {sentenceIndex > 0 && (
                        <button className="btn btn-secondary" onClick={() => setSentenceIndex(i => i - 1)}>
                            ← Previous
                        </button>
                    )}
                    <button
                        className="btn btn-primary"
                        onClick={() => {
                            if (sentenceIndex < sentences.length - 1) {
                                setSentenceIndex(i => i + 1);
                            } else {
                                setPhase(3); // Move to sentence quiz
                            }
                        }}
                    >
                        {sentenceIndex < sentences.length - 1 ? 'Next Sentence →' : 'Sentence Practice →'}
                    </button>
                </div>
            </div>
        );
    }

    // ─── Phase: Sentence Exercises ───
    if (currentPhase === 'sentence-quiz') {
        if (sentenceQuizOrder.length === 0) {
            setPhase(4);
            return null;
        }

        const exercise = sentenceQuizOrder[sentenceQuizIndex];
        if (!exercise) {
            setPhase(4);
            return null;
        }

        const handleSentenceNext = () => {
            if (sentenceQuizIndex < sentenceQuizOrder.length - 1) {
                setSentenceQuizIndex(i => i + 1);
            } else {
                setPhase(4); // Move to summary
            }
        };

        return (
            <div className="animate-in" key={`sq-${sentenceQuizIndex}`}>
                <button className="back-button" onClick={onBack}>← Back to Dashboard</button>
                <PhaseHeader phase={3} />
                <ProgressIndicator current={currentStep} total={totalSteps} label={`Lesson ${lessonNum} — Sentence Practice`} />

                {exercise.type === 'translate' && (
                    <QuizCard
                        word={{
                            id: exercise.sentence.id,
                            character: exercise.sentence.character,
                            pinyin: exercise.sentence.pinyin,
                            english: exercise.sentence.english,
                            phonetic: '',
                        }}
                        audioReady={audioReady}
                        isSentence={true}
                        onResult={(result) => recordSentenceResult(result)}
                        onNext={handleSentenceNext}
                    />
                )}
                {exercise.type === 'scramble' && (
                    <ScrambleExercise
                        sentence={exercise.sentence}
                        onResult={(result) => recordSentenceResult(result)}
                        onNext={handleSentenceNext}
                    />
                )}
                {exercise.type === 'fillblank' && (
                    <FillBlankExercise
                        sentence={exercise.sentence}
                        audioReady={audioReady}
                        onResult={(result) => recordSentenceResult(result)}
                        onNext={handleSentenceNext}
                    />
                )}
            </div>
        );
    }

    // ─── Phase: Summary ───
    if (currentPhase === 'summary') {
        return (
            <div className="animate-in">
                <LessonSummary
                    lessonNum={lessonNum}
                    scores={scores}
                    wordResults={wordResults}
                    vocab={vocab}
                    srsData={progress.srsData}
                    onFinish={() => onComplete(lessonNum, scores)}
                />
            </div>
        );
    }

    return null;
}

function PhaseHeader({ phase }) {
    const labels = ['📖 Learn', '✏️ Quiz', '💬 Sentences', '🧩 Practice', '📊 Summary'];
    return (
        <div className="phase-indicator">
            {labels.map((label, i) => (
                <div
                    key={i}
                    className={`phase-dot ${i === phase ? 'active' : ''} ${i < phase ? 'completed' : ''}`}
                    title={label}
                />
            ))}
        </div>
    );
}

function ProgressIndicator({ current, total, label }) {
    const pct = total > 0 ? Math.round((current / total) * 100) : 0;
    return (
        <div className="progress-container">
            <div className="progress-header">
                <span>{label}</span>
                <span>{pct}%</span>
            </div>
            <div className="progress-bar">
                <div className="progress-fill" style={{ width: `${pct}%` }} />
            </div>
        </div>
    );
}
