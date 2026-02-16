import React, { useState, useEffect, useCallback } from 'react';
import Dashboard from './Dashboard.jsx';
import Lesson from './Lesson.jsx';
import ReviewSession from './ReviewSession.jsx';
import { loadProgress, saveProgress, resetProgress } from '../engine/storage.js';
import { getDueItems, createSRSItem } from '../engine/srs.js';
import { initAudio } from '../engine/audio.js';
import hsk1Vocab from '../data/hsk1-vocab.js';

export default function App() {
    const [view, setView] = useState('dashboard'); // dashboard | lesson | review
    const [currentLesson, setCurrentLesson] = useState(null);
    const [userProgress, setUserProgress] = useState(null); // Full progress object
    const [audioReady, setAudioReady] = useState(false);

    // Load progress on mount
    useEffect(() => {
        const saved = loadProgress();
        setUserProgress(saved);
        initAudio().then(ready => setAudioReady(ready));
    }, []);

    // Save progress whenever it changes
    useEffect(() => {
        if (userProgress) {
            saveProgress(userProgress);
        }
    }, [userProgress]);

    // Helper to get current level's progress
    const currentLevel = userProgress?.level || 1;
    const progress = userProgress?.levels?.[currentLevel] || {};

    const updateLevelProgress = useCallback((updater) => {
        setUserProgress(prev => {
            const currentLvl = prev.level;
            const currentData = prev.levels[currentLvl];
            const nextData = typeof updater === 'function' ? updater(currentData) : { ...currentData, ...updater };

            return {
                ...prev,
                levels: {
                    ...prev.levels,
                    [currentLvl]: nextData
                }
            };
        });
    }, []);

    const setLevel = useCallback((newLevel) => {
        setUserProgress(prev => ({
            ...prev,
            level: newLevel
        }));
        setView('dashboard');
        setCurrentLesson(null);
    }, []);

    const startLesson = useCallback((lessonNum) => {
        setCurrentLesson(lessonNum);
        setView('lesson');
    }, []);

    const completeLesson = useCallback((lessonNum, scores) => {
        updateLevelProgress(prev => {
            const completedLessons = prev.completedLessons.includes(lessonNum)
                ? prev.completedLessons
                : [...prev.completedLessons, lessonNum];

            const lessonScores = {
                ...prev.lessonScores,
                [lessonNum]: scores,
            };

            return {
                ...prev,
                completedLessons,
                lessonScores,
                currentLesson: Math.max(prev.currentLesson, lessonNum + 1),
            };
        });
        setView('dashboard');
        setCurrentLesson(null);
    }, [updateLevelProgress]);

    const startReview = useCallback(() => {
        setView('review');
    }, []);

    const finishReview = useCallback(() => {
        setView('dashboard');
    }, []);

    const goHome = useCallback(() => {
        setView('dashboard');
        setCurrentLesson(null);
    }, []);

    const handleReset = useCallback(() => {
        if (window.confirm('Are you sure you want to reset all progress? This cannot be undone.')) {
            resetProgress();
            setUserProgress(loadProgress());
            setView('dashboard');
        }
    }, []);

    if (!userProgress) {
        return (
            <div className="app-container flex-center" style={{ height: '100vh' }}>
                <div className="text-center">
                    <div className="chinese-character">載入中...</div>
                    <div className="chinese-pinyin">Loading...</div>
                </div>
            </div>
        );
    }

    const dueCount = getDueItems(progress.srsData || {}).length;

    return (
        <div className="app-container">
            {view === 'dashboard' && (
                <Dashboard
                    currentLevel={currentLevel}
                    setLevel={setLevel}
                    progress={progress}
                    dueCount={dueCount}
                    onStartLesson={startLesson}
                    onStartReview={startReview}
                    onReset={handleReset}
                    audioReady={audioReady}
                />
            )}
            {view === 'lesson' && currentLesson && (
                <Lesson
                    level={currentLevel}
                    lessonNum={currentLesson}
                    progress={progress}
                    updateProgress={updateLevelProgress}
                    onComplete={completeLesson}
                    onBack={goHome}
                    audioReady={audioReady}
                />
            )}
            {view === 'review' && (
                <ReviewSession
                    level={currentLevel}
                    progress={progress}
                    updateProgress={updateLevelProgress}
                    onFinish={finishReview}
                    onBack={goHome}
                    audioReady={audioReady}
                />
            )}
        </div>
    );
}
