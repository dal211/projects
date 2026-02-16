import { getAllLevels, getLevelData } from '../data/index.js';
import { getMastery } from '../engine/srs.js';
import { exportProgress } from '../engine/storage.js';

const LESSON_THEMES = {
    1: {
        1: { title: "Greetings & Basics", desc: "Hello, Goodbye, Thank you" },
        2: { title: "People & Introductions", desc: "Pronouns, Who, What, Name" },
        3: { title: "Numbers", desc: "Counting 1-10" },
        4: { title: "Family", desc: "Dad, Mom, Son, Daughter" },
        5: { title: "Time & Dates", desc: "Today, Tomorrow, Year, Month" },
        6: { title: "Daily Activities", desc: "Eat, Drink, Sleep, Listen" },
        7: { title: "Food & Drink", desc: "Rice, Tea, Fruit, Delicious" },
        8: { title: "Places & Going Out", desc: "School, Hospital, Store" },
        9: { title: "Common Verbs", desc: "Want, Can, Have, Like" },
        10: { title: "Descriptions", desc: "Big, Small, Hot, Cold" },
        11: { title: "Weather & Nature", desc: "Rain, Weather, Why" },
        12: { title: "Transport", desc: "Car, Taxi, Directions" },
        13: { title: "Money & Shopping", desc: "Buy, Money, How much" },
        14: { title: "Conversation", desc: "Phone, Computer, Study" },
        15: { title: "Review & Essentials", desc: "Mr., Miss, You're welcome" },
    },
    2: {
        1: { title: "Travel & Location", desc: "Airport, Station, Far, Near" },
        2: { title: "Daily Routine", desc: "Get up, Sleep, Begin, Prepare" },
        3: { title: "Colors & Descriptions", desc: "Red, White, New, Old" },
        4: { title: "Sports & Exercise", desc: "Run, Swim, Play ball" },
        5: { title: "Feelings & Body", desc: "Sick, Medicine, Eye, Hand" },
        6: { title: "Food & Service", desc: "Waiter, Egg, Lamb, Cheap" },
        7: { title: "Weather & Seasons", desc: "Winter, Snow, Cloudy" },
        8: { title: "House & Objects", desc: "Door, Room, Light, Watch" },
        9: { title: "Work & Study", desc: "Exam, Question, Meaning" },
        10: { title: "Relationships", desc: "Wife, Husband, Help, Introduce" },
        11: { title: "Time & Frequency", desc: "Every, Time, Long, Fast" },
        12: { title: "Comparisons", desc: "More, Most, Than" },
        13: { title: "Actions & Directions", desc: "Enter, Give, Ask, Wait" },
        14: { title: "Modals & Particles", desc: "Because, So, Although" },
        15: { title: "Review & Integration", desc: "Travel, Birthday, Sing" },
    },
    3: {
        1: { title: "Weekend Plans", desc: "Plan, Weekend, North, South" },
        2: { title: "At the Office", desc: "Manager, Secretary, Leg, Hurt" },
        3: { title: "Drinks & Likes", desc: "Coffee, Tea, Shirt, Fresh" },
        4: { title: "She's always smiling", desc: "Smart, Enthusiastic, Supermarket" },
        5: { title: "Health & Seasons", desc: "Fever, Spring, Summer, Care" },
    },
    4: {
        1: { title: "Love & Affection", desc: "Romance, Arrange, Safe, Guarantee" },
        2: { title: "Daily Life & Work", desc: "Performance, Biscuit, Regardless" },
        3: { title: "Feelings & Communication", desc: "Success, Honest, Shocked" },
        4: { title: "Time & Occasions", desc: "Careless, Answer, Disturb" },
        5: { title: "Society & Culture", desc: "Apologize, Earth, Movement" },
    },
    5: {
        1: { title: "Individual & Perspective", desc: "Cherish, Comfort, Coastal, Grasp" },
        2: { title: "Family & Society", desc: "Insurance, Background, Undergraduate" },
        3: { title: "Communication & Expression", desc: "Debate, Symbol, facial expression" },
        4: { title: "Finance & Development", desc: "Property, Interview, Rainbow" },
        5: { title: "Society & Culture", desc: "Damp, Thorough, Ingredient" },
    }
};

export default function Dashboard({ currentLevel, setLevel, progress, dueCount, onStartLesson, onStartReview, onReset, audioReady }) {
    const { completedLessons = [], currentLesson = 1, srsData = {} } = progress;
    const allLevels = getAllLevels();
    const { totalLessons, getVocabByLesson } = getLevelData(currentLevel);

    // Calculate overall mastery
    const allItems = Object.values(srsData);
    const overallMastery = allItems.length > 0
        ? Math.round(allItems.reduce((sum, item) => sum + getMastery(item), 0) / allItems.length)
        : 0;

    const totalWords = allItems.length;
    const masteredWords = allItems.filter(item => getMastery(item) >= 80).length;

    return (
        <div className="animate-in">
            {/* Header */}
            <header className="app-header">
                <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: 'var(--space-md)' }}>
                    <div>
                        <h1 className="app-title">華語練習</h1>
                        <p className="app-subtitle">Mandarin Chinese Practice</p>
                    </div>
                    <select
                        value={currentLevel}
                        onChange={(e) => setLevel(Number(e.target.value))}
                        className="level-selector"
                        style={{
                            padding: 'var(--space-xs) var(--space-sm)',
                            borderRadius: 'var(--radius-sm)',
                            background: 'var(--color-bg-card)',
                            color: 'var(--color-text-primary)',
                            border: '1px solid var(--color-border)',
                            fontSize: '1rem',
                            fontWeight: 'bold'
                        }}
                    >
                        {allLevels.map(lvl => (
                            <option key={lvl} value={lvl}>HSK {lvl}</option>
                        ))}
                    </select>
                </div>
            </header>

            {/* Stats */}
            {totalWords > 0 && (
                <div className="card-flat mb-lg">
                    <div className="score-grid" style={{ margin: 0 }}>
                        <div className="score-item">
                            <div className="score-value" style={{ color: 'var(--color-primary)' }}>{totalWords}</div>
                            <div className="score-label">Words Seen</div>
                        </div>
                        <div className="score-item">
                            <div className="score-value" style={{ color: 'var(--color-correct)' }}>{masteredWords}</div>
                            <div className="score-label">Mastered</div>
                        </div>
                        <div className="score-item">
                            <div className="score-value" style={{ color: 'var(--color-accent)' }}>{overallMastery}%</div>
                            <div className="score-label">Mastery</div>
                        </div>
                    </div>
                </div>
            )}

            {/* Review Banner */}
            {dueCount > 0 && (
                <div className="review-banner">
                    <div className="review-banner-text">
                        <span className="review-count">{dueCount}</span> words ready for review
                    </div>
                    <button className="btn btn-primary btn-sm" onClick={onStartReview}>
                        Review Now
                    </button>
                </div>
            )}

            {/* Audio status */}
            {!audioReady && (
                <div style={{
                    padding: 'var(--space-sm) var(--space-md)',
                    background: 'var(--color-partial-bg)',
                    border: '1px solid var(--color-partial-border)',
                    borderRadius: 'var(--radius-sm)',
                    fontSize: '0.8rem',
                    color: 'var(--color-partial)',
                    marginBottom: 'var(--space-md)',
                    textAlign: 'center',
                }}>
                    🔇 Audio unavailable — English phonetic hints will be shown instead
                </div>
            )}

            {/* Lesson List */}
            <h2 style={{ fontSize: '1.1rem', color: 'var(--color-text-secondary)', marginBottom: 'var(--space-md)' }}>
                Lessons (HSK {currentLevel})
            </h2>
            <div className="lesson-grid">
                {Array.from({ length: totalLessons }, (_, i) => i + 1).map(num => {
                    const isCompleted = completedLessons.includes(num);
                    const isActive = num === currentLesson || (num <= currentLesson && !isCompleted);
                    const isLocked = num > currentLesson && !isCompleted;

                    // Theme logic
                    const theme = LESSON_THEMES[currentLevel]?.[num];

                    // Calculate lesson mastery
                    const vocab = getVocabByLesson(num);
                    const lessonSRS = vocab.map(v => srsData[v.id]).filter(Boolean);
                    const lessonMastery = lessonSRS.length > 0
                        ? Math.round(lessonSRS.reduce((s, i) => s + getMastery(i), 0) / vocab.length)
                        : 0;

                    return (
                        <div
                            key={num}
                            className={`lesson-item ${isCompleted ? 'completed' : ''} ${isActive && !isCompleted ? 'active' : ''} ${isLocked ? 'locked' : ''}`}
                            onClick={() => !isLocked && onStartLesson(num)}
                        >
                            <div className="lesson-number">
                                {isCompleted ? '✓' : num}
                            </div>
                            <div className="lesson-info">
                                <div className="lesson-title">{theme?.title || `Lesson ${num}`}</div>
                                <div className="lesson-desc">{theme?.desc}</div>
                            </div>
                            {isCompleted && (
                                <div className="lesson-status">{lessonMastery}%</div>
                            )}
                            {isActive && !isCompleted && (
                                <div className="lesson-status" style={{ color: 'var(--color-primary)' }}>Start →</div>
                            )}
                        </div>
                    );
                })}
            </div>

            {/* Settings */}
            <div className="settings-panel">
                <div className="settings-row">
                    <span className="settings-label">Export Progress</span>
                    <button className="btn-ghost btn-sm" onClick={exportProgress}>💾 Export</button>
                </div>
                <div className="settings-row">
                    <span className="settings-label">Reset All Progress</span>
                    <button className="danger-btn" onClick={onReset}>Reset</button>
                </div>
            </div>
        </div>
    );
}
