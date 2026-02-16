import React from 'react';
import { speakChinese, isAudioAvailable } from '../engine/audio.js';

export default function VocabCard({ word, audioReady }) {
    const canSpeak = audioReady && isAudioAvailable();

    const handleSpeak = () => {
        if (canSpeak) {
            speakChinese(word.character);
        }
    };

    return (
        <div className="card text-center" style={{ margin: 'var(--space-lg) 0' }}>
            <div className="chinese-display">
                <div className="chinese-character large">{word.character}</div>
                <div className="chinese-pinyin">{word.pinyin}</div>
                <div className="chinese-english">{word.english}</div>
                {(!canSpeak && word.phonetic) && (
                    <div className="chinese-phonetic">🔤 Sounds like: "{word.phonetic}"</div>
                )}
            </div>

            {canSpeak && (
                <button className="btn-icon" onClick={() => speakChinese(word.character, word.phonetic)} title="Listen">
                    🔊
                </button>
            )}

            <div style={{
                marginTop: 'var(--space-md)',
                padding: 'var(--space-sm) var(--space-md)',
                background: 'var(--color-bg-elevated)',
                borderRadius: 'var(--radius-sm)',
                fontSize: '0.8rem',
                color: 'var(--color-text-muted)',
            }}>
                Category: {word.category}
            </div>
        </div>
    );
}
