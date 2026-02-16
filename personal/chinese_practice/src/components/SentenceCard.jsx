import React from 'react';
import { speakChinese, isAudioAvailable } from '../engine/audio.js';

export default function SentenceCard({ sentence, audioReady }) {
    const canSpeak = audioReady && isAudioAvailable();

    return (
        <div className="card text-center" style={{ margin: 'var(--space-lg) 0' }}>
            <div className="chinese-display">
                <div className="chinese-character" style={{ fontSize: '2.5rem' }}>
                    {sentence.character}
                </div>
                <div className="chinese-pinyin">{sentence.pinyin}</div>
                <div className="chinese-english">{sentence.english}</div>
            </div>

            {canSpeak && (
                <button
                    className="btn-icon"
                    onClick={() => speakChinese(sentence.character)}
                    title="Listen"
                    style={{ margin: '0 auto' }}
                >
                    🔊
                </button>
            )}
        </div>
    );
}
