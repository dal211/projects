import React from 'react';
import { TONE_VOWELS } from '../engine/pinyin.js';

export default function PinyinInput({ value, onChange, onSubmit, status, disabled, placeholder }) {
    const inputRef = React.useRef(null);

    const handleKeyDown = (e) => {
        if (e.key === 'Enter' && !disabled) {
            onSubmit();
        }
    };

    const insertTone = (char) => {
        if (disabled) return;
        const input = inputRef.current;
        if (!input) return;

        const start = input.selectionStart;
        const end = input.selectionEnd;
        const newValue = value.substring(0, start) + char + value.substring(end);
        onChange(newValue);

        // Restore cursor position after React re-render
        setTimeout(() => {
            input.selectionStart = start + char.length;
            input.selectionEnd = start + char.length;
            input.focus();
        }, 0);
    };

    const statusClass = status ? ` ${status}` : '';

    return (
        <div>
            <div className="input-wrapper">
                <input
                    ref={inputRef}
                    type="text"
                    className={`pinyin-input${statusClass}`}
                    value={value}
                    onChange={(e) => onChange(e.target.value)}
                    onKeyDown={handleKeyDown}
                    disabled={disabled}
                    placeholder={placeholder || 'Type pinyin with tone marks...'}
                    autoComplete="off"
                    autoCorrect="off"
                    autoCapitalize="off"
                    spellCheck="false"
                />
            </div>
            <div className="tone-helper">
                {TONE_VOWELS.map((row, i) => (
                    <div key={i} className="tone-row">
                        {row.map((char) => (
                            <button
                                key={char}
                                className="tone-btn"
                                onClick={() => insertTone(char)}
                                disabled={disabled}
                                tabIndex={-1}
                                title={char}
                            >
                                {char}
                            </button>
                        ))}
                    </div>
                ))}
            </div>
        </div>
    );
}
