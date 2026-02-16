// Audio playback using Web Speech API
// Priority: zh-TW > zh-CN > English phonetic fallback

let cachedVoice = null;
let voiceChecked = false;

/**
 * Find the best available Chinese voice
 */
function findChineseVoice() {
    if (voiceChecked) return cachedVoice;

    const voices = window.speechSynthesis?.getVoices() || [];

    // Priority: Taiwan Mandarin > Mainland Mandarin
    cachedVoice =
        voices.find(v => v.lang === 'zh-TW') ||
        voices.find(v => v.lang.startsWith('zh-TW')) ||
        voices.find(v => v.lang === 'zh-CN') ||
        voices.find(v => v.lang.startsWith('zh-CN')) ||
        voices.find(v => v.lang.startsWith('zh')) ||
        null;

    voiceChecked = true;
    return cachedVoice;
}

/**
 * Initialize voices (they load async in some browsers)
 */
export function initAudio() {
    return new Promise((resolve) => {
        if (!window.speechSynthesis) {
            resolve(false);
            return;
        }

        const voices = window.speechSynthesis.getVoices();
        if (voices.length > 0) {
            findChineseVoice();
            resolve(!!cachedVoice);
            return;
        }

        window.speechSynthesis.onvoiceschanged = () => {
            findChineseVoice();
            resolve(!!cachedVoice);
        };

        // Timeout fallback
        setTimeout(() => {
            findChineseVoice();
            resolve(!!cachedVoice);
        }, 1000);
    });
}

/**
 * Speak Chinese text using TTS
 * @param {string} text - Chinese characters to speak
 * @param {string} [phonetic] - English phonetic approximation (fallback)
 * @param {number} rate - speech rate (0.5 - 2.0, default 0.8 for learning)
 * @returns {boolean} true if speech started, false if unavailable
 */
export function speakChinese(text, phonetic, rate = 0.8) {
    if (!window.speechSynthesis) return false;

    // Cancel any ongoing speech
    window.speechSynthesis.cancel();

    const voice = findChineseVoice();

    if (voice) {
        // Speak Chinese
        const utterance = new SpeechSynthesisUtterance(text);
        utterance.voice = voice;
        utterance.lang = voice.lang;
        utterance.rate = rate;
        utterance.pitch = 1;
        window.speechSynthesis.speak(utterance);
        return true;
    } else if (phonetic) {
        // Fallback: Speak phonetic approximation using default (likely English) voice
        const utterance = new SpeechSynthesisUtterance(phonetic);
        // Let browser pick default voice (usually English/System language)
        utterance.rate = 0.9; // Slightly slower for clarity
        window.speechSynthesis.speak(utterance);
        return true;
    }

    return false;
}

/**
 * Check if Chinese TTS is available
 */
export function isAudioAvailable() {
    if (!window.speechSynthesis) return false;
    return !!findChineseVoice();
}

/**
 * Get the voice locale being used
 */
export function getVoiceLocale() {
    const voice = findChineseVoice();
    return voice ? voice.lang : null;
}
