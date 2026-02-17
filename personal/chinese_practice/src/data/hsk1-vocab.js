// HSK 1 Vocabulary — Taiwan Mandarin (Traditional Characters)
// Organized by lesson with everyday conversational focus

const hsk1Vocab = [
    // ─── Lesson 1: Greetings & Basics ───
    { id: "hsk1_001", character: "你", pinyin: "nǐ", english: "you", phonetic: "nee", category: "pronouns", lesson: 1 },
    { id: "hsk1_002", character: "好", pinyin: "hǎo", english: "good; well", phonetic: "how", category: "adjectives", lesson: 1 },
    { id: "hsk1_003", character: "你好", pinyin: "nǐ hǎo", english: "hello", phonetic: "nee how", category: "greetings", lesson: 1 },
    { id: "hsk1_004", character: "我", pinyin: "wǒ", english: "I; me", phonetic: "waw", category: "pronouns", lesson: 1 },
    { id: "hsk1_005", character: "是", pinyin: "shì", english: "to be; am; is", phonetic: "shuh", category: "verbs", lesson: 1 },
    { id: "hsk1_006", character: "不", pinyin: "bù", english: "not; no", phonetic: "boo", category: "adverbs", lesson: 1 },
    { id: "hsk1_007", character: "謝謝", pinyin: "xiè xie", english: "thank you", phonetic: "shyeh shyeh", category: "greetings", lesson: 1 },
    { id: "hsk1_008", character: "再見", pinyin: "zài jiàn", english: "goodbye", phonetic: "dzai jyen", category: "greetings", lesson: 1 },
    { id: "hsk1_009", character: "對不起", pinyin: "duì bu qǐ", english: "sorry", phonetic: "dway boo chee", category: "greetings", lesson: 1 },
    { id: "hsk1_010", character: "沒關係", pinyin: "méi guān xi", english: "it doesn't matter; no problem", phonetic: "may gwan shee", category: "greetings", lesson: 1 },

    // ─── Lesson 2: People & Introductions ───
    { id: "hsk1_011", character: "他", pinyin: "tā", english: "he; him", phonetic: "tah", category: "pronouns", lesson: 2 },
    { id: "hsk1_012", character: "她", pinyin: "tā", english: "she; her", phonetic: "tah", category: "pronouns", lesson: 2 },
    { id: "hsk1_013", character: "們", pinyin: "men", english: "(plural marker for people)", phonetic: "mun", category: "particles", lesson: 2 },
    { id: "hsk1_014", character: "我們", pinyin: "wǒ men", english: "we; us", phonetic: "waw mun", category: "pronouns", lesson: 2 },
    { id: "hsk1_015", character: "你們", pinyin: "nǐ men", english: "you (plural)", phonetic: "nee mun", category: "pronouns", lesson: 2 },
    { id: "hsk1_016", character: "這", pinyin: "zhè", english: "this", phonetic: "juh", category: "pronouns", lesson: 2 },
    { id: "hsk1_017", character: "那", pinyin: "nà", english: "that", phonetic: "nah", category: "pronouns", lesson: 2 },
    { id: "hsk1_018", character: "誰", pinyin: "shuí", english: "who", phonetic: "shway", category: "question words", lesson: 2 },
    { id: "hsk1_019", character: "什麼", pinyin: "shén me", english: "what", phonetic: "shun muh", category: "question words", lesson: 2 },
    { id: "hsk1_020", character: "名字", pinyin: "míng zi", english: "name", phonetic: "ming dzuh", category: "nouns", lesson: 2 },

    // ─── Lesson 3: Numbers ───
    { id: "hsk1_021", character: "一", pinyin: "yī", english: "one", phonetic: "ee", category: "numbers", lesson: 3 },
    { id: "hsk1_022", character: "二", pinyin: "èr", english: "two", phonetic: "are", category: "numbers", lesson: 3 },
    { id: "hsk1_023", character: "三", pinyin: "sān", english: "three", phonetic: "sahn", category: "numbers", lesson: 3 },
    { id: "hsk1_024", character: "四", pinyin: "sì", english: "four", phonetic: "suh", category: "numbers", lesson: 3 },
    { id: "hsk1_025", character: "五", pinyin: "wǔ", english: "five", phonetic: "woo", category: "numbers", lesson: 3 },
    { id: "hsk1_026", character: "六", pinyin: "liù", english: "six", phonetic: "lyoh", category: "numbers", lesson: 3 },
    { id: "hsk1_027", character: "七", pinyin: "qī", english: "seven", phonetic: "chee", category: "numbers", lesson: 3 },
    { id: "hsk1_028", character: "八", pinyin: "bā", english: "eight", phonetic: "bah", category: "numbers", lesson: 3 },
    { id: "hsk1_029", character: "九", pinyin: "jiǔ", english: "nine", phonetic: "jyoh", category: "numbers", lesson: 3 },
    { id: "hsk1_030", character: "十", pinyin: "shí", english: "ten", phonetic: "shuh", category: "numbers", lesson: 3 },

    // ─── Lesson 4: Family ───
    { id: "hsk1_031", character: "家", pinyin: "jiā", english: "home; family", phonetic: "jyah", category: "nouns", lesson: 4 },
    { id: "hsk1_032", character: "人", pinyin: "rén", english: "person; people", phonetic: "run", category: "nouns", lesson: 4 },
    { id: "hsk1_033", character: "爸爸", pinyin: "bà ba", english: "dad", phonetic: "bah bah", category: "family", lesson: 4 },
    { id: "hsk1_034", character: "媽媽", pinyin: "mā ma", english: "mom", phonetic: "mah mah", category: "family", lesson: 4 },
    { id: "hsk1_035", character: "兒子", pinyin: "ér zi", english: "son", phonetic: "are dzuh", category: "family", lesson: 4 },
    { id: "hsk1_036", character: "女兒", pinyin: "nǚ ér", english: "daughter", phonetic: "nyoo are", category: "family", lesson: 4 },
    { id: "hsk1_037", character: "朋友", pinyin: "péng yǒu", english: "friend", phonetic: "pung yo", category: "nouns", lesson: 4 },
    { id: "hsk1_038", character: "同學", pinyin: "tóng xué", english: "classmate", phonetic: "tong shweh", category: "nouns", lesson: 4 },
    { id: "hsk1_039", character: "老師", pinyin: "lǎo shī", english: "teacher", phonetic: "lao shuh", category: "nouns", lesson: 4 },
    { id: "hsk1_040", character: "學生", pinyin: "xué shēng", english: "student", phonetic: "shweh shung", category: "nouns", lesson: 4 },

    // ─── Lesson 5: Time & Dates ───
    { id: "hsk1_041", character: "年", pinyin: "nián", english: "year", phonetic: "nyen", category: "time", lesson: 5 },
    { id: "hsk1_042", character: "月", pinyin: "yuè", english: "month; moon", phonetic: "yweh", category: "time", lesson: 5 },
    { id: "hsk1_043", character: "日", pinyin: "rì", english: "day; date", phonetic: "ruh", category: "time", lesson: 5 },
    { id: "hsk1_044", character: "今天", pinyin: "jīn tiān", english: "today", phonetic: "jin tyen", category: "time", lesson: 5 },
    { id: "hsk1_045", character: "明天", pinyin: "míng tiān", english: "tomorrow", phonetic: "ming tyen", category: "time", lesson: 5 },
    { id: "hsk1_046", character: "昨天", pinyin: "zuó tiān", english: "yesterday", phonetic: "dzwaw tyen", category: "time", lesson: 5 },
    { id: "hsk1_047", character: "上午", pinyin: "shàng wǔ", english: "morning (formal)", phonetic: "shahng woo", category: "time", lesson: 5 },
    { id: "hsk1_048", character: "下午", pinyin: "xià wǔ", english: "afternoon", phonetic: "shyah woo", category: "time", lesson: 5 },
    { id: "hsk1_049", character: "現在", pinyin: "xiàn zài", english: "now", phonetic: "shyen dzai", category: "time", lesson: 5 },
    { id: "hsk1_050", character: "時候", pinyin: "shí hòu", english: "time; moment", phonetic: "shuh ho", category: "time", lesson: 5 },

    // ─── Lesson 6: Daily Activities ───
    { id: "hsk1_051", character: "吃", pinyin: "chī", english: "to eat", phonetic: "chuh", category: "verbs", lesson: 6 },
    { id: "hsk1_052", character: "喝", pinyin: "hē", english: "to drink", phonetic: "huh", category: "verbs", lesson: 6 },
    { id: "hsk1_053", character: "睡覺", pinyin: "shuì jiào", english: "to sleep", phonetic: "shway jyao", category: "verbs", lesson: 6 },
    { id: "hsk1_054", character: "看", pinyin: "kàn", english: "to look; to watch; to read", phonetic: "kahn", category: "verbs", lesson: 6 },
    { id: "hsk1_055", character: "聽", pinyin: "tīng", english: "to listen", phonetic: "ting", category: "verbs", lesson: 6 },
    { id: "hsk1_056", character: "說", pinyin: "shuō", english: "to speak; to say", phonetic: "shwaw", category: "verbs", lesson: 6 },
    { id: "hsk1_057", character: "讀", pinyin: "dú", english: "to read aloud", phonetic: "doo", category: "verbs", lesson: 6 },
    { id: "hsk1_058", character: "寫", pinyin: "xiě", english: "to write", phonetic: "shyeh", category: "verbs", lesson: 6 },
    { id: "hsk1_059", character: "做", pinyin: "zuò", english: "to do; to make", phonetic: "dzwaw", category: "verbs", lesson: 6 },
    { id: "hsk1_060", character: "工作", pinyin: "gōng zuò", english: "to work; work/job", phonetic: "gong dzwaw", category: "verbs", lesson: 6 },

    // ─── Lesson 7: Food & Drink ───
    { id: "hsk1_061", character: "飯", pinyin: "fàn", english: "rice; meal", phonetic: "fahn", category: "food", lesson: 7 },
    { id: "hsk1_062", character: "菜", pinyin: "cài", english: "vegetables; dish", phonetic: "tsai", category: "food", lesson: 7 },
    { id: "hsk1_063", character: "水", pinyin: "shuǐ", english: "water", phonetic: "shway", category: "food", lesson: 7 },
    { id: "hsk1_064", character: "茶", pinyin: "chá", english: "tea", phonetic: "chah", category: "food", lesson: 7 },
    { id: "hsk1_065", character: "水果", pinyin: "shuǐ guǒ", english: "fruit", phonetic: "shway gwaw", category: "food", lesson: 7 },
    { id: "hsk1_066", character: "蘋果", pinyin: "píng guǒ", english: "apple", phonetic: "ping gwaw", category: "food", lesson: 7 },
    { id: "hsk1_067", character: "杯子", pinyin: "bēi zi", english: "cup; glass", phonetic: "bay dzuh", category: "food", lesson: 7 },
    { id: "hsk1_068", character: "米飯", pinyin: "mǐ fàn", english: "cooked rice", phonetic: "mee fahn", category: "food", lesson: 7 },
    { id: "hsk1_069", character: "餐廳", pinyin: "cān tīng", english: "restaurant", phonetic: "tsahn ting", category: "food", lesson: 7 },
    { id: "hsk1_070", character: "好吃", pinyin: "hǎo chī", english: "delicious", phonetic: "how chuh", category: "food", lesson: 7 },

    // ─── Lesson 8: Places & Going Out ───
    { id: "hsk1_071", character: "去", pinyin: "qù", english: "to go", phonetic: "choo", category: "verbs", lesson: 8 },
    { id: "hsk1_072", character: "來", pinyin: "lái", english: "to come", phonetic: "lai", category: "verbs", lesson: 8 },
    { id: "hsk1_073", character: "回", pinyin: "huí", english: "to return", phonetic: "hway", category: "verbs", lesson: 8 },
    { id: "hsk1_074", character: "在", pinyin: "zài", english: "at; in; to be at", phonetic: "dzai", category: "prepositions", lesson: 8 },
    { id: "hsk1_075", character: "哪裡", pinyin: "nǎ lǐ", english: "where", phonetic: "nah lee", category: "question words", lesson: 8 },
    { id: "hsk1_076", character: "這裡", pinyin: "zhè lǐ", english: "here", phonetic: "juh lee", category: "pronouns", lesson: 8 },
    { id: "hsk1_077", character: "那裡", pinyin: "nà lǐ", english: "there", phonetic: "nah lee", category: "pronouns", lesson: 8 },
    { id: "hsk1_078", character: "學校", pinyin: "xué xiào", english: "school", phonetic: "shweh shyao", category: "places", lesson: 8 },
    { id: "hsk1_079", character: "醫院", pinyin: "yī yuàn", english: "hospital", phonetic: "ee ywahn", category: "places", lesson: 8 },
    { id: "hsk1_080", character: "商店", pinyin: "shāng diàn", english: "shop; store", phonetic: "shahng dyen", category: "places", lesson: 8 },

    // ─── Lesson 9: Common Verbs & Actions ───
    { id: "hsk1_081", character: "想", pinyin: "xiǎng", english: "to think; to want to", phonetic: "shyahng", category: "verbs", lesson: 9 },
    { id: "hsk1_082", character: "會", pinyin: "huì", english: "can; will; to know how to", phonetic: "hway", category: "verbs", lesson: 9 },
    { id: "hsk1_083", character: "能", pinyin: "néng", english: "can; able to", phonetic: "nung", category: "verbs", lesson: 9 },
    { id: "hsk1_084", character: "要", pinyin: "yào", english: "to want; to need; will", phonetic: "yao", category: "verbs", lesson: 9 },
    { id: "hsk1_085", character: "有", pinyin: "yǒu", english: "to have; there is", phonetic: "yo", category: "verbs", lesson: 9 },
    { id: "hsk1_086", character: "沒有", pinyin: "méi yǒu", english: "don't have; there isn't", phonetic: "may yo", category: "verbs", lesson: 9 },
    { id: "hsk1_087", character: "喜歡", pinyin: "xǐ huān", english: "to like", phonetic: "shee hwahn", category: "verbs", lesson: 9 },
    { id: "hsk1_088", character: "愛", pinyin: "ài", english: "to love", phonetic: "eye", category: "verbs", lesson: 9 },
    { id: "hsk1_089", character: "買", pinyin: "mǎi", english: "to buy", phonetic: "my", category: "verbs", lesson: 9 },
    { id: "hsk1_090", character: "叫", pinyin: "jiào", english: "to call; to be called", phonetic: "jyao", category: "verbs", lesson: 9 },

    // ─── Lesson 10: Adjectives & Descriptions ───
    { id: "hsk1_091", character: "大", pinyin: "dà", english: "big; large", phonetic: "dah", category: "adjectives", lesson: 10 },
    { id: "hsk1_092", character: "小", pinyin: "xiǎo", english: "small; little", phonetic: "shyao", category: "adjectives", lesson: 10 },
    { id: "hsk1_093", character: "多", pinyin: "duō", english: "many; much", phonetic: "dwaw", category: "adjectives", lesson: 10 },
    { id: "hsk1_094", character: "少", pinyin: "shǎo", english: "few; little", phonetic: "shao", category: "adjectives", lesson: 10 },
    { id: "hsk1_095", character: "冷", pinyin: "lěng", english: "cold", phonetic: "lung", category: "adjectives", lesson: 10 },
    { id: "hsk1_096", character: "熱", pinyin: "rè", english: "hot", phonetic: "ruh", category: "adjectives", lesson: 10 },
    { id: "hsk1_097", character: "高興", pinyin: "gāo xìng", english: "happy; glad", phonetic: "gow shing", category: "adjectives", lesson: 10 },
    { id: "hsk1_098", character: "漂亮", pinyin: "piào liàng", english: "beautiful; pretty", phonetic: "pyao lyahng", category: "adjectives", lesson: 10 },
    { id: "hsk1_099", character: "很", pinyin: "hěn", english: "very", phonetic: "hun", category: "adverbs", lesson: 10 },
    { id: "hsk1_100", character: "也", pinyin: "yě", english: "also; too", phonetic: "yeh", category: "adverbs", lesson: 10 },

    // ─── Lesson 11: Weather & Nature ───
    { id: "hsk1_101", character: "天氣", pinyin: "tiān qì", english: "weather", phonetic: "tyen chee", category: "nouns", lesson: 11 },
    { id: "hsk1_102", character: "下雨", pinyin: "xià yǔ", english: "to rain", phonetic: "shyah yoo", category: "weather", lesson: 11 },
    { id: "hsk1_103", character: "太", pinyin: "tài", english: "too (excessively)", phonetic: "tie", category: "adverbs", lesson: 11 },
    { id: "hsk1_104", character: "了", pinyin: "le", english: "(completed action particle)", phonetic: "luh", category: "particles", lesson: 11 },
    { id: "hsk1_105", character: "的", pinyin: "de", english: "(possessive/descriptive particle)", phonetic: "duh", category: "particles", lesson: 11 },
    { id: "hsk1_106", character: "嗎", pinyin: "ma", english: "(question particle)", phonetic: "mah", category: "particles", lesson: 11 },
    { id: "hsk1_107", character: "呢", pinyin: "ne", english: "(and you? particle)", phonetic: "nuh", category: "particles", lesson: 11 },
    { id: "hsk1_108", character: "都", pinyin: "dōu", english: "all; both", phonetic: "doh", category: "adverbs", lesson: 11 },
    { id: "hsk1_109", character: "怎麼", pinyin: "zěn me", english: "how; how come", phonetic: "dzun muh", category: "question words", lesson: 11 },
    { id: "hsk1_110", character: "為什麼", pinyin: "wèi shén me", english: "why", phonetic: "way shun muh", category: "question words", lesson: 11 },

    // ─── Lesson 12: Transport & Getting Around ───
    { id: "hsk1_111", character: "車", pinyin: "chē", english: "vehicle; car", phonetic: "chuh", category: "transport", lesson: 12 },
    { id: "hsk1_112", character: "計程車", pinyin: "jì chéng chē", english: "taxi", phonetic: "jee chung chuh", category: "transport", lesson: 12 },
    { id: "hsk1_113", character: "前面", pinyin: "qián miàn", english: "in front; ahead", phonetic: "chyen myen", category: "directions", lesson: 12 },
    { id: "hsk1_114", character: "後面", pinyin: "hòu miàn", english: "behind; back", phonetic: "ho myen", category: "directions", lesson: 12 },
    { id: "hsk1_115", character: "裡面", pinyin: "lǐ miàn", english: "inside", phonetic: "lee myen", category: "directions", lesson: 12 },
    { id: "hsk1_116", character: "上", pinyin: "shàng", english: "up; on; above", phonetic: "shahng", category: "directions", lesson: 12 },
    { id: "hsk1_117", character: "下", pinyin: "xià", english: "down; under; below", phonetic: "shyah", category: "directions", lesson: 12 },
    { id: "hsk1_118", character: "坐", pinyin: "zuò", english: "to sit; to take (transport)", phonetic: "dzwaw", category: "verbs", lesson: 12 },
    { id: "hsk1_119", character: "住", pinyin: "zhù", english: "to live; to stay", phonetic: "joo", category: "verbs", lesson: 12 },
    { id: "hsk1_120", character: "走", pinyin: "zǒu", english: "to walk; to go", phonetic: "dzoh", category: "verbs", lesson: 12 },

    // ─── Lesson 13: Money & Shopping ───
    { id: "hsk1_121", character: "錢", pinyin: "qián", english: "money", phonetic: "chyen", category: "nouns", lesson: 13 },
    { id: "hsk1_122", character: "塊", pinyin: "kuài", english: "(unit of money; piece)", phonetic: "kwai", category: "measure words", lesson: 13 },
    { id: "hsk1_123", character: "個", pinyin: "gè", english: "(general measure word)", phonetic: "guh", category: "measure words", lesson: 13 },
    { id: "hsk1_124", character: "幾", pinyin: "jǐ", english: "how many; several", phonetic: "jee", category: "question words", lesson: 13 },
    { id: "hsk1_125", character: "多少", pinyin: "duō shǎo", english: "how many; how much", phonetic: "dwaw shao", category: "question words", lesson: 13 },
    { id: "hsk1_126", character: "東西", pinyin: "dōng xi", english: "thing; stuff", phonetic: "dong shee", category: "nouns", lesson: 13 },
    { id: "hsk1_127", character: "一些", pinyin: "yì xiē", english: "some; a few", phonetic: "ee shyeh", category: "measure words", lesson: 13 },
    { id: "hsk1_128", character: "本", pinyin: "běn", english: "(measure word for books)", phonetic: "bun", category: "measure words", lesson: 13 },
    { id: "hsk1_129", character: "書", pinyin: "shū", english: "book", phonetic: "shoo", category: "nouns", lesson: 13 },
    { id: "hsk1_130", character: "字", pinyin: "zì", english: "character; word", phonetic: "dzuh", category: "nouns", lesson: 13 },

    // ─── Lesson 14: Everyday Phrases & Conversation ───
    { id: "hsk1_131", character: "請", pinyin: "qǐng", english: "please; to invite", phonetic: "ching", category: "verbs", lesson: 14 },
    { id: "hsk1_132", character: "請問", pinyin: "qǐng wèn", english: "may I ask; excuse me", phonetic: "ching wun", category: "phrases", lesson: 14 },
    { id: "hsk1_133", character: "知道", pinyin: "zhī dào", english: "to know (a fact)", phonetic: "juh dao", category: "verbs", lesson: 14 },
    { id: "hsk1_134", character: "認識", pinyin: "rèn shi", english: "to know (a person); to recognize", phonetic: "run shuh", category: "verbs", lesson: 14 },
    { id: "hsk1_135", character: "打電話", pinyin: "dǎ diàn huà", english: "to make a phone call", phonetic: "dah dyen hwah", category: "verbs", lesson: 14 },
    { id: "hsk1_136", character: "電腦", pinyin: "diàn nǎo", english: "computer", phonetic: "dyen now", category: "nouns", lesson: 14 },
    { id: "hsk1_137", character: "中文", pinyin: "zhōng wén", english: "Chinese (language)", phonetic: "jong wun", category: "nouns", lesson: 14 },
    { id: "hsk1_138", character: "學", pinyin: "xué", english: "to learn; to study", phonetic: "shweh", category: "verbs", lesson: 14 },
    { id: "hsk1_139", character: "歲", pinyin: "suì", english: "years old", phonetic: "sway", category: "measure words", lesson: 14 },
    { id: "hsk1_140", character: "點", pinyin: "diǎn", english: "o'clock; point; a little", phonetic: "dyen", category: "nouns", lesson: 14 },

    // ─── Lesson 15: Review & Extra Essentials ───
    { id: "hsk1_141", character: "沒", pinyin: "méi", english: "not (for 有)", phonetic: "may", category: "adverbs", lesson: 15 },
    { id: "hsk1_142", character: "和", pinyin: "hé", english: "and; with", phonetic: "huh", category: "conjunctions", lesson: 15 },
    { id: "hsk1_143", character: "在", pinyin: "zài", english: "at; (progressive marker)", phonetic: "dzai", category: "prepositions", lesson: 15 },
    { id: "hsk1_144", character: "先生", pinyin: "xiān shēng", english: "Mr.; sir; husband", phonetic: "shyen shung", category: "nouns", lesson: 15 },
    { id: "hsk1_145", character: "小姐", pinyin: "xiǎo jiě", english: "Miss; young lady", phonetic: "shyao jyeh", category: "nouns", lesson: 15 },
    { id: "hsk1_146", character: "不客氣", pinyin: "bú kè qì", english: "you're welcome", phonetic: "boo kuh chee", category: "phrases", lesson: 15 },
    { id: "hsk1_147", character: "太好了", pinyin: "tài hǎo le", english: "great!; wonderful!", phonetic: "tie how luh", category: "phrases", lesson: 15 },
    { id: "hsk1_148", character: "可以", pinyin: "kě yǐ", english: "can; may; okay", phonetic: "kuh ee", category: "verbs", lesson: 15 },
    { id: "hsk1_149", character: "喂", pinyin: "wèi", english: "hello (on phone)", phonetic: "way", category: "interjections", lesson: 15 },
    { id: "hsk1_150", character: "對", pinyin: "duì", english: "correct; right", phonetic: "dway", category: "adjectives", lesson: 15 },
];

export default hsk1Vocab;

export const TOTAL_LESSONS = 15;

export function getVocabByLesson(lessonNum) {
    return hsk1Vocab.filter(v => v.lesson === lessonNum);
}

export function getVocabById(id) {
    return hsk1Vocab.find(v => v.id === id);
}
