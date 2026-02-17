// HSK 6 Sentences — Taiwan Mandarin
// Lessons 1-5

const hsk6Sentences = [
    // Lesson 1
    { id: "s6_001", character: "他對這件古董愛不釋手。", pinyin: "tā duì zhè jiàn gǔ dǒng ài bù shì shǒu", english: "He loves this antique so much he can't put it down.", vocabIds: ["hsk6_003"], lesson: 1 },
    { id: "s6_002", character: "他的話中暗示了一些細節。", pinyin: "tā de huà zhōng àn shì le yī xiē xì jié", english: "His words hinted at some details.", vocabIds: ["hsk6_009"], lesson: 1 },
    { id: "s6_003", character: "我們需要解開自然界的奧秘。", pinyin: "wǒ men xū yào jiě kāi zì rán jiè de ào mì", english: "We need to unlock the mysteries of nature.", vocabIds: ["hsk6_016"], lesson: 1 },

    // Lesson 2
    { id: "s6_004", character: "我們不能半途而廢。", pinyin: "wǒ men bù néng bàn tú ér fèi", english: "We cannot give up halfway.", vocabIds: ["hsk6_039"], lesson: 2 },
    { id: "s6_005", character: "他正在擺脫過去的陰影。", pinyin: "tā zhèng zài bǎi tuō guò qù de yīn yǐng", english: "He is breaking away from the shadows of the past.", vocabIds: ["hsk6_028"], lesson: 2 },
    { id: "s6_006", character: "他的伴侶一直支持著他。", pinyin: "tā de bàn lǚ yī zhí zhī chí zhe tā", english: "His partner has always supported him.", vocabIds: ["hsk6_037"], lesson: 2 },

    // Lesson 3
    { id: "s6_007", character: "這位老人飽經風霜。", pinyin: "zhè wèi lǎo rén bǎo jīng fēng shuāng", english: "This old man is weather-beaten.", vocabIds: ["hsk6_049"], lesson: 3 },
    { id: "s6_008", character: "請務必對此事保密。", pinyin: "qǐng wù bì duì cǐ shì bǎo mì", english: "Please be sure to keep this matter secret.", vocabIds: ["hsk6_051"], lesson: 3 },
    { id: "s6_009", character: "他有著遠大的抱負。", pinyin: "tā yǒu zhe yuǎn dà de bào fù", english: "He has great aspirations.", vocabIds: ["hsk6_058"], lesson: 3 },

    // Lesson 4
    { id: "s6_010", character: "他能背誦整篇課文。", pinyin: "tā néng bèi sòng zhěng piān kè wén", english: "He can recite the entire text from memory.", vocabIds: ["hsk6_072"], lesson: 4 },
    { id: "s6_011", character: "這是一個備忘錄，記錄了會議要點。", pinyin: "zhè shì yí gè bèi wàng lù, jì lù le huì yì yào diǎn", english: "This is a memo recording the key points of the meeting.", vocabIds: ["hsk6_070"], lesson: 4 },
    { id: "s6_012", character: "他在各個城市之間奔波。", pinyin: "tā zài gè gè chéng shì zhī jiān bēn bō", english: "He is rushing about between various cities.", vocabIds: ["hsk6_076"], lesson: 4 },

    // Lesson 5
    { id: "s6_013", character: "經濟正在蓬勃發展。", pinyin: "jīng jì zhèng zài péng bó fā zhǎn", english: "The economy is flourishing.", vocabIds: ["hsk6_087"], lesson: 5 },
    { id: "s6_014", character: "這件事注定要失敗。", pinyin: "zhè jiàn shì zhù dìng yào shī bài", english: "This matter is bound to fail.", vocabIds: ["hsk6_095"], lesson: 5 },
    { id: "s6_015", character: "我們必須消除貿易壁壘。", pinyin: "wǒ men bì xū xiāo chú mào yì bì lěi", english: "We must eliminate trade barriers.", vocabIds: ["hsk6_097"], lesson: 5 },

    // Lesson 6
    { id: "s6_016", character: "政治穩定是國家發展的前提。", pinyin: "zhèng zhì wěn dìng shì guó jiā fā zhǎn de qián tí", english: "Political stability is the premise of national development.", vocabIds: ["hsk6_101", "hsk6_109"], lesson: 6 },
    { id: "s6_017", character: "改革需要強而有力的策略。", pinyin: "gǎi gé xū yào qiáng ér yǒu lì de cè lüè", english: "Reform requires strong and powerful strategies.", vocabIds: ["hsk6_108", "hsk6_105"], lesson: 6 },

    // Lesson 7
    { id: "s6_018", character: "這部詩歌具有卓越的藝術意境。", pinyin: "zhè bù shī gē jù yǒu zhuó yuè de yì shù yì jìng", english: "This poem has an outstanding artistic conception.", vocabIds: ["hsk6_112", "hsk6_120", "hsk6_115"], lesson: 7 },
    { id: "s6_019", character: "經典文字在歷史中長久流傳。", pinyin: "jīng diǎn wén zì zài lì shǐ zhōng cháng jiǔ liú chuán", english: "Classic texts circulate for a long time in history.", vocabIds: ["hsk6_111", "hsk6_116"], lesson: 7 },

    // Lesson 8
    { id: "s6_020", character: "實踐是檢驗真理的唯一標準。", pinyin: "shí jiàn shì jiǎn yàn zhēn lǐ de wéi yī biāo zhǔn", english: "Practice is the sole criterion for testing truth.", vocabIds: ["hsk6_126", "hsk6_125"], lesson: 8 },
    { id: "s6_021", character: "這兩者之間存在著深刻的矛盾。", pinyin: "zhè liǎng zhě zhī jiān cún zài zhe shēn kè de máo dùn", english: "There is a profound contradiction between the two.", vocabIds: ["hsk6_124"], lesson: 8 },

    // Lesson 9
    { id: "s6_022", character: "推動社會和諧是我們的義務。", pinyin: "tuī dòng shè huì hé xié shì wǒ men de yì wù", english: "Promoting social harmony is our duty.", vocabIds: ["hsk6_133", "hsk6_134", "hsk6_131"], lesson: 9 },
    { id: "s6_023", character: "慈善事業展現了人類的尊嚴。", pinyin: "cí shàn shì yè zhǎn xiàn le rén lèi de zūn yán", english: "Charity work demonstrates human dignity.", vocabIds: ["hsk6_132", "hsk6_137"], lesson: 9 },

    // Lesson 10
    { id: "s6_024", character: "這項精密工程代表了尖端科技。", pinyin: "zhè xiàng jīng mì gōng chéng dài biǎo le jiān duān kē jì", english: "This precision engineering represents cutting-edge technology.", vocabIds: ["hsk6_146", "hsk6_141", "hsk6_144"], lesson: 10 },
    { id: "s6_025", character: "自動化系統提升了產能。", pinyin: "zì dòng huà xì tǒng tí shēng le chǎn néng", english: "Automation systems have increased production capacity.", vocabIds: ["hsk6_148"], lesson: 10 },

    // Lesson 11
    { id: "s6_026", character: "外交談判正在艱難地進行中。", pinyin: "wài jiāo tán pàn zhèng zài jiān nán de jìn háng zhōng", english: "Diplomatic negotiations are underway with difficulty.", vocabIds: ["hsk6_151", "hsk6_153"], lesson: 11 },
    { id: "s6_027", character: "各國正在尋求建立國際聯盟。", pinyin: "gè guó zhèng zài xún qiú jiàn lì guó jì lián méng", english: "Countries are seeking to establish international alliances.", vocabIds: ["hsk6_152"], lesson: 11 },

    // Lesson 12
    { id: "s6_028", character: "歷史的變遷有其自身的規律。", pinyin: "shǐ lì de biàn qiān yǒu qí zì shēn de guī lǜ", english: "Historical transitions have their own laws.", vocabIds: ["hsk6_161", "hsk6_169"], lesson: 12 },
    { id: "s6_029", character: "反思教訓是為了更好的傳承。", pinyin: "fǎn sī jiào xun shì wèi le gèng hǎo de chuán chéng", english: "Reflecting on lessons is for better inheritance.", vocabIds: ["hsk6_170", "hsk6_167", "hsk6_168"], lesson: 12 },

    // Lesson 13
    { id: "s6_030", character: "這部電影引發了觀眾的共鳴。", pinyin: "zhè bù diàn yǐng yǐn fā le guān zhòng de gòng míng", english: "This movie triggered a resonance among the audience.", vocabIds: ["hsk6_177"], lesson: 13 },
    { id: "s6_031", character: "鑑賞藝術作品需要深厚的底蘊。", pinyin: "jiàn shǎng yì shù zuò pǐn xū yào shēn hòu de dǐ yùn", english: "Appreciating works of art requires deep background.", vocabIds: ["hsk6_171"], lesson: 13 },

    // Lesson 14
    { id: "s6_032", character: "嚴謹的論證增強了說服力。", pinyin: "yán jǐn de lùn zhèng zēng qiáng le shuō fú lì", english: "Rigorous argumentation enhances persuasiveness.", vocabIds: ["hsk6_189", "hsk6_185", "hsk6_190"], lesson: 14 },
    { id: "s6_033", character: "這是一個充滿邏輯的結論。", pinyin: "zhè shì yí gè chōng mǎn luó jí de jié lùn", english: "This is a conclusion full of logic.", vocabIds: ["hsk6_181", "hsk6_183"], lesson: 14 },

    // Lesson 15
    { id: "s6_034", character: "他在學術研究上有極高的造詣。", pinyin: "tā zài xué shù yán jiū shàng yǒu jí gāo de zào yì", english: "He has extreme attainments in academic research.", vocabIds: ["hsk6_197", "hsk6_195"], lesson: 15 },
    { id: "s6_035", character: "掌握新技術是科學發突破的關鍵。", pinyin: "zhǎng wò xīn jì shù shì kē xué tū pò de guān jiàn", english: "Mastering new technology is the key to scientific breakthroughs.", vocabIds: ["hsk6_192", "hsk6_200"], lesson: 15 },
];

export default hsk6Sentences;
export function getSentencesByLesson(lessonNum) {
    return hsk6Sentences.filter(s => s.lesson === lessonNum);
}
