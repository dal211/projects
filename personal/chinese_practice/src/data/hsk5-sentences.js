// HSK 5 Sentences — Taiwan Mandarin
// Lessons 1-5

const hsk5Sentences = [
    // Lesson 1
    { id: "s5_001", character: "我們應該愛護大自然。", pinyin: "wǒ men yīng gāi ài hù dà zì rán", english: "We should cherish nature.", vocabIds: ["hsk5_001"], lesson: 1 },
    { id: "s5_002", character: "這件事我沒有把握。", pinyin: "zhè jiàn shì wǒ méi yǒu bǎ wò", english: "I am not sure about this matter.", vocabIds: ["hsk5_008"], lesson: 1 },
    { id: "s5_003", character: "夕陽下的海岸非常漂亮。", pinyin: "xī yáng xià de hǎi àn fēi cháng piào liàng", english: "The coast under the sunset is very beautiful.", vocabIds: ["hsk5_006"], lesson: 1 },

    // Lesson 2
    { id: "s5_004", character: "請保留這張收據。", pinyin: "qǐng bǎo liú zhè zhāng shōu jù", english: "Please keep this receipt.", vocabIds: ["hsk5_022"], lesson: 2 },
    { id: "s5_005", character: "畢竟他還是個孩子。", pinyin: "bì jìng tā hái shì ge hái zi", english: "After all, he is still a child.", vocabIds: ["hsk5_036"], lesson: 2 },
    { id: "s5_006", character: "他在背景音樂中說話。", pinyin: "tā zài bèi jǐng yīn yuè zhōng shuō huà", english: "He is speaking with background music.", vocabIds: ["hsk5_027"], lesson: 2 },

    // Lesson 3
    { id: "s5_007", character: "每個人都有表達意見的權利。", pinyin: "měi gè rén dōu yǒu biǎo dá yì jiàn de quán lì", english: "Everyone has the right to express their opinions.", vocabIds: ["hsk5_044"], lesson: 3 },
    { id: "s5_008", character: "表面上他很淡定。", pinyin: "biǎo miàn shàng tā hěn dàn dìng", english: "On the surface, he is very calm.", vocabIds: ["hsk5_045"], lesson: 3 },
    { id: "s5_009", character: "博物館明天不開放。", pinyin: "bó wù guǎn míng tiān bù kāi fàng", english: "The museum is not open tomorrow.", vocabIds: ["hsk5_053"], lesson: 3 },

    // Lesson 4
    { id: "s5_010", character: "參與這項活動很有意義。", pinyin: "cān yù zhè xiàng huó dòng hěn yǒu yì yì", english: "Participating in this activity is very meaningful.", vocabIds: ["hsk5_068"], lesson: 4 },
    { id: "s5_011", character: "這是我曾經住過的地方。", pinyin: "zhè shì wǒ céng jīng zhù guo de dì fang", english: "This is a place where I once lived.", vocabIds: ["hsk5_075"], lesson: 4 },
    { id: "s5_012", character: "請按照步驟操作。", pinyin: "qǐng àn zhào bù zhòu cāo zuò", english: "Please follow the steps to operate.", vocabIds: ["hsk5_061"], lesson: 4 },

    // Lesson 5
    { id: "s5_013", character: "這是一個徹底的改變。", pinyin: "zhè shì yí ge chè dǐ de gǎi biàn", english: "This is a thorough change.", vocabIds: ["hsk5_093"], lesson: 5 },
    { id: "s5_014", character: "大家都保持沉默。", pinyin: "dà jiā dōu bǎo chí chén mò", english: "Everyone kept silent.", vocabIds: ["hsk5_094"], lesson: 5 },
    { id: "s5_015", character: "趁天還沒黑，我們走吧。", pinyin: "chèn tiān hái méi hēi, wǒ men zǒu ba", english: "Let's go while it's still light.", vocabIds: ["hsk5_095"], lesson: 5 },

    // Lesson 6
    { id: "s5_016", character: "政府正在採取措施應對通膨。", pinyin: "zhèng fǔ zhèng zài cǎi qǔ cuò shī yìng duì tōng péng", english: "The government is taking measures to deal with inflation.", vocabIds: ["hsk5_110", "hsk5_101"], lesson: 6 },
    { id: "s5_017", character: "這筆投資帶來了豐厚的利潤。", pinyin: "zhè bǐ tóu zī dài lái le fēng hòu de lì rùn", english: "This investment brought rich profits.", vocabIds: ["hsk5_103", "hsk5_104"], lesson: 6 },

    // Lesson 7
    { id: "s5_018", character: "他因為工作壓力感到非常焦慮。", pinyin: "tā yīn wèi gōng zuò yā lì gǎn dào fēi cháng jiāo lǜ", english: "He feels very anxious because of work pressure.", vocabIds: ["hsk5_114", "hsk5_113"], lesson: 7 },
    { id: "s5_019", character: "樂觀的人更容易面對挑戰。", pinyin: "lè guān de rén gèng róng yì miàn duì tiǎo zhàn", english: "Optimistic people find it easier to face challenges.", vocabIds: ["hsk5_119"], lesson: 7 },

    // Lesson 8
    { id: "s5_020", character: "現代人的生活方式越來越多元。", pinyin: "xiàn dài rén de shēng huó fāng shì yuè lái yuè duō yuán", english: "Modern people's lifestyles are becoming more and more diverse.", vocabIds: ["hsk5_121", "hsk5_130"], lesson: 8 },
    { id: "s5_021", character: "科技的發展為我們提供了便利。", pinyin: "kē jì de fā zhǎn wèi wǒ men tí gōng le biàn lì", english: "The development of technology provides us with convenience.", vocabIds: ["hsk5_126", "hsk5_127"], lesson: 8 },

    // Lesson 9
    { id: "s5_022", character: "這部文學作品富有藝術美學。", pinyin: "zhè bù wén xué zuò pǐn fù yǒu yì shù měi xué", english: "This literary work is rich in artistic aesthetics.", vocabIds: ["hsk5_132", "hsk5_131", "hsk5_133"], lesson: 9 },
    { id: "s5_023", character: "藝術家從傳統文化中獲取靈感。", pinyin: "yì shù jiā cóng chuán tǒng wén huà zhōng huò qǔ líng gǎn", english: "Artists get inspiration from traditional culture.", vocabIds: ["hsk5_137", "hsk5_136"], lesson: 9 },

    // Lesson 10
    { id: "s5_024", character: "這項實驗需要非常精確的數據。", pinyin: "zhè xiàng shí yàn xū yào fēi cháng jīng què de shù jù", english: "This experiment requires very precise data.", vocabIds: ["hsk5_146", "hsk5_149"], lesson: 10 },
    { id: "s5_025", character: "邏輯分析是科學研究的基礎。", pinyin: "luó jí fēn xī shì kē xué yán jiū de jī chǔ", english: "Logical analysis is the basis of scientific research.", vocabIds: ["hsk5_142", "hsk5_143", "hsk5_141"], lesson: 10 },

    // Lesson 11
    { id: "s5_026", character: "他在面試中表現得很出色。", pinyin: "tā zài miàn shì zhōng biǎo xiàn dé hěn chū sè", english: "He performed very well in the interview.", vocabIds: ["hsk5_154"], lesson: 11 },
    { id: "s5_027", character: "公司為員工提供了良好的福利。", pinyin: "gōng sī wèi yuán gōng tí gōng le liáng hǎo de fú lì", english: "The company provides good benefits for its employees.", vocabIds: ["hsk5_157"], lesson: 11 },

    // Lesson 12
    { id: "s5_028", character: "全球化促進了不同文化的融合。", pinyin: "quán qiú huà cù jìn le bù tóng wén huà de róng hé", english: "Globalization promotes the fusion of different cultures.", vocabIds: ["hsk5_161", "hsk5_170"], lesson: 12 },
    { id: "s5_029", character: "我們應該包容不同背景的人。", pinyin: "wǒ men yīng gāi bāo róng bù tóng bèi jǐng de rén", english: "We should be inclusive of people from different backgrounds.", vocabIds: ["hsk5_165", "hsk5_169"], lesson: 12 },

    // Lesson 13
    { id: "s5_030", character: "預防疾病比治療更重要。", pinyin: "yù fáng jí bìng bǐ zhì liáo gèng zhòng yào", english: "Preventing disease is more important than treatment.", vocabIds: ["hsk5_175", "hsk5_176"], lesson: 13 },
    { id: "s5_031", character: "他正在恢復中，症狀已經減輕。", pinyin: "tā zhèng zài huī fù zhōng, zhèng zhuàng yǐ jīng jiǎn qīng", english: "He is recovering, and his symptoms have eased.", vocabIds: ["hsk5_180", "hsk5_174"], lesson: 13 },

    // Lesson 14
    { id: "s5_032", character: "每個人都有不同的人生觀點。", pinyin: "měi gè rén dōu yǒu bù tóng de rén shēng guān diǎn", english: "Everyone has different viewpoints on life.", vocabIds: ["hsk5_190", "hsk5_189"], lesson: 14 },
    { id: "s5_033", character: "思考問題的本質需要智慧。", pinyin: "sī kǎo wèn tí de běn zhí xū yào zhì huì", english: "Reflecting on the essence of a problem requires wisdom.", vocabIds: ["hsk5_187", "hsk5_188", "hsk5_186"], lesson: 14 },

    // Lesson 15
    { id: "s5_034", character: "他對這項技術有深刻的體會。", pinyin: "tā duì zhè xiàng jì shù yǒu shēn kè de tǐ huì", english: "He has a profound understanding of this technology from experience.", vocabIds: ["hsk5_192", "hsk5_191"], lesson: 15 },
    { id: "s5_035", character: "他已經熟練掌握了多種語言。", pinyin: "tā yǐ jīng shú liàn zhǎng wò le duō zhǒng yǔ yán", english: "He has skillfully mastered multiple languages.", vocabIds: ["hsk5_194", "hsk5_197"], lesson: 15 },
];

export default hsk5Sentences;
export function getSentencesByLesson(lessonNum) {
    return hsk5Sentences.filter(s => s.lesson === lessonNum);
}
