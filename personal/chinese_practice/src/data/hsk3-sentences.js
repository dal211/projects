// HSK 3 Sentences — Taiwan Mandarin
// Complete List (Lessons 1-20)

const hsk3Sentences = [
    // Lesson 1
    { id: "s3_001", character: "週末你有什麼打算？", pinyin: "zhōu mò nǐ yǒu shén me dǎ suàn", english: "What are your plans for the weekend?", vocabIds: ["hsk3_001", "hsk3_002"], lesson: 1 },
    { id: "s3_002", character: "我一直想去南方旅遊。", pinyin: "wǒ yì zhí xiǎng qù nán fāng lǚ yóu", english: "I've always wanted to travel south.", vocabIds: ["hsk3_004", "hsk3_009", "hsk2_148"], lesson: 1 },
    { id: "s3_003", character: "這張地圖是哪裡買的？", pinyin: "zhè zhāng dì tú shì nǎ lǐ mǎi de", english: "Where was this map bought?", vocabIds: ["hsk3_013"], lesson: 1 },

    // Lesson 2
    { id: "s3_004", character: "我的腿有點疼。", pinyin: "wǒ de tuǐ yǒu diǎn téng", english: "My leg hurts a little.", vocabIds: ["hsk3_015", "hsk3_016"], lesson: 2 },
    { id: "s3_005", character: "這件事其實不難。", pinyin: "zhè jiàn shì qí shí bù nán", english: "This matter is actually not difficult.", vocabIds: ["hsk3_031", "hsk3_020"], lesson: 2 },
    { id: "s3_006", character: "經理在辦公室嗎？", pinyin: "jīng lǐ zài bàn gōng shì ma", english: "Is the manager in the office?", vocabIds: ["hsk3_023", "hsk3_024"], lesson: 2 },

    // Lesson 3
    { id: "s3_007", character: "你喝茶還是喝咖啡？", pinyin: "nǐ hē chá hái shì hē kā fēi", english: "Do you drink tea or coffee?", vocabIds: ["hsk3_033"], lesson: 3 },
    { id: "s3_008", character: "記得要小心。", pinyin: "jì de yào xiǎo xīn", english: "Remember to be careful.", vocabIds: ["hsk3_037", "hsk3_035"], lesson: 3 },
    { id: "s3_009", character: "這條褲子很舒服。", pinyin: "zhè tiáo kù zi hěn shū fu", english: "These pants are comfortable.", vocabIds: ["hsk3_036", "hsk3_046"], lesson: 3 },

    // Lesson 4
    { id: "s3_010", character: "這個孩子很聰明。", pinyin: "zhè ge hái zi hěn cōng ming", english: "This child is very smart.", vocabIds: ["hsk3_053"], lesson: 4 },
    { id: "s3_011", character: "他對人很熱情。", pinyin: "tā duì rén hěn rè qíng", english: "He is very warm towards people.", vocabIds: ["hsk3_054"], lesson: 4 },
    { id: "s3_012", character: "超市裡有蛋糕。", pinyin: "chāo shì lǐ yǒu dàn gāo", english: "There is cake in the supermarket.", vocabIds: ["hsk3_060", "hsk3_061"], lesson: 4 },

    // Lesson 5
    { id: "s3_013", character: "我最近感冒了。", pinyin: "wǒ zuì jìn gǎn mào le", english: "I caught a cold recently.", vocabIds: ["hsk3_075", "hsk3_069"], lesson: 5 },
    { id: "s3_014", character: "春天來了，草綠了。", pinyin: "chūn tiān lái le, cǎo lǜ le", english: "Spring is here, the grass is green.", vocabIds: ["hsk3_072", "hsk3_077", "hsk3_048"], lesson: 5 },
    { id: "s3_015", character: "當然可以照顧你。", pinyin: "dāng rán kě yǐ zhào gù nǐ", english: "Of course I can take care of you.", vocabIds: ["hsk3_071", "hsk3_067"], lesson: 5 },

    // Lesson 6
    { id: "s3_016", character: "這個畫很簡單。", pinyin: "zhè ge huà hěn jiǎn dān", english: "This drawing is very simple.", vocabIds: ["hsk3_079", "hsk3_078"], lesson: 6 },
    { id: "s3_017", character: "他一邊聽音樂一邊寫字。", pinyin: "tā yì biān tīng yīn yuè yì biān xiě zì", english: "He listens to music while writing.", vocabIds: ["hsk3_082"], lesson: 6 },
    { id: "s3_018", character: "你的數學怎麼樣？", pinyin: "nǐ de shù xué zěn me yàng", english: "How is your math?", vocabIds: ["hsk3_083"], lesson: 6 },

    // Lesson 7
    { id: "s3_019", character: "我們明天見面。", pinyin: "wǒ men míng tiān jiàn miàn", english: "We will meet tomorrow.", vocabIds: ["hsk3_084"], lesson: 7 },
    { id: "s3_020", character: "過去的事情別說了。", pinyin: "guò qù de shì qing bié shuō le", english: "Don't speak of past matters.", vocabIds: ["hsk3_085", "hsk2_141"], lesson: 7 },
    { id: "s3_021", character: "你變了很多。", pinyin: "nǐ biàn le hěn duō", english: "You have changed a lot.", vocabIds: ["hsk3_086"], lesson: 7 },

    // Lesson 8
    { id: "s3_022", character: "對不起，我遲到了。", pinyin: "duì bu qǐ, wǒ chí dào le", english: "Sorry, I am late.", vocabIds: ["hsk3_089"], lesson: 8 },
    { id: "s3_023", character: "這是個大問題。", pinyin: "zhè shì ge dà wèn tí", english: "This is a big problem.", vocabIds: ["hsk3_091"], lesson: 8 },
    { id: "s3_024", character: "我們習慣早起。", pinyin: "wǒ men xí guàn zǎo qǐ", english: "We are used to getting up early.", vocabIds: ["hsk3_090"], lesson: 8 },

    // Lesson 9
    { id: "s3_025", character: "這裡的環境很好。", pinyin: "zhè lǐ de huán jìng hěn hǎo", english: "The environment here is very good.", vocabIds: ["hsk3_094"], lesson: 9 },
    { id: "s3_026", character: "我在路口等你。", pinyin: "wǒ zài lù kǒu děng nǐ", english: "I will wait for you at the intersection.", vocabIds: ["hsk3_096", "hsk2_128"], lesson: 9 },
    { id: "s3_027", character: "太陽從東邊出來。", pinyin: "tài yáng cóng dōng bian chū lái", english: "The sun rises from the east.", vocabIds: ["hsk3_097"], lesson: 9 },

    // Lesson 10
    { id: "s3_028", character: "我很喜歡中國文化。", pinyin: "wǒ hěn xǐ huān zhōng guó wén huà", english: "I really like Chinese culture.", vocabIds: ["hsk3_102"], lesson: 10 },
    { id: "s3_029", character: "這個故事很有名。", pinyin: "zhè ge gù shi hěn yǒu míng", english: "This story is very famous.", vocabIds: ["hsk3_100", "hsk3_101"], lesson: 10 },
    { id: "s3_030", character: "你知道這段歷史嗎？", pinyin: "nǐ zhī dào zhè duàn lì shǐ ma", english: "Do you know this history?", vocabIds: ["hsk3_099"], lesson: 10 },

    // Lesson 11
    { id: "s3_031", character: "我們正在開會。", pinyin: "wǒ men zhèng zài kāi huì", english: "We are having a meeting.", vocabIds: ["hsk3_104"], lesson: 11 },
    { id: "s3_032", character: "別忘記去圖書館。", pinyin: "bié wàng jì qù tú shū guǎn", english: "Don't forget to go to the library.", vocabIds: ["hsk3_106", "hsk3_107"], lesson: 11 },
    { id: "s3_033", character: "這本書是我借的。", pinyin: "zhè běn shū shì wǒ jiè de", english: "I borrowed this book.", vocabIds: ["hsk3_108"], lesson: 11 },

    // Lesson 12
    { id: "s3_034", character: "冬天下雪嗎？", pinyin: "dōng tiān xià xuě ma", english: "Does it snow in winter?", vocabIds: ["hsk3_110", "hsk2_061"], lesson: 12 },
    { id: "s3_035", character: "外麵刮風了。", pinyin: "wài miàn guā fēng le", english: "It's windy outside.", vocabIds: ["hsk3_111"], lesson: 12 },
    { id: "s3_036", character: "祝你身體健康。", pinyin: "zhù nǐ shēn tǐ jiàn kāng", english: "Wish you good health.", vocabIds: ["hsk3_113"], lesson: 12 },

    // Lesson 13
    { id: "s3_037", character: "你有別的選擇嗎？", pinyin: "nǐ yǒu bié de xuǎn zé ma", english: "Do you have other choices?", vocabIds: ["hsk3_114"], lesson: 13 },
    { id: "s3_038", character: "這兒上網很方便。", pinyin: "zhè r shàng wǎng hěn fāng biàn", english: "It is very convenient to go online here.", vocabIds: ["hsk3_117", "hsk3_118"], lesson: 13 },
    { id: "s3_039", character: "請注意安全。", pinyin: "qǐng zhù yì ān quán", english: "Please pay attention to safety.", vocabIds: ["hsk3_116"], lesson: 13 },

    // Lesson 14
    { id: "s3_040", character: "今天是什麼節日？", pinyin: "jīn tiān shì shén me jié rì", english: "What holiday is today?", vocabIds: ["hsk3_119"], lesson: 14 },
    { id: "s3_041", character: "歡迎大家參加。", pinyin: "huān yíng dà jiā cān jiā", english: "Welcome everyone to participate.", vocabIds: ["hsk3_123", "hsk3_121"], lesson: 14 },
    { id: "s3_042", character: "街道很乾淨。", pinyin: "jiē dào hěn gān jìng", english: "The street is very clean.", vocabIds: ["hsk3_122", "hsk3_130"], lesson: 14 },

    // Lesson 15
    { id: "s3_043", character: "我喜歡小動物。", pinyin: "wǒ xǐ huān xiǎo dòng wù", english: "I like small animals.", vocabIds: ["hsk3_124"], lesson: 15 },
    { id: "s3_044", character: "你看過大熊貓嗎？", pinyin: "nǐ kàn guo dà xióng māo ma", english: "Have you seen a giant panda?", vocabIds: ["hsk3_127"], lesson: 15 },
    { id: "s3_045", character: "天上有月亮。", pinyin: "tiān shàng yǒu yuè liang", english: "There is a moon in the sky.", vocabIds: ["hsk3_128"], lesson: 15 },

    // Lesson 16
    { id: "s3_046", character: "我先洗澡，再刷牙。", pinyin: "wǒ xiān xǐ zǎo, zài shuā yá", english: "I shower first, then brush teeth.", vocabIds: ["hsk3_131", "hsk3_132"], lesson: 16 },
    { id: "s3_047", character: "請把燈關了。", pinyin: "qǐng bǎ dēng guān le", english: "Please turn off the light.", vocabIds: ["hsk3_133"], lesson: 16 },
    { id: "s3_048", character: "房間打掃乾淨了。", pinyin: "fáng jiān dǎ sǎo gān jìng le", english: "The room has been cleaned.", vocabIds: ["hsk3_129", "hsk3_130"], lesson: 16 },

    // Lesson 17
    { id: "s3_049", character: "我完全同意你的看法。", pinyin: "wǒ wán quán tóng yì nǐ de kàn fǎ", english: "I completely agree with your view.", vocabIds: ["hsk3_138", "hsk3_135", "hsk3_137"], lesson: 17 },
    { id: "s3_050", character: "你認為怎麼樣？", pinyin: "nǐ rèn wéi zěn me yàng", english: "What do you think?", vocabIds: ["hsk3_134"], lesson: 17 },
    { id: "s3_051", character: "沒人反對這個計劃。", pinyin: "méi rén fǎn duì zhè ge jì huà", english: "No one opposes this plan.", vocabIds: ["hsk3_136"], lesson: 17 },

    // Lesson 18
    { id: "s3_052", character: "別難過，不要哭。", pinyin: "bié nán guò, bú yào kū", english: "Don't be sad, don't cry.", vocabIds: ["hsk3_142", "hsk3_140"], lesson: 18 },
    { id: "s3_053", character: "看他笑得多開心。", pinyin: "kàn tā xiào de duō kāi xīn", english: "Look how happily he smiles.", vocabIds: ["hsk3_141"], lesson: 18 },
    { id: "s3_054", character: "我很擔心他的健康。", pinyin: "wǒ hěn dān xīn tā de jiàn kāng", english: "I am worried about his health.", vocabIds: ["hsk3_143", "hsk3_113"], lesson: 18 },

    // Lesson 19
    { id: "s3_055", character: "請出示你的護照。", pinyin: "qǐng chū shì nǐ de hù zhào", english: "Please show your passport.", vocabIds: ["hsk3_144"], lesson: 19 },
    { id: "s3_056", character: "我要去銀行換錢。", pinyin: "wǒ yào qù yín háng huàn qián", english: "I want to go to the bank to exchange money.", vocabIds: ["hsk3_147", "hsk3_146"], lesson: 19 },
    { id: "s3_057", character: "這裡可以用信用卡嗎？", pinyin: "zhè lǐ kě yǐ yòng xìn yòng kǎ ma", english: "Can I use a credit card here?", vocabIds: ["hsk3_148"], lesson: 19 },

    // Lesson 20
    { id: "s3_058", character: "我們終於成功了！", pinyin: "wǒ men zhōng yú chéng gōng le", english: "We finally succeeded!", vocabIds: ["hsk3_149", "hsk3_150"], lesson: 20 },
    { id: "s3_059", character: "你的漢語水平提高了很多。", pinyin: "nǐ de hàn yǔ shuǐ píng tí gāo le hěn duō", english: "Your Chinese level has improved a lot.", vocabIds: ["hsk3_152", "hsk3_153"], lesson: 20 },
    { id: "s3_060", character: "祝你取得好成績。", pinyin: "zhù nǐ qǔ dé hǎo chéng jì", english: "Wish you get good grades.", vocabIds: ["hsk3_151"], lesson: 20 },
];

export default hsk3Sentences;

export function getSentencesByLesson(lessonNum) {
    return hsk3Sentences.filter(s => s.lesson === lessonNum);
}
