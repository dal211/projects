// HSK 1 Sentences — Taiwan Mandarin (Traditional Characters)
// Each sentence uses vocabulary from its lesson and prior lessons

const hsk1Sentences = [
    // ─── Lesson 1: Greetings ───
    { id: "s001", character: "你好！", pinyin: "nǐ hǎo!", english: "Hello!", words: ["hsk1_003"], lesson: 1 },
    { id: "s002", character: "你好嗎？", pinyin: "nǐ hǎo ma?", english: "How are you?", words: ["hsk1_001", "hsk1_002", "hsk1_106"], lesson: 1 },
    { id: "s003", character: "我很好，謝謝！", pinyin: "wǒ hěn hǎo, xiè xie!", english: "I'm very well, thank you!", words: ["hsk1_004", "hsk1_002", "hsk1_007"], lesson: 1 },
    { id: "s004", character: "再見！", pinyin: "zài jiàn!", english: "Goodbye!", words: ["hsk1_008"], lesson: 1 },
    { id: "s005", character: "對不起！", pinyin: "duì bu qǐ!", english: "Sorry!", words: ["hsk1_009"], lesson: 1 },

    // ─── Lesson 2: Introductions ───
    { id: "s006", character: "你叫什麼名字？", pinyin: "nǐ jiào shén me míng zi?", english: "What is your name?", words: ["hsk1_001", "hsk1_090", "hsk1_019", "hsk1_020"], lesson: 2 },
    { id: "s007", character: "我叫小明。", pinyin: "wǒ jiào xiǎo míng.", english: "My name is Xiao Ming.", words: ["hsk1_004", "hsk1_090"], lesson: 2 },
    { id: "s008", character: "他是我的朋友。", pinyin: "tā shì wǒ de péng yǒu.", english: "He is my friend.", words: ["hsk1_011", "hsk1_005", "hsk1_004", "hsk1_037"], lesson: 2 },
    { id: "s009", character: "她是誰？", pinyin: "tā shì shuí?", english: "Who is she?", words: ["hsk1_012", "hsk1_005", "hsk1_018"], lesson: 2 },
    { id: "s010", character: "這是什麼？", pinyin: "zhè shì shén me?", english: "What is this?", words: ["hsk1_016", "hsk1_005", "hsk1_019"], lesson: 2 },

    // ─── Lesson 3: Numbers ───
    { id: "s011", character: "一、二、三", pinyin: "yī, èr, sān", english: "one, two, three", words: ["hsk1_021", "hsk1_022", "hsk1_023"], lesson: 3 },
    { id: "s012", character: "我有三個朋友。", pinyin: "wǒ yǒu sān gè péng yǒu.", english: "I have three friends.", words: ["hsk1_004", "hsk1_085", "hsk1_023", "hsk1_037"], lesson: 3 },
    { id: "s013", character: "你幾歲？", pinyin: "nǐ jǐ suì?", english: "How old are you?", words: ["hsk1_001", "hsk1_124", "hsk1_139"], lesson: 3 },
    { id: "s014", character: "我十八歲。", pinyin: "wǒ shí bā suì.", english: "I am eighteen years old.", words: ["hsk1_004", "hsk1_030", "hsk1_028", "hsk1_139"], lesson: 3 },

    // ─── Lesson 4: Family ───
    { id: "s015", character: "我的家有五個人。", pinyin: "wǒ de jiā yǒu wǔ gè rén.", english: "My family has five people.", words: ["hsk1_004", "hsk1_031", "hsk1_085", "hsk1_025", "hsk1_032"], lesson: 4 },
    { id: "s016", character: "她是我媽媽。", pinyin: "tā shì wǒ mā ma.", english: "She is my mom.", words: ["hsk1_012", "hsk1_005", "hsk1_004", "hsk1_034"], lesson: 4 },
    { id: "s017", character: "我的老師很好。", pinyin: "wǒ de lǎo shī hěn hǎo.", english: "My teacher is very good.", words: ["hsk1_004", "hsk1_039", "hsk1_002"], lesson: 4 },
    { id: "s018", character: "他是學生。", pinyin: "tā shì xué shēng.", english: "He is a student.", words: ["hsk1_011", "hsk1_005", "hsk1_040"], lesson: 4 },

    // ─── Lesson 5: Time ───
    { id: "s019", character: "今天是幾月幾日？", pinyin: "jīn tiān shì jǐ yuè jǐ rì?", english: "What date is today?", words: ["hsk1_044", "hsk1_005", "hsk1_124", "hsk1_042", "hsk1_043"], lesson: 5 },
    { id: "s020", character: "現在幾點？", pinyin: "xiàn zài jǐ diǎn?", english: "What time is it now?", words: ["hsk1_049", "hsk1_124", "hsk1_140"], lesson: 5 },
    { id: "s021", character: "明天見！", pinyin: "míng tiān jiàn!", english: "See you tomorrow!", words: ["hsk1_045"], lesson: 5 },
    { id: "s022", character: "昨天下午你做什麼？", pinyin: "zuó tiān xià wǔ nǐ zuò shén me?", english: "What did you do yesterday afternoon?", words: ["hsk1_046", "hsk1_048", "hsk1_001", "hsk1_059", "hsk1_019"], lesson: 5 },

    // ─── Lesson 6: Daily Activities ───
    { id: "s023", character: "你想吃什麼？", pinyin: "nǐ xiǎng chī shén me?", english: "What do you want to eat?", words: ["hsk1_001", "hsk1_081", "hsk1_051", "hsk1_019"], lesson: 6 },
    { id: "s024", character: "我想喝茶。", pinyin: "wǒ xiǎng hē chá.", english: "I want to drink tea.", words: ["hsk1_004", "hsk1_081", "hsk1_052", "hsk1_064"], lesson: 6 },
    { id: "s025", character: "他在看書。", pinyin: "tā zài kàn shū.", english: "He is reading a book.", words: ["hsk1_011", "hsk1_074", "hsk1_054", "hsk1_129"], lesson: 6 },
    { id: "s026", character: "我每天都工作。", pinyin: "wǒ měi tiān dōu gōng zuò.", english: "I work every day.", words: ["hsk1_004", "hsk1_108", "hsk1_060"], lesson: 6 },

    // ─── Lesson 7: Food ───
    { id: "s027", character: "這個菜很好吃。", pinyin: "zhè gè cài hěn hǎo chī.", english: "This dish is very delicious.", words: ["hsk1_016", "hsk1_123", "hsk1_062", "hsk1_070"], lesson: 7 },
    { id: "s028", character: "請給我一杯水。", pinyin: "qǐng gěi wǒ yì bēi shuǐ.", english: "Please give me a glass of water.", words: ["hsk1_131", "hsk1_004", "hsk1_067", "hsk1_063"], lesson: 7 },
    { id: "s029", character: "你喜歡吃水果嗎？", pinyin: "nǐ xǐ huān chī shuǐ guǒ ma?", english: "Do you like eating fruit?", words: ["hsk1_001", "hsk1_087", "hsk1_051", "hsk1_065", "hsk1_106"], lesson: 7 },
    { id: "s030", character: "我們去餐廳吃飯。", pinyin: "wǒ men qù cān tīng chī fàn.", english: "Let's go to a restaurant to eat.", words: ["hsk1_014", "hsk1_071", "hsk1_069", "hsk1_051", "hsk1_061"], lesson: 7 },

    // ─── Lesson 8: Places ───
    { id: "s031", character: "你要去哪裡？", pinyin: "nǐ yào qù nǎ lǐ?", english: "Where do you want to go?", words: ["hsk1_001", "hsk1_084", "hsk1_071", "hsk1_075"], lesson: 8 },
    { id: "s032", character: "我要去學校。", pinyin: "wǒ yào qù xué xiào.", english: "I'm going to school.", words: ["hsk1_004", "hsk1_084", "hsk1_071", "hsk1_078"], lesson: 8 },
    { id: "s033", character: "商店在哪裡？", pinyin: "shāng diàn zài nǎ lǐ?", english: "Where is the store?", words: ["hsk1_080", "hsk1_074", "hsk1_075"], lesson: 8 },
    { id: "s034", character: "他回家了。", pinyin: "tā huí jiā le.", english: "He went home.", words: ["hsk1_011", "hsk1_073", "hsk1_031", "hsk1_104"], lesson: 8 },

    // ─── Lesson 9: Common Verbs ───
    { id: "s035", character: "你會說中文嗎？", pinyin: "nǐ huì shuō zhōng wén ma?", english: "Can you speak Chinese?", words: ["hsk1_001", "hsk1_082", "hsk1_056", "hsk1_137", "hsk1_106"], lesson: 9 },
    { id: "s036", character: "我想買這個。", pinyin: "wǒ xiǎng mǎi zhè gè.", english: "I want to buy this.", words: ["hsk1_004", "hsk1_081", "hsk1_089", "hsk1_016", "hsk1_123"], lesson: 9 },
    { id: "s037", character: "我也喜歡。", pinyin: "wǒ yě xǐ huān.", english: "I also like it.", words: ["hsk1_004", "hsk1_100", "hsk1_087"], lesson: 9 },
    { id: "s038", character: "我沒有錢。", pinyin: "wǒ méi yǒu qián.", english: "I don't have money.", words: ["hsk1_004", "hsk1_086", "hsk1_121"], lesson: 9 },

    // ─── Lesson 10: Descriptions ───
    { id: "s039", character: "這個太大了。", pinyin: "zhè gè tài dà le.", english: "This is too big.", words: ["hsk1_016", "hsk1_123", "hsk1_103", "hsk1_091", "hsk1_104"], lesson: 10 },
    { id: "s040", character: "今天很冷。", pinyin: "jīn tiān hěn lěng.", english: "It's very cold today.", words: ["hsk1_044", "hsk1_099", "hsk1_095"], lesson: 10 },
    { id: "s041", character: "她很漂亮。", pinyin: "tā hěn piào liàng.", english: "She is very beautiful.", words: ["hsk1_012", "hsk1_099", "hsk1_098"], lesson: 10 },
    { id: "s042", character: "我很高興認識你。", pinyin: "wǒ hěn gāo xìng rèn shi nǐ.", english: "I'm very glad to meet you.", words: ["hsk1_004", "hsk1_099", "hsk1_097", "hsk1_134", "hsk1_001"], lesson: 10 },

    // ─── Lesson 11: Particles & Questions ───
    { id: "s043", character: "今天天氣怎麼樣？", pinyin: "jīn tiān tiān qì zěn me yàng?", english: "How is the weather today?", words: ["hsk1_044", "hsk1_101", "hsk1_109"], lesson: 11 },
    { id: "s044", character: "你呢？", pinyin: "nǐ ne?", english: "And you?", words: ["hsk1_001", "hsk1_107"], lesson: 11 },
    { id: "s045", character: "你為什麼不吃？", pinyin: "nǐ wèi shén me bù chī?", english: "Why aren't you eating?", words: ["hsk1_001", "hsk1_110", "hsk1_006", "hsk1_051"], lesson: 11 },
    { id: "s046", character: "我們都是學生。", pinyin: "wǒ men dōu shì xué shēng.", english: "We are all students.", words: ["hsk1_014", "hsk1_108", "hsk1_005", "hsk1_040"], lesson: 11 },

    // ─── Lesson 12: Transport ───
    { id: "s047", character: "我要坐計程車。", pinyin: "wǒ yào zuò jì chéng chē.", english: "I want to take a taxi.", words: ["hsk1_004", "hsk1_084", "hsk1_118", "hsk1_112"], lesson: 12 },
    { id: "s048", character: "你住在哪裡？", pinyin: "nǐ zhù zài nǎ lǐ?", english: "Where do you live?", words: ["hsk1_001", "hsk1_119", "hsk1_074", "hsk1_075"], lesson: 12 },
    { id: "s049", character: "商店在前面。", pinyin: "shāng diàn zài qián miàn.", english: "The store is ahead.", words: ["hsk1_080", "hsk1_074", "hsk1_113"], lesson: 12 },
    { id: "s050", character: "我們走吧。", pinyin: "wǒ men zǒu ba.", english: "Let's go.", words: ["hsk1_014", "hsk1_120"], lesson: 12 },

    // ─── Lesson 13: Shopping ───
    { id: "s051", character: "這個多少錢？", pinyin: "zhè gè duō shǎo qián?", english: "How much is this?", words: ["hsk1_016", "hsk1_123", "hsk1_125", "hsk1_121"], lesson: 13 },
    { id: "s052", character: "我要買一本書。", pinyin: "wǒ yào mǎi yì běn shū.", english: "I want to buy a book.", words: ["hsk1_004", "hsk1_084", "hsk1_089", "hsk1_128", "hsk1_129"], lesson: 13 },
    { id: "s053", character: "你有幾個東西？", pinyin: "nǐ yǒu jǐ gè dōng xi?", english: "How many things do you have?", words: ["hsk1_001", "hsk1_085", "hsk1_124", "hsk1_123", "hsk1_126"], lesson: 13 },
    { id: "s054", character: "十塊錢。", pinyin: "shí kuài qián.", english: "Ten dollars.", words: ["hsk1_030", "hsk1_122", "hsk1_121"], lesson: 13 },

    // ─── Lesson 14: Conversation ───
    { id: "s055", character: "請問，你會說中文嗎？", pinyin: "qǐng wèn, nǐ huì shuō zhōng wén ma?", english: "Excuse me, can you speak Chinese?", words: ["hsk1_132", "hsk1_001", "hsk1_082", "hsk1_056", "hsk1_137", "hsk1_106"], lesson: 14 },
    { id: "s056", character: "我在學中文。", pinyin: "wǒ zài xué zhōng wén.", english: "I am studying Chinese.", words: ["hsk1_004", "hsk1_074", "hsk1_138", "hsk1_137"], lesson: 14 },
    { id: "s057", character: "你知道嗎？", pinyin: "nǐ zhī dào ma?", english: "Do you know?", words: ["hsk1_001", "hsk1_133", "hsk1_106"], lesson: 14 },
    { id: "s058", character: "我打電話給你。", pinyin: "wǒ dǎ diàn huà gěi nǐ.", english: "I'll call you.", words: ["hsk1_004", "hsk1_135", "hsk1_001"], lesson: 14 },

    // ─── Lesson 15: Putting It All Together ───
    { id: "s059", character: "不客氣！", pinyin: "bú kè qì!", english: "You're welcome!", words: ["hsk1_146"], lesson: 15 },
    { id: "s060", character: "太好了！", pinyin: "tài hǎo le!", english: "Great!", words: ["hsk1_147"], lesson: 15 },
    { id: "s061", character: "先生，可以嗎？", pinyin: "xiān shēng, kě yǐ ma?", english: "Sir, is that okay?", words: ["hsk1_144", "hsk1_148", "hsk1_106"], lesson: 15 },
    { id: "s062", character: "對，我是他的同學。", pinyin: "duì, wǒ shì tā de tóng xué.", english: "Right, I am his classmate.", words: ["hsk1_150", "hsk1_004", "hsk1_005", "hsk1_011", "hsk1_038"], lesson: 15 },
];

export default hsk1Sentences;

export function getSentencesByLesson(lessonNum) {
    return hsk1Sentences.filter(s => s.lesson === lessonNum);
}
