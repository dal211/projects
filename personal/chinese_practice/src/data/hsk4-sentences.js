// HSK 4 Sentences — Taiwan Mandarin
// Lessons 1-5

const hsk4Sentences = [
    // Lesson 1
    { id: "s4_001", character: "我不能保證一定會成功。", pinyin: "wǒ bù néng bǎo zhèng yí dìng hui chéng gōng", english: "I cannot guarantee success.", vocabIds: ["hsk4_010"], lesson: 1 },
    { id: "s4_002", character: "請按照這個標準做。", pinyin: "qǐng àn zhào zhè ge biāo zhǔn zuò", english: "Please do it according to this standard.", vocabIds: ["hsk4_005", "hsk4_020"], lesson: 1 },
    { id: "s4_003", character: "你的漢語真棒！", pinyin: "nǐ de hàn yǔ zhēn bàng", english: "Your Chinese is really great!", vocabIds: ["hsk4_007"], lesson: 1 },

    // Lesson 2
    { id: "s4_004", character: "請填一下這張表格。", pinyin: "qǐng tián yí xià zhè zhāng biǎo gé", english: "Please fill out this form.", vocabIds: ["hsk4_021"], lesson: 2 },
    { id: "s4_005", character: "不管你去不去，我都會去。", pinyin: "bù guǎn nǐ qù bú qù, wǒ dōu huì qù", english: "No matter if you go or not, I will go.", vocabIds: ["hsk4_030"], lesson: 2 },
    { id: "s4_006", character: "他不僅會唱歌，還會跳舞。", pinyin: "tā bù jǐn huì chàng gē, hái huì tiào wǔ", english: "He not only can sing but also dance.", vocabIds: ["hsk4_032"], lesson: 2 },

    // Lesson 3
    { id: "s4_007", character: "你猜我看到了誰？", pinyin: "nǐ cāi wǒ kàn dào le shéi", english: "Guess who I saw?", vocabIds: ["hsk4_035"], lesson: 3 },
    { id: "s4_008", character: "廚房裡非常乾淨。", pinyin: "chú fáng lǐ fēi cháng gān jìng", english: "The kitchen is very clean.", vocabIds: ["hsk4_057"], lesson: 3 },
    { id: "s4_009", character: "這件事讓我大吃一驚。", pinyin: "zhè jiàn shì ràng wǒ dà chī yì jīng", english: "This matter shocked me greatly.", vocabIds: ["hsk4_051"], lesson: 3 },

    // Lesson 4
    { id: "s4_010", character: "我從來沒見過他。", pinyin: "wǒ cóng lái méi jiàn guo tā", english: "I have never seen him.", vocabIds: ["hsk4_061"], lesson: 4 },
    { id: "s4_011", character: "別打擾他寫作業。", pinyin: "bié dǎ rǎo tā xiě zuò yè", english: "Don't disturb him doing homework.", vocabIds: ["hsk4_067"], lesson: 4 },
    { id: "s4_012", character: "現在大約三點。", pinyin: "xiàn zài dà yuē sān diǎn", english: "It's approximately three o'clock now.", vocabIds: ["hsk4_074"], lesson: 4 },

    // Lesson 5
    { id: "s4_013", character: "今天路上堵車了。", pinyin: "jīn tiān lù shang dǔ chē le", english: "There was a traffic jam on the road today.", vocabIds: ["hsk4_098"], lesson: 5 },
    { id: "s4_014", character: "你的登機牌在哪裡？", pinyin: "nǐ de dēng jī pái zài nǎ lǐ", english: "Where is your boarding pass?", vocabIds: ["hsk4_087"], lesson: 5 },
    { id: "s4_015", character: "他不得不離開這裡。", pinyin: "tā bù dé bù lí kāi zhè lǐ", english: "He had no choice but to leave here.", vocabIds: ["hsk4_029"], lesson: 5 },

    // Lesson 6
    { id: "s4_016", character: "我的電腦密碼忘記了。", pinyin: "wǒ de diàn nǎo mì mǎ wàng jì le", english: "I forgot my computer password.", vocabIds: ["hsk4_101", "hsk4_107"], lesson: 6 },
    { id: "s4_017", character: "這款軟體很容易使用。", pinyin: "zhè kuǎn ruǎn tǐ hěn róng yì shǐ yòng", english: "This software is very easy to use.", vocabIds: ["hsk4_105"], lesson: 6 },

    // Lesson 7
    { id: "s4_018", character: "我們必須保護大自然。", pinyin: "wǒ men bì xū bǎo hù dà zì rán", english: "We must protect nature.", vocabIds: ["hsk4_115", "hsk4_116"], lesson: 7 },
    { id: "s4_019", character: "減少垃圾對環境有好處。", pinyin: "jiǎn shǎo lè sè duì huán jìng yǒu hǎo chù", english: "Reducing garbage is good for the environment.", vocabIds: ["hsk4_119", "hsk4_111"], lesson: 7 },

    // Lesson 8
    { id: "s4_020", character: "他在這門專業很有研究。", pinyin: "tā zài zhè mén zhuān yè hěn yǒu yán jiū", english: "He has done a lot of research in this major.", vocabIds: ["hsk4_123", "hsk4_125"], lesson: 8 },
    { id: "s4_021", character: "他申請到了全額獎學金。", pinyin: "tā shēn qǐng dào le quán é jiǎng xué jīn", english: "He applied for and received a full scholarship.", vocabIds: ["hsk4_127", "hsk4_126"], lesson: 8 },

    // Lesson 9
    { id: "s4_022", character: "每個人都應該遵守法律。", pinyin: "měi gè rén dōu yīng gāi zūn shǒu fǎ lǜ", english: "Everyone should obey the law.", vocabIds: ["hsk4_137", "hsk4_131"], lesson: 9 },
    { id: "s4_023", character: "律師在法庭上提出了證據。", pinyin: "lǜ shī zài fǎ tíng shàng tí chū le zhèng jù", english: "The lawyer presented evidence in court.", vocabIds: ["hsk4_134", "hsk4_139"], lesson: 9 },

    // Lesson 10
    { id: "s4_024", character: "失業是目前嚴重的社會問題。", pinyin: "shī yè shì mù qián yán zhòng de shè huì wèn tí", english: "Unemployment is a serious social problem at present.", vocabIds: ["hsk4_142", "hsk4_141"], lesson: 10 },
    { id: "s4_025", character: "這家保險公司發展很快。", pinyin: "zhè jiā bǎo xiǎn gōng sī fā zhǎn hěn kuài", english: "This insurance company is developing very fast.", vocabIds: ["hsk4_147", "hsk4_144"], lesson: 10 },

    // Lesson 11
    { id: "s4_026", character: "我們的目的地是那座山。", pinyin: "wǒ men de mù dì dì shì nà zuò shān", english: "Our destination is that mountain.", vocabIds: ["hsk4_151"], lesson: 11 },
    { id: "s4_027", character: "我需要辦理簽證才能旅行。", pinyin: "wǒ xū yào bàn lǐ qiān zhèng cái néng lǚ xíng", english: "I need to get a visa to travel.", vocabIds: ["hsk4_159"], lesson: 11 },

    // Lesson 12
    { id: "s4_028", character: "他在看晚間新聞節目。", pinyin: "tā zài kàn wǎn jiān xīn wén jié mù", english: "He is watching the evening news program.", vocabIds: ["hsk4_161", "hsk4_162"], lesson: 12 },
    { id: "s4_029", character: "這條廣告流行了很久。", pinyin: "zhè tiáo guǎng gào liú xíng le hěn jiǔ", english: "This advertisement has been popular for a long time.", vocabIds: ["hsk4_164", "hsk4_165"], lesson: 12 },

    // Lesson 13
    { id: "s4_030", character: "這個發明改變了科學界。", pinyin: "zhè ge fā míng gǎi biàn le kē xué jiè", english: "This invention changed the scientific world.", vocabIds: ["hsk4_172", "hsk4_171"], lesson: 13 },
    { id: "s4_031", character: "他們在實驗室有很多發現。", pinyin: "tā men zài shí yàn shì yǒu hěn duō fā xiàn", english: "They have had many discoveries in the laboratory.", vocabIds: ["hsk4_173", "hsk4_174"], lesson: 13 },

    // Lesson 14
    { id: "s4_032", character: "市場競爭非常激烈。", pinyin: "shì chǎng jìng zhēng fēi cháng jī liè", english: "Market competition is very intense.", vocabIds: ["hsk4_182", "hsk4_187"], lesson: 14 },
    { id: "s4_033", character: "這家公司的利潤增加了。", pinyin: "zhè jiā gōng sī de lì rùn zēng jiā le", english: "The profit of this company has increased.", vocabIds: ["hsk4_186"], lesson: 14 },

    // Lesson 15
    { id: "s4_034", character: "我對達成目標很有信心。", pinyin: "wǒ duì dá chéng mù biāo hěn yǒu xìn xīn", english: "I am confident in achieving the goal.", vocabIds: ["hsk4_200", "hsk4_195", "hsk4_197"], lesson: 15 },
    { id: "s4_035", character: "這是一個巨大的挑戰。", pinyin: "zhè shì yí ge jù dà de tiǎo zhàn", english: "This is a huge challenge.", vocabIds: ["hsk4_196"], lesson: 15 },
];

export default hsk4Sentences;
export function getSentencesByLesson(lessonNum) {
    return hsk4Sentences.filter(s => s.lesson === lessonNum);
}
