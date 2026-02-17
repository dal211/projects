// HSK 2 Sentences — Taiwan Mandarin
// Designed to reinforce new vocabulary in context

const hsk2Sentences = [
    // Lesson 1: Travel & Location
    { id: "s2_001", character: "我要去飛機場。", pinyin: "wǒ yào qù fēi jī chǎng", english: "I want to go to the airport.", vocabIds: ["hsk1_084", "hsk2_001"], lesson: 1 },
    { id: "s2_002", character: "火車站在哪裡？", pinyin: "huǒ chē zhàn zài nǎ lǐ", english: "Where is the train station?", vocabIds: ["hsk2_003", "hsk1_075"], lesson: 1 },
    { id: "s2_003", character: "學校離這裡很遠。", pinyin: "xué xiào lí zhè lǐ hěn yuǎn", english: "The school is very far from here.", vocabIds: ["hsk2_009", "hsk2_008"], lesson: 1 },
    { id: "s2_004", character: "請往前面走。", pinyin: "qǐng wǎng qián miàn zǒu", english: "Please walk towards the front.", vocabIds: ["hsk2_010", "hsk1_120"], lesson: 1 },

    // Lesson 2: Daily Routine
    { id: "s2_005", character: "你每天幾點起床？", pinyin: "nǐ měi tiān jǐ diǎn qǐ chuáng", english: "What time do you get up every day?", vocabIds: ["hsk2_019", "hsk2_011"], lesson: 2 },
    { id: "s2_006", character: "我太累了，想休息。", pinyin: "wǒ tài lèi le, xiǎng xiū xi", english: "I am too tired, I want to rest.", vocabIds: ["hsk2_017", "hsk2_018"], lesson: 2 },
    { id: "s2_007", character: "我們準備開始吧。", pinyin: "wǒ men zhǔn bèi kāi shǐ ba", english: "Let's prepare to start.", vocabIds: ["hsk2_015", "hsk2_014"], lesson: 2 },

    // Lesson 3: Colors & Descriptions
    { id: "s2_008", character: "你喜歡什麼顏色？", pinyin: "nǐ xǐ huān shén me yán sè", english: "What color do you like?", vocabIds: ["hsk2_021"], lesson: 3 },
    { id: "s2_009", character: "這件衣服是紅色的。", pinyin: "zhè jiàn yī fu shì hóng sè de", english: "This piece of clothing is red.", vocabIds: ["hsk2_030", "hsk2_022"], lesson: 3 },
    { id: "s2_010", character: "那件白色的很長。", pinyin: "nà jiàn bái sè de hěn cháng", english: "That white one is very long.", vocabIds: ["hsk2_023", "hsk2_027"], lesson: 3 },

    // Lesson 4: Sports & Exercise
    { id: "s2_011", character: "我們去踢足球吧。", pinyin: "wǒ men qù tī zú qiú ba", english: "Let's go play soccer.", vocabIds: ["hsk2_038", "hsk2_039"], lesson: 4 },
    { id: "s2_012", character: "你會不會打籃球？", pinyin: "nǐ huì bú huì dǎ lán qiú", english: "Can you play basketball?", vocabIds: ["hsk2_036", "hsk2_040"], lesson: 4 },
    { id: "s2_013", character: "他每天早上跑步。", pinyin: "tā měi tiān zǎo shang pǎo bù", english: "He runs every morning.", vocabIds: ["hsk2_020", "hsk2_032"], lesson: 4 },

    // Lesson 5: Feelings & Body
    { id: "s2_014", character: "你的眼睛很漂亮。", pinyin: "nǐ de yǎn jing hěn piào liàng", english: "Your eyes are beautiful.", vocabIds: ["hsk2_042", "hsk1_098"], lesson: 5 },
    { id: "s2_015", character: "我不舒服，生病了。", pinyin: "wǒ bù shū fu, shēng bìng le", english: "I'm not comfortable, I got sick.", vocabIds: ["hsk2_045"], lesson: 5 },
    { id: "s2_016", character: "這個藥怎麼吃？", pinyin: "zhè ge yào zěn me chī", english: "How do you take this medicine?", vocabIds: ["hsk2_046"], lesson: 5 },

    // Lesson 6: Food & Service
    { id: "s2_017", character: "服務員，我要咖啡。", pinyin: "fú wù yuán, wǒ yào kā fēi", english: "Waiter, I want coffee.", vocabIds: ["hsk2_051", "hsk2_057"], lesson: 6 },
    { id: "s2_018", character: "羊肉很好吃。", pinyin: "yáng ròu hěn hǎo chī", english: "The lamb is delicious.", vocabIds: ["hsk2_054", "hsk2_053"], lesson: 6 },
    { id: "s2_019", character: "牛奶很便宜。", pinyin: "niú nǎi hěn pián yi", english: "Milk is cheap.", vocabIds: ["hsk2_056", "hsk2_059"], lesson: 6 },

    // Lesson 7: Weather & Seasons
    { id: "s2_020", character: "外麵下雪了。", pinyin: "wài miàn xià xuě le", english: "It's snowing outside.", vocabIds: ["hsk2_061"], lesson: 7 },
    { id: "s2_021", character: "今天是陰天。", pinyin: "jīn tiān shì yīn tiān", english: "Today is cloudy.", vocabIds: ["hsk2_063"], lesson: 7 },
    { id: "s2_022", character: "今天比昨天冷。", pinyin: "jīn tiān bǐ zuó tiān lěng", english: "Today is colder than yesterday.", vocabIds: ["hsk2_068", "hsk2_066"], lesson: 7 },

    // Lesson 8: House & Objects
    { id: "s2_023", character: "桌子上有報紙。", pinyin: "zhuō zi shàng yǒu bào zhǐ", english: "There is a newspaper on the table.", vocabIds: ["hsk2_074", "hsk2_077"], lesson: 8 },
    { id: "s2_024", character: "那是誰的手錶？", pinyin: "nà shì shuí de shǒu biǎo", english: "Whose watch is that?", vocabIds: ["hsk2_076"], lesson: 8 },
    { id: "s2_025", character: "房間裡沒有人。", pinyin: "fáng jiān lǐ méi yǒu rén", english: "There is no one in the room.", vocabIds: ["hsk2_072"], lesson: 8 },

    // Lesson 9: Work & Study
    { id: "s2_026", character: "這個題是什麼意思？", pinyin: "zhè ge tí shì shén me yì si", english: "What does this question mean?", vocabIds: ["hsk2_082", "hsk2_083"], lesson: 9 },
    { id: "s2_027", character: "謝謝你的幫助。", pinyin: "xiè xie nǐ de bāng zhù", english: "Thank you for your help.", vocabIds: ["hsk2_085"], lesson: 9 },
    { id: "s2_028", character: "別說話，考試了。", pinyin: "bié shuō huà, kǎo shì le", english: "Don't talk, it's an exam.", vocabIds: ["hsk2_086", "hsk2_088"], lesson: 9 },

    // Lesson 10: Relationships
    { id: "s2_029", character: "這是我姐姐的丈夫。", pinyin: "zhè shì wǒ jiě jie de zhàng fu", english: "This is my older sister's husband.", vocabIds: ["hsk2_094", "hsk2_091"], lesson: 10 },
    { id: "s2_030", character: "讓我給你介紹一下。", pinyin: "ràng wǒ gěi nǐ jiè shào yí xià", english: "Let me introduce to you.", vocabIds: ["hsk2_100"], lesson: 10 },
    { id: "s2_031", character: "你看見那個男人了嗎？", pinyin: "nǐ kàn jiàn nà ge nán rén le ma", english: "Did you see that man?", vocabIds: ["hsk2_098"], lesson: 10 },

    // Lesson 11: Time & Frequency
    { id: "s2_032", character: "我等了二十分鐘。", pinyin: "wǒ děng le èr shí fēn zhōng", english: "I waited for twenty minutes.", vocabIds: ["hsk2_104", "hsk2_128"], lesson: 11 },
    { id: "s2_033", character: "請再說一次。", pinyin: "qǐng zài shuō yí cì", english: "Please say it one more time.", vocabIds: ["hsk2_108", "hsk2_102"], lesson: 11 },
    { id: "s2_034", character: "你怎麼還沒起床？", pinyin: "nǐ zěn me hái méi qǐ chuáng", english: "How come you haven't gotten up yet?", vocabIds: ["hsk2_109", "hsk2_011"], lesson: 11 },

    // Lesson 12: Comparisons & Quantity
    { id: "s2_035", character: "我有兩千塊錢。", pinyin: "wǒ yǒu liǎng qiān kuài qián", english: "I have two thousand yuan.", vocabIds: ["hsk2_116", "hsk2_118"], lesson: 12 },
    { id: "s2_036", character: "我是第一名。", pinyin: "wǒ shì dì yī míng", english: "I am first place.", vocabIds: ["hsk2_119"], lesson: 12 },
    { id: "s2_037", character: "這個最好吃。", pinyin: "zhè ge zuì hǎo chī", english: "This is the most delicious.", vocabIds: ["hsk2_111"], lesson: 12 },

    // Lesson 13: Actions & Directions
    { id: "s2_038", character: "請進房間。", pinyin: "qǐng jìn fáng jiān", english: "Please enter the room.", vocabIds: ["hsk2_121", "hsk2_072"], lesson: 13 },
    { id: "s2_039", character: "我送給你一本書。", pinyin: "wǒ sòng gěi nǐ yì běn shū", english: "I give you a book (as a gift).", vocabIds: ["hsk2_123", "hsk2_124"], lesson: 13 },
    { id: "s2_040", character: "你告訴他了嗎？", pinyin: "nǐ gào su tā le ma", english: "Did you tell him?", vocabIds: ["hsk2_125"], lesson: 13 },

    // Lesson 14: Modals & States
    { id: "s2_041", character: "因為下雨，所以我沒去。", pinyin: "yīn wèi xià yǔ, suǒ yǐ wǒ méi qù", english: "Because it rained, so I didn't go.", vocabIds: ["hsk2_136", "hsk2_137"], lesson: 14 },
    { id: "s2_042", character: "門開著呢。", pinyin: "mén kāi zhe ne", english: "The door is open.", vocabIds: ["hsk2_071", "hsk2_134"], lesson: 14 },
    { id: "s2_043", character: "雖然很貴，但是我喜歡。", pinyin: "suī rán hěn guì, dàn shì wǒ xǐ huān", english: "Although it's expensive, I like it.", vocabIds: ["hsk2_139", "hsk2_138"], lesson: 14 },

    // Lesson 15: Review & Integration
    { id: "s2_044", character: "祝你生日快樂！", pinyin: "zhù nǐ shēng rì kuài lè", english: "Happy birthday to you!", vocabIds: ["hsk2_150", "hsk2_145"], lesson: 15 },
    { id: "s2_045", character: "我們去旅遊吧。", pinyin: "wǒ men qù lǚ yóu ba", english: "Let's go travel.", vocabIds: ["hsk2_148"], lesson: 15 },
    { id: "s2_046", character: "你會唱歌跳舞嗎？", pinyin: "nǐ huì chàng gē tiào wǔ ma", english: "Can you sing and dance?", vocabIds: ["hsk2_146", "hsk2_147"], lesson: 15 },
];

export default hsk2Sentences;

export function getSentencesByLesson(lessonNum) {
    return hsk2Sentences.filter(s => s.lesson === lessonNum);
}
