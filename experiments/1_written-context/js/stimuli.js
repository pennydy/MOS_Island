var all_stimuli = [
    {
      "item": "1",
      "verb": "whisper",
      "condition":"critical",
      "verb_focus": "Scott said: John didn't WHISPER that Mary met with the lawyer.",
      "emb_focus": "Scott said: John didn't whisper that Mary met with the LAWYER.",
      "target_full": "Hanako said: Then who did John whisper that Mary met with?",
      "option_bg": "Who Mary met with according to John.",
      "option_fg": "The way John said that Mary met with the lawyer."
    },
    {
      "item": "2",
      "verb": "stammer",
      "condition":"critical",
      "verb_focus": "Scott said: Emma didn't STAMMER that Kevin lost the keys.",
      "emb_focus": "Scott said: Emma didn't stammer that Kevin lost the KEYS.",
      "target_full": "Hanako said: Then what did Emma stammer that Kevin lost?",
      "option_bg": "What Kevin lost according to Emma.",
      "option_fg": "The way Emma said that Kevin lost the keys."
    },
    {
      "item": "3",
      "verb": "mumble",
      "condition":"critical",
      "verb_focus": "Scott said: Howard didn't MUMBLE that Alex bought a birthday cake.",
      "emb_focus": "Scott said: Howard didn't mumble that Alex bought a BIRTHDAY CAKE.",
      "target_full": "Hanako said: Then what did Howard mumble that Alex bought?",
      "option_bg": "What Alex bought according to Howard.",
      "option_fg": "The way Howard said that Alex bought a birthday cake."
    },
    {
      "item": "4",
      "verb": "mutter",
      "condition":"critical",
      "verb_focus": "Scott said: Laura didn't MUTTERED that Brandon broke his laptop.",
      "emb_focus": "Scott said: Laura didn't mutter that Brandon broke his LAPTOP.",
      "target_full": "Hanako said: Then what did Laura mutter that Brandon broke?",
      "option_bg": "What Brandon broke according to Laura.",
      "option_fg": "The way Laura said that Brandon broke his laptop."
    },
    {
      "item": "5",
      "verb": "shout",
      "condition":"critical",
      "verb_focus": "Scott said: Bill didn't SHOUT that Dan knew the professor.",
      "emb_focus": "Scott said: Bill didn't shout that Dan knew the PROFESSOR.",
      "target_full": "Hanako said: Then who did Bill shout that Dan knew?",
      "option_bg": "Who Dan knew according to Bill.",
      "option_fg": "The way Bill said that Dan knew the professor."
    },
    {
      "item": "6",
      "verb": "scream",
      "condition":"critical",
      "verb_focus": "Scott said: Amy didn't SCREAM that Charlie saw the robber.",
      "emb_focus": "Scott said: Amy didn't scream that Charlie saw the ROBBER.",
      "target_full": "Hanako said: Then who did Amy scream that Charlie saw?",
      "option_bg": "Who Charlie saw according to Amy.",
      "option_fg": "The way Amy said that Charlie saw the robber."
    },
    {
      "item": "7",
      "verb": "yell",
      "condition":"critical",
      "verb_focus": "Scott said: Jake didn't YELL that Yumi found the wallet.",
      "emb_focus": "Scott said: Jake didn't yell that Yumi found the WALLET.",
      "target_full": "Hanako said: Then what did Jake yell that Yumi found?",
      "option_bg": "What Yumi found according to Jake.",
      "option_fg": "The way Jake said that Yumi found the wallet."
    },
    {
      "item": "8",
      "verb": "groan",
      "condition":"critical",
      "verb_focus": "Scott said: Ashley didn't GROAN that Hasan talked to the detectives.",
      "emb_focus": "Scott said: Ashley didn't groan that Hasan talked to the DETECTIVES.",
      "target_full": "Hanako said: Then who did Ashley groan that Hasan talked to?",
      "option_bg": "Who Hasan talked to according to Ashley.",
      "option_fg": "The way Ashley said that Hasan talked to the detectives."
    },
    {
      "item": "9",
      "verb": "whine",
      "condition":"critical",
      "verb_focus": "Scott said: Yash didn't WHINE that Ming forgot her phone.",
      "emb_focus": "Scott said: Yash didn't whine that Ming forgot her PHONE.",
      "target_full": "Hanako said: Then what did Yash whine that Ming forgot?",
      "option_bg": "What Ming forgot according to Yash.",
      "option_fg": "The way Yash said that Ming forgot her phone."
    },
    {
      "item": "10",
      "verb": "murmur",
      "condition":"critical",
      "verb_focus": "Scott said: Fatima didn't MURMUR that Omar had dinner with his manager.",
      "emb_focus": "Scott said: Fatima didn't murmur that Omar had dinner with his MANAGER.",
      "target_full": "Hanako said: Then who did Fatima murmur that Omar had dinner with?",
      "option_bg": "Who Omar had dinner with according to Fatima.",
      "option_fg": "The way Fatima said that Omar had dinner with his manager."
    },
    {
      "item": "11",
      "verb": "shriek",
      "condition":"critical",
      "verb_focus": "Scott said: Igor didn't SHRIEK that Penny won the lottery.",
      "emb_focus": "Scott said: Igor didn't shriek that Penny won the LOTTERY.",
      "target_full": "Hanako said: Then what did Igor shriek that Penny won?",
      "option_bg": "What Penny won according to Igor.",
      "option_fg": "The way Igor said that Penny won the lottery."
    },
    {
      "item": "12",
      "verb": "moan",
      "condition":"critical",
      "verb_focus": "Scott said: Chandler didn't MOAN that Tia brought her parents.",
      "emb_focus": "Scott said: Chandler didn't moan that Tia brought her PARENTS.",
      "target_full": "Hanako said: Then who did Chandler moan that Tia brought?",
      "option_bg": "Who Tia brought according to Chandler.",
      "option_fg": "The way Chandler said that Tia brought her parents."
    },
    {
      "item": "101",
      "verb": "say",
      "condition": "filler_good_1",
      "context_full": "Scott said: Hannah didn't say that George emailed the STUDENTS.",
      "target_full": "Hanako said: Then who did Hannah say that George emailed?",
      "option_bg": "Who George emailed according to Hannah.",
      "option_fg": "Who emailed the students according to Hannah."
    },
    {
      "item": "102",
      "verb": "think",
      "condition": "filler_good_1",
      "context_full": "Scott said: Ollie didn't think that Doug would invite the MAYOR.",
      "target_full": "Hanako said: Then who did Ollie think that Doug would invite?",
      "option_bg": "Who Doug would invite according to Ollie.",
      "option_fg": "Who invited the mayor acoording to Ollie."
    },
    {
      "item": "103",
      "verb": "suspect",
      "condition": "filler_good_1",
      "context_full": "Scott said: Nancy didn't suspect that Julian ate the LAST CUPCAKE.",
      "target_full": "Hanako said: Then what did Nancy suspect that Julian ate?",
      "option_bg": "What Julian ate according to Nancy.",
      "option_fg": "Who ate the last cupcake according to Nancy."
    },
    {
      "item": "104",
      "verb": "suggest",
      "condition": "filler_good_1",
      "context_full": "Scott said: Grace didn't sugguest that Karen wrote a BOOK.",
      "target_full": "Hanako said: Then what did Grace suggst that Karen wrote?",
      "option_bg": "What Karen wrote according to Grace.",
      "option_fg": "Who wrote a book according to Grace."
    },
    {
      "item": "105",
      "verb": "believe",
      "condition": "filler_good_1",
      "context_full": "Scott said: Danny didn't believe that Jennifer got a CAT.",
      "target_full": "Hanako said: Then what did Danny believe that Jennifer got?",
      "option_bg": "What Jennifer got according to Danny.",
      "option_fg": "Who got a cat according to Danny."
    },
    {
      "item": "106",
      "verb": "expect",
      "condition": "filler_good_1",
      "context_full": "Scott said: Charlie didn't expect that Maddie would call her ROOMMATE.",
      "target_full": "Hanako said: Then who did Charlie expect that Maddie would call?",
      "option_bg": "Who Maddie would call according to Charlie.",
      "option_fg": "Who would call Maddie's roommates according to Charlie."
    },
    {
      "item": "107",
      "verb": "imply",
      "condition": "filler_good_2",
      "context_full": "Scott said: RONALD didn't imply that Jacy rented the truck.",
      "target_full": "Hanako said: Then who implied that Jacy rented the truck?",
      "option_bg": "Who implied that Jacy rented the truck.",
      "option_fg": "What Jacy rented according to Ronald."
    },
    {
      "item": "108",
      "verb": "hope",
      "condition": "filler_good_2",
      "context_full": "Scott said: MATTHEW didn't hope that Dana brought a gift.",
      "target_full": "Hanako said: Then who hoped that Dana brought a gift?",
      "option_bg": "Who hoped that Dana brought a gift.",
      "option_fg": "What Dana brought according to Matthew."
    },
    {
      "item": "109",
      "verb": "insist",
      "condition": "filler_good_2",
      "context_full": "Scott said: KATE didn't insist that Zack broke the plates.",
      "target_full": "Hanako said: Then who insisted that Zack broke the plates?",
      "option_bg": "Who insisted that Zack broke the plates.",
      "option_fg": "What Zack broke according to Kate."
    },
    {
      "item": "110",
      "verb": "guess",
      "condition": "filler_good_2",
      "context_full": "Scott said: JULIAN didn't guess that Charlie received a message.",
      "target_full": "Hanako said: Then who guessed that Charlie received a message?",
      "option_bg": "Who guessed that Charlie received a message.",
      "option_fg": "What Charlie received according to Julian."
    },
    {
      "item": "111",
      "verb": "reveal",
      "condition": "filler_good_2",
      "context_full": "Scott said: JESS didn't reveal that Todd went out with Betty.",
      "target_full": "Hanako said: Then who revealed that Todd went out with Betty?",
      "option_bg": "Who revealed that Todd went out with Betty.",
      "option_fg": "Who Todd went out with according to Jess."
    },
    {
      "item": "112",
      "verb": "expect",
      "condition": "filler_good_2",
      "context_full": "Scott said: SALLY didn't expect that Tony would visit George.",
      "target_full": "Hanako said: Then who expected that Tony would visit George?",
      "option_bg": "Who expected that Tony would visit George.",
      "option_fg": "Who Tony would visit according to Sally."
    },
    {
      "item": "113",
      "verb": "say",
      "condition": "filler_bad_1",
      "context_full": "Scott said: Emily didn't say that PAUL drafted the letter.",
      "target_full": "Hanako said: Then who did Emily say that drafted the letter?",
      "option_bg": "Who drafted the letter according to Emily.",
      "option_fg": "Who said that Paul drafted the letter."
    },
    {
      "item": "114",
      "verb": "think",
      "condition": "filler_bad_1",
      "context_full": "Scott said: Helen didn't think that MARC cut the bread. ",
      "target_full": "Hanako said: Then who did Helen think that cut the bread?",
      "option_bg": "Who cut the bread according to Helen.",
      "option_fg": "Who thought that Marc cut the bread."
    },
    {
      "item": "115",
      "verb": "suspect",
      "condition": "filler_bad_1",
      "context_full": "Scott said: Tim didn't suspect that JASON drank the beer. ",
      "target_full": "Hanako said: Then who did Tim suspect that drank the beer?",
      "option_bg": "Who drank the beer according to Tim.",
      "option_fg": "Who suspected that Jason drank the beer."
    },
    {
      "item": "116",
      "verb": "suggest",
      "condition": "filler_bad_1",
      "context_full": "Scott said: Tony didn't suggest that LISA was in the office.",
      "target_full": "Hanako said: Then who did Tony suggest that was in the office?",
      "option_bg": "Who was in the office according to Tony.",
      "option_fg": "Who suggested that Lisa was in the office."
    },
    {
      "item": "117",
      "verb": "believe",
      "condition": "filler_bad_1",
      "context_full": "Scott said: Larry didn't believe that NICK cooked dinner.",
      "target_full": "Hanako said: Then who did Larry believe that cooked dinner?",
      "option_bg": "Who cooked dinner according to Larry.",
      "option_fg": "Who believed that Nick cooked dinner."
    },
    {
      "item": "118",
      "verb": "expect",
      "condition": "filler_bad_1",
      "context_full": "Scott said: Ben didn't expect that LAURA would order pizza.",
      "target_full": "Hanako said: Then who did Ben expect that would order pizza?",
      "option_bg": "Who would order pizza according to Ben.",
      "option_fg": "Who expected that Laura would order pizza."
    },
    {
      "item": "119",
      "verb": "imply",
      "condition": "filler_bad_2",
      "context_full": "Scott said: Rui didn't imply that the brother of CAROL signed the contract.",
      "target_full": "Hanako said: Then who did Rui imply that the brother of signed the contract?",
      "option_bg": "Whose brother signed the contract according to Rui.",
      "option_fg": "What the brother of Carol signed according to Rui."
    },
    {
      "item": "120",
      "verb": "hope",
      "condition": "filler_bad_2",
      "context_full": "Scott said: Frankie didn't hope that the agent of the GUITARIST would show up. ",
      "target_full": "Hanako said: Then who did Frankie hope that the agent of would show up?",
      "option_bg": "Whose agent Frankie hoped would show up.",
      "option_fg": "What the agent of the guitarist did according to Frankie."
    },
    {
      "item": "121",
      "verb": "insist",
      "condition": "filler_bad_2",
      "context_full": "Scott said: Ted didn't insist that the uncle of EVA should stay for dinner. ",
      "target_full": "Hanako said: Then who did Ted insist that the uncle of should stay for dinner?",
      "option_bg": "Whose uncle should stay for dinner according to Ted.",
      "option_fg": "What the uncle of Eva should do according to Ted."
    },
    {
      "item": "122",
      "verb": "guess",
      "condition": "filler_bad_2",
      "context_full": "Scott said: Jessie didn't guess that the assistant to the DIRECTOR would give the presentation.",
      "target_full": "Hanako said: Then who did Jessie guess that the assistant to would give the presentation?",
      "option_bg": "Whose assistant would give the presentation according to Jessie.",
      "option_fg": "What the assistant to the director would do according to Jessie."
    },
    {
      "item": "123",
      "verb": "reveal",
      "condition": "filler_bad_2",
      "context_full": "Scott said: Donald didn't reveal that the parents of KRISHNA were moving to Texas. ",
      "target_full": "Hanako said: Then who did Donald reveal that the parents of were moving to Texas?",
      "option_bg": "Whose parents were moving to Texas according to Donald.",
      "option_fg": "Where Krishna's parents were moving to according to Donald."
    },
    {
      "item": "124",
      "verb": "expect",
      "condition": "filler_bad_2",
      "context_full": "Scott said: Jamal didn't expect that the roommates of NINA would come to the party.",
      "target_full": "Hanako said: Then who did Jamal expect that the roommates of would come to the party?",
      "option_bg": "Whose roommates would come to the party according to Jamal.",
      "option_fg": "What  Nina's roommates would do according to Jamal. "
    },
    {
      "item": "201",
      "verb": "",
      "condition": "practice_good",
      "context_full": "Scott said: Fiona didn't buy PINEAPPLES. ",
      "target_full": "Hanako said: Then what did Fiona buy?",
      "option_bg": "What Fiona bought.",
      "option_fg": "Who bought pineapples."
    },
    {
      "item": "202",
      "verb": "",
      "condition": "practice_good",
      "context_full": "Scott said: Vera didn't DRIVE to Michigan. ",
      "target_full": "Hanako said: Then how did Vera get to Michigan?",
      "option_bg": "How Vera travelled to Michigan.",
      "option_fg": "Where Vera drove to."
    },
    {
      "item": "203",
      "verb": "",
      "condition": "practice_bad",
      "context_full": "Scott said: Hank didn't buy the RED car.",
      "target_full": "Hanako said: Then what color did Hank buy car?",
      "option_bg": "Which car Hank bought.",
      "option_fg": "Who bought the red car."
    },
    {
      "item": "204",
      "verb": "",
      "condition": "practice_bad",
      "context_full": "Scott said: Prisha doesn't speak KOREAN.",
      "target_full": "Hanako said: Then what Prisha does speak the language?",
      "option_bg": "What language does Prisha speak.",
      "option_fg": "Who speaks Korean."
    }
  ]