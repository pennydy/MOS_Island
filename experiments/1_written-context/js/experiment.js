var order = 1;

function make_slides(f) {
    var slides = {};

    
    slides.i0 = slide({
        name : "i0",
        start: function() {
            exp.startT = Date.now();
        }
    });

    slides.instructions = slide({
        name : "instructions",
        button : function() {
            exp.startT = Date.now();
            exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });

  
    slides.practice_slider_good_1 = slide({
        name : "practice_slider_good_1",
        present : [{"a": 1}],
        start : function() {
            $(".err").hide(); // hide the error message   
            $(".errgood").hide();
        },
        present_handle : function(stim) {
            $(".err").hide();
            $(".errgood").hide();
            this.stim = stim;
            this.init_sliders();
            $(".slider_table").show();
            $(".prompt").html("Scott said: Fiona didn't buy <strong>PINEAPPLES</strong>.  <p>  Hanako said: Then what did Fiona buy?");
            $(".question").html("How natural/acceptable does Hanako's question sound?");
            exp.sliderPost = null; // erase current slider value
            // exp.first_response_wrong = 0;
            exp.attempts = 0;
        },

        button : function() {
            if (exp.sliderPost == null) {
                $(".err").show();
            } else if (exp.sliderPost < 0.5) {
                // exp.first_response_wrong = 1;
                // exp.attempts = exp.attempts + 1;
                $(".errgood").show();
            } else {
                this.log_responses();
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                _stream.apply(this);
                // exp.go(); // check if the response is logged
            }
        },

        init_sliders : function() {
            utils.make_slider("#practice_slider_1", function(event, ui) {
                exp.sliderPost = ui.value;
            });
        },

        log_responses : function() {
          exp.data_trials.push({
            "response" : exp.sliderPost,
            // "wrong_attempts": exp.attempts,

            "item_type" : "practice_good_1",
            "block_sequence": "practice",
            "item_number": "practice_good_1",
            "condition": "practice_good",
          });
        }
    });
    
    
    slides.post_practice_1 = slide({
        name : "post_practice_1",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });
    
    slides.practice_choice_good_2 = slide({
        name : "practice_choice_good_2",
    
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a": 2}],
        start : function() {
            $(".err").hide(); // hide the error message   
            $(".errgood").hide();
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".err").hide();
            $(".errgood").hide();
            this.stim = stim;
            $(".prompt").html("Scott said: Vera didn't DRIVE to <b>Michigan<\/b>.  <p>  Hanako said: Then how did Vera get to Michigan?");
            $(".question").html("What was Scott talking about?");
            exp.first_response_wrong = 0;
            exp.response = undefined;
            $('input[name="practice"]:checked').removeAttr("checked");
            var left_botton = "Where Vera drove to."
            $(".left_botton").html(left_botton);
            var right_botton = "How Vera travelled to Michigan."
            $(".right_botton").html(right_botton);
            exp.attempts = 0;
        },
        button : function() {
            exp.response = $('input[name="practice"]:checked').val()
            if (exp.response == undefined) {
                $(".forced_choice_err").show();
            } else if (exp.response == "backgrounded") {
                exp.first_response_wrong = 1;
                exp.attempts = exp.attempts + 1;
                $(".errgood").show();
            } else {
                this.log_responses();
                _stream.apply(this);
            }
        },
        log_responses : function() {
          exp.data_trials.push({
            "response" : exp.response,
            "wrong_attempts": exp.attempts,
            "item_type" : "practice_good_2",
            "block_sequence": "practice",
            "item_number": "practice_good_2",
            "phase": "practice_good",
            "trial_sequence_total": 0,
          });
    
        }
      });
    
    
    slides.practice_slider_bad_2 = slide({
        name : "practice_slider_bad_2",
    
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [1],
    
      
        //this gets run only at the beginning of the block
        present_handle : function(stim) {
          $(".err").hide();
          $(".errbad").hide();
          $(".prompt").html("Prisha doesn't speak <b>KOREAN<\/b>.  <p>  Hanako said: Then what Prisha does speak the language?");
          $(".question").html("Is Hanako's question natural?");
          this.init_sliders();
          exp.sliderPost = null; //erase current slider value
          exp.first_response_wrong = 0;
          exp.attempts = 0;
        },
        button : function() {
          if (exp.sliderPost == null) {
            $(".err").show();
          } 
          else if (exp.sliderPost > 0.5) {
            exp.first_response_wrong = 1;
            exp.attempts = exp.attempts + 1;
            $(".errbad").show();
          }
          else {
            this.log_responses();
            /* use _stream.apply(this); if and only if there is
            "present" data. (and only *after* responses are logged) */
            _stream.apply(this);
          }
        },
        init_sliders : function() {
          utils.make_slider("#practice_slider_2", function(event, ui) {
            exp.sliderPost = ui.value;
            
          });
        },
        log_responses : function() {
          exp.data_trials.push({
            "acceptability_rating" : exp.sliderPost,
            "wrong_attempts": exp.attempts,
            "item_type" : "practice_bad_2",
            "condition": "practice",
            "bg_response" : "NA"
          });
    
        }
      });
    
      slides.post_practice_2 = slide({
        name : "post_practice_2",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });
    
    
      slides.last_reminder = slide({
        name : "last_reminder",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
        
      });

    slides.block1 = slide({
        name : "block1",
        present : exp.stims_block,
        start : function() {
            $(".err").hide(); // hide the error message   
        },
        
        present_handle : function(stim) {
            $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
            this.stim = stim;
            this.stim.trial_start = Date.now();      
            $(".err").hide();   
            $(".bg_err").hide() 
            
            var context = this.stim.context_full;
            var target = this.stim.target_full;
            $(".context").html(context);
            $(".target").html(target);

            exp.acceptability_sliderPost = null;
            exp.bg_response = undefined;
            
            if (this.stim.task == "acceptability") {
                this.init_sliders();
                var question = "Is Hanako's question natural?";

                var leftLabel =  "very unnatural";
                $(".leftLabel").html(leftLabel);
                var rightLabel = "very natural";
                $(".rightLabel").html(rightLabel);

                $(".slider_table").show();

                exp.bg_response = "NA";
            } else if (this.stim.task == "backgroundedness") {
                var question = "What was Scott talking about?";
                exp.bg_response = undefined;
                $(".slider_table").hide(); // hide the slider
                $('input[name="practice"]:checked').removeAttr("checked");
                // the order of the bottons also need to be randomized
                // CHANGE THE LABELS IN THE HTML FILE AS WELL
                var left_botton = this.stim.option_bg;
                $(".left_botton").html(left_botton);
                var right_botton = this.stim.option_fg;
                $(".right_botton").html(right_botton);

                exp.acceptability_sliderPost = "NA";
            }

            $(".continue_button").show(); // show the belief button
            $(".question").show(question);

            console.log(this.stim); 
  
        },
        
        button : function() {
            console.log("acceptability rating: "+exp.acceptability_sliderPost);
            if (this.stim.task=="acceptability" & exp.acceptability_sliderPost != null) {
                $(".err").show();
            } else if (this.stim.task=="backgrounededness" & exp.response != undefined) {
                $(".forced_choice_err").show();
            } else {
                this.log_responses();
                _stream.apply(this); //use exp.go() if and only if there is no "present" data.
            }
        },

        init_sliders : function() {
            utils.make_slider("#single_slider", function(event, ui) {
                exp.acceptability_sliderPost = ui.value;
            });
        },

        log_responses : function() {
            exp.data_trials.push({
                // "question_type" : this.stim.block, // only 1 block
                // "slide_number_in_experiment" : exp.phase, // trial number
                "trial_num" : order,
                "item_id" : this.stim.item,
                "condition" : this.stim.condition,
                "verb": this.stim.verb,
                "task": this.stim.task,
                "context": context,  // check if the context matches with the condition
                "acceptability_rating" : exp.acceptability_sliderPost,
                "bg_response" : exp.bg_response,
                "rt" : Date.now() - this.stim.trial_start
            });
            order += 1;
        }

    }); 
  
 
    slides.questionaire =  slide({
        name : "questionaire",
        submit : function(e){
        //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
        exp.subj_data = {
            language : $("#language").val(),
            enjoyment : $("#enjoyment").val(),
            asses : $('input[name="assess"]:checked').val(),
            age : $("#age").val(),
            gender : $("#gender").val(),
            education : $("#education").val(),
            comments : $("#comments").val(),
        };
        exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });

    slides.finished = slide({
        name : "finished",
        start : function() {
        exp.data= {
            "trials" : exp.data_trials,
            "catch_trials" : exp.catch_trials,
            "system" : exp.system,
            "condition" : exp.condition,
            "subject_information" : exp.subj_data,
            "time_in_minutes" : (Date.now() - exp.startT)/60000
        };
        // record data using proliferate
        proliferate.submit(exp.data);
        }
    });
    console.log(slides);

    return slides;
}

function init() {
    
    var critical_verbs = _.shuffle([
        {
            "item": "1",
            "verb": "whisper",
            "condition": "critical",
            "verb_focus": "Scott said: John didn't WHISPER that Mary met with the lawyer.",
            "emb_focus": "Scott said: John didn't whisper that Mary met with the LAWYER.",
            "target_full": "Hanako said: Then who did John whisper that Mary met with?",
            "option_bg": "Who Mary met with according to John.",
            "option_fg": "The way John said that Mary met with the lawyer."
          },
          {
            "item": "2",
            "verb": "stammer",
            "condition": "critical",
            "verb_focus": "Scott said: Emma didn't STAMMER that Kevin lost the keys.",
            "emb_focus": "Scott said: Emma didn't stammer that Kevin lost the KEYS.",
            "target_full": "Hanako said: Then what did Emma stammer that Kevin lost?",
            "option_bg": "What Kevin lost according to Emma.",
            "option_fg": "The way Emma said that Kevin lost the keys."
          },
          {
            "item": "3",
            "verb": "mumble",
            "condition": "critical",
            "verb_focus": "Scott said: Howard didn't MUMBLE that Alex bought a birthday cake.",
            "emb_focus": "Scott said: Howard didn't mumble that Alex bought a BIRTHDAY CAKE.",
            "target_full": "Hanako said: Then what did Howard mumble that Alex bought?",
            "option_bg": "What Alex bought according to Howard.",
            "option_fg": "The way Howard said that Alex bought a birthday cake."
          },
          {
            "item": "4",
            "verb": "mutter",
            "condition": "critical",
            "verb_focus": "Scott said: Laura didn't MUTTERED that Brandon broke his laptop.",
            "emb_focus": "Scott said: Laura didn't mutter that Brandon broke his LAPTOP.",
            "target_full": "Hanako said: Then what did Laura mutter that Brandon broke?",
            "option_bg": "What Brandon broke according to Laura.",
            "option_fg": "The way Laura said that Brandon broke his laptop."
          },
          {
            "item": "5",
            "verb": "shout",
            "condition": "critical",
            "verb_focus": "Scott said: Bill didn't SHOUT that Dan knew the professor.",
            "emb_focus": "Scott said: Bill didn't shout that Dan knew the PROFESSOR.",
            "target_full": "Hanako said: Then who did Bill shout that Dan knew?",
            "option_bg": "Who Dan knew according to Bill.",
            "option_fg": "The way Bill said that Dan knew the professor."
          },
          {
            "item": "6",
            "verb": "scream",
            "condition": "critical",
            "verb_focus": "Scott said: Amy didn't SCREAM that Charlie saw the robber.",
            "emb_focus": "Scott said: Amy didn't scream that Charlie saw the ROBBER.",
            "target_full": "Hanako said: Then who did Amy scream that Charlie saw?",
            "option_bg": "Who Charlie saw according to Amy.",
            "option_fg": "The way Amy said that Charlie saw the robber."
          },
          {
            "item": "7",
            "verb": "yell",
            "condition": "critical",
            "verb_focus": "Scott said: Jake didn't YELL that Yumi found the wallet.",
            "emb_focus": "Scott said: Jake didn't yell that Yumi found the WALLET.",
            "target_full": "Hanako said: Then what did Jake yell that Yumi found?",
            "option_bg": "What Yumi found according to Jake.",
            "option_fg": "The way Jake said that Yumi found the wallet."
          },
          {
            "item": "8",
            "verb": "groan",
            "condition": "critical",
            "verb_focus": "Scott said: Ashley didn't GROAN that Hasan talked to the detectives.",
            "emb_focus": "Scott said: Ashley didn't groan that Hasan talked to the DETECTIVES.",
            "target_full": "Hanako said: Then who did Ashley groan that Hasan talked to?",
            "option_bg": "Who Hasan talked to according to Ashley.",
            "option_fg": "The way Ashley said that Hasan talked to the detectives."
          },
          {
            "item": "9",
            "verb": "whine",
            "condition": "critical",
            "verb_focus": "Scott said: Yash didn't WHINE that Ming forgot her phone.",
            "emb_focus": "Scott said: Yash didn't whine that Ming forgot her PHONE.",
            "target_full": "Hanako said: Then what did Yash whine that Ming forgot?",
            "option_bg": "What Ming forgot according to Yash.",
            "option_fg": "The way Yash said that Ming forgot her phone."
          },
          {
            "item": "10",
            "verb": "murmur",
            "condition": "critical",
            "verb_focus": "Scott said: Fatima didn't MURMUR that Omar had dinner with his manager.",
            "emb_focus": "Scott said: Fatima didn't murmur that Omar had dinner with his MANAGER.",
            "target_full": "Hanako said: Then who did Fatima murmur that Omar had dinner with?",
            "option_bg": "Who Omar had dinner with according to Fatima.",
            "option_fg": "The way Fatima said that Omar had dinner with his manager."
          },
          {
            "item": "11",
            "verb": "shriek",
            "condition": "critical",
            "verb_focus": "Scott said: Igor didn't SHRIEK that Penny won the lottery.",
            "emb_focus": "Scott said: Igor didn't shriek that Penny won the LOTTERY.",
            "target_full": "Hanako said: Then what did Igor shriek that Penny won?",
            "option_bg": "What Penny won according to Igor.",
            "option_fg": "The way Igor said that Penny won the lottery."
          },
          {
            "item": "12",
            "verb": "moan",
            "condition": "critical",
            "verb_focus": "Scott said: Chandler didn't MOAN that Tia brought her parents.",
            "emb_focus": "Scott said: Chandler didn't moan that Tia brought her PARENTS.",
            "target_full": "Hanako said: Then who did Chandler moan that Tia brought?",
            "option_bg": "Who Tia brought according to Chandler.",
            "option_fg": "The way Chandler said that Tia brought her parents."
          }
        ]);

    
    // fillers
    var fillers = _.shuffle([
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
          }
    ])


    // function makeStim(i) {
    //     var item = critical_verbs[i];
    //     // the same as return item, except "item" is relabelled as "item_id"
    //     return{
    //         "item_id" : item.item,
    //         "verb" : item.verb,
    //         "target_full" : item.target_full,
    //         "verb_focus" : item.verb_focus,
    //         "embed_focus" : item.emb_focus,
    //         "option_bg" : item.option_bg,
    //         "option_fg" : item.option_fg
    //     } 
    // }

    // split critical items into half verb half embedded focus
    embed_focus = [];
    verb_focus = [];

    for (var i=0; i<critical_verbs.length/2; i++) {
        var stim = critical_verbs[i];
        stim.condition = "verb_focus";
        stim.context_full = stim["verb_focus"];
        verb_focus.push(stim)
    }
    for (var i=critical_verbs.length/2; i<critical_verbs.length; i++) {
        var stim = critical_verbs[i];
        stim.condition = "embed_focus";
        stim.context_full = stim["embed_focus"];
        embed_focus.push(stim)
    }
    
    filler_good_1 = []
    filler_good_2 = []
    filler_bad_1 = []
    filler_bad_2 = []
    for (var i=0; i<fillers.length; i++) {
        var filler_item = fillers[i];
        var condition = filler_item.condition;
        if (condition == "filler_good_1") {
            filler_good_1.push(filler_item);
        } else if (condition == "filler_good_2") {
            filler_good_2.push(filler_item);
        } else if (condition == "filler_bad_1") {
            filler_bad_1.push(filler_item);
        } else if (condition == "filler_bad_2") {
            filler_bad_2.push(filler_item);
        }
    }

    num_blocks = 6
    num_per_block = 6
    total_blocks = []
    exp.stims_block = []
    for (var i=0; i<num_blocks; i++) {
        // each block will have two critical items(one embed_focus, one verb_focus) and four filler items(one in each condition)
        // num_per_block == block.length == 6
        var block = [embed_focus.pop(), verb_focus.pop(),filler_good_1.pop(), filler_good_2.pop(), filler_bad_1.pop(), filler_bad_2.pop()];
        // split into half acceptability and half backgroundedness inside each block
        for (var j=0; j<block.length/2; j++) {
            // console.log(block);
            block[j].task = "acceptability";
        }
        for (var j=block.length/2; j<block.length; j++) {
            block[j].task = "backgroundedness";
        }
        // randomize the items within each block
        block = _.shuffle(block);
        console.log(block)
        total_blocks.push(block);
    }
    // randomize the order of blocks
	total_blocks = _.shuffle(total_blocks); 
    console.log(total_blocks);
    total_blocks = total_blocks.flat();

    for (var k=0; k<num_per_block * num_blocks; k++) {
        var stim = total_blocks[k];
        exp.stims_block.push(jQuery.extend(true, {}, stim));
    }

    console.log(exp.stims_block) 

    
    exp.trials = [];
    exp.catch_trials = [];
    // exp.condition = {}; // can randomize between subject conditions here -> not needed?
    exp.system = {
        Browser : BrowserDetect.browser,
        OS : BrowserDetect.OS,
        screenH: screen.height,
        screenUH: exp.height,
        screenW: screen.width,
        screenUW: exp.width
    };
    //blocks of the experiment:
    exp.structure=["i0", "practice_slider_good_1","post_practice_1", "practice_choice_good_2", "practice_slider_bad_2", "block1", 'questionaire', 'finished'];
    // console.log(exp.structure);

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 1 + 4 + 1 + 36 + 1; 
    $(".nQs").html(exp.nQs);

    $('.slide').hide(); //hide everything

    $("#start_button").click(function() {
        exp.go();
    });

    exp.go(); //show first slide
}
