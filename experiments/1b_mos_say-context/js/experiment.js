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

  
    slides.practice_acceptability_good_1 = slide({
        name : "practice_acceptability_good_1",
        present : [{"a": 1}],
        start : function() {
            $(".slider_err").hide(); // hide the error message   
            $(".errgood").hide();
        },
        present_handle : function(stim) {
            $(".slider_err").hide();
            $(".err_good").hide();
            this.stim = stim;
            this.init_sliders();
            $(".slider_table").show();
            var context = "Hanako said: Fiona didn't buy <b>PINEAPPLES</b>.";
            exp.context = context;
            $(".context").html(context);
            var target = "Scott said: Then what did Fiona buy?";
            $(".target").html(target);
            $(".question").html("How natural/acceptable does <u>Scott</u>'s question sound?");
            exp.sliderPost = null; // erase current slider value
            // exp.first_response_wrong = 0;
            exp.incorrect_attempts = 0;
        },

        button : function() {
            if (exp.sliderPost == null) {
                $(".slider_err").show();
            } else if (exp.sliderPost < 0.5) {
                // exp.first_response_wrong = 1;
                exp.incorrect_attempts += 1;
                $(".slider_err").hide();
                $(".err_good").show();
            } else {
                this.log_responses();
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                _stream.apply(this);
            }
        },

        init_sliders : function() {
            utils.make_slider("#practice_slider_1", function(event, ui) {
                exp.sliderPost = ui.value;
            });
        },

        log_responses : function() {
          // console.log("attempts: "+exp.incorrect_attempts)
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "201",
            "block_id" : "practice",
            "condition" : "practice_acceptability_good_1",
            "verb": "NA",
            "task": "acceptability",
            "context": exp.context,  // check if the context matches with the condition
            "acceptability_rating" : exp.sliderPost,
            "bg_response" : "NA",
            "wrong_attempts" : exp.incorrect_attempts
          });
        }
    });
    
    slides.post_practice_acceptability_good_1 = slide({
        name : "post_practice_acceptability_good_1",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });
    
    slides.practice_bg_good_2 = slide({
        name : "practice_bg_good_2",
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a": 2}],
        start : function() {
            $(".forced_choice_err").hide(); // hide the error message   
            $(".err_good").hide();
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
          $(".forced_choice_err").hide();
          $(".err_good").hide();
          this.stim = stim;
          var context = "Hanako said: Vera didn't <b>DRIVE</b> to Michigan.";
          $(".context").html(context);
          exp.context = context;
          var target = "Scott said: Then how did Vera get to Michigan?"
          $(".target").html(target);
          $(".question").html("What was <u>Hanako</u> talking about?");
          // exp.first_response_wrong = 0;
          exp.bg_response = undefined;
          exp.bg_choice = undefined;
          exp.selected_content = undefined; 
          $('input[name="practice"]:checked').removeAttr("checked");
          var top_button = "Where Vera drove to.";
          $(".top_button").html(top_button);
          var bottom_button = "How Vera travelled to Michigan.";
          $(".bottom_button").html(bottom_button);
          // exp.incorrect_attempts = 0;
        },
        button : function() {
          exp.bg_response = $('input[name="practice"]:checked').val()

          if (exp.bg_response == undefined) {
            $(".forced_choice_err").show();
          } else {
            // we hard-coded that the bottom is the correct answer
            if (exp.bg_response == "bottom") {
              exp.bg_choice = "correct";
              exp.selected_content = "How Vera travelled to Michigan.";
            } else {
              exp.bg_choice = "incorrect";
              exp.selected_content = "Where Vera drove to.";
            }
            this.log_responses();
            _stream.apply(this);
          }
        },
        log_responses : function() {
          // console.log("bg_response: "+ exp.bg_response)
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "202",
            "block_id" : "practice",
            "condition" : "practice_bg_good_2",
            "verb": "NA",
            "task": "backgroundedness",
            "context": exp.context,  // check if the context matches with the condition
            "acceptability_rating" : "NA",
            "bg_response" : exp.bg_choice,
            "original_bg_choice" : exp.selected_content
          });
    
        }
      });

      slides.post_practice_bg_good_2 = slide({
        name : "post_practice_bg_good_2",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });
    
      slides.practice_acceptability_bad_2 = slide({
        name : "practice_acceptability_bad_2",
    
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a":4}],
        start : function() {
          $(".slider_err").hide(); // hide the error message   
          $(".err_bad").hide();
        },
      
        //this gets run only at the beginning of the block
        present_handle : function(stim) {
          $(".slider_err").hide();
          $(".err_bad").hide();
          var context = "Hanako said: Prisha doesn't speak <b>KOREAN</b>.";
          $(".context").html(context);
          exp.context = context;
          var target = "Scott said: Then what Prisha does speak the language?"
          $(".target").html(target);
          $(".question").html("How natural/acceptable does <u>Scott</u>'s question sound?");
          this.init_sliders();
          exp.sliderPost = null; //erase current slider value
          // exp.first_response_wrong = 0;
          exp.incorrect_attempts = 0;
        },
        button : function() {
          if (exp.sliderPost == null) {
            $(".slider_err").show();
          } 
          else if (exp.sliderPost > 0.5) {
            // exp.first_response_wrong = 1;
            exp.incorrect_attempts += 1;
            $(".slider_err").hide();
            $(".err_bad").show();
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
          // console.log("attempts: "+exp.incorrect_attempts)
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "204",
            "block_id" : "practice",
            "condition" : "practice_accpetability_bad_2",
            "verb": "NA",
            "task": "acceptability",
            "context": exp.context,  // check if the context matches with the condition
            "acceptability_rating" : exp.sliderPost,
            "bg_response" : "NA", 
            "wrong_attempts" : exp.incorrect_attempts
          });
    
        }
      });
    
      slides.post_practice_acceptability_bad_2 = slide({
        name : "post_practice_acceptability_bad_2",
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
      });

      slides.practice_bg_bad_1 = slide({
        name : "practice_bg_bad_1",
    
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a": 3}],
        start : function() {
            $(".forced_choice_err").hide(); // hide the error message   
            $(".err_good").hide();
        },
        // this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".forced_choice_err").hide();
            $(".err_good").hide();
            this.stim = stim;
            var context = "Hanako said: Hank didn't buy the <b>RED</b> car.";
            $(".context").html(context);
            exp.context = context;
            var target = "Scott said: Then what color did Hank buy car?"
            $(".target").html(target);
            $(".question").html("What was <u>Hanako</u> talking about?");
            // exp.first_response_wrong = 0;
            exp.bg_response = undefined;
            exp.bg_choice = undefined;
            exp.selected_content = undefined; 
            $('input[name="practice"]:checked').removeAttr("checked");
            var top_button = "Which car Hank bought.";
            $(".top_button").html(top_button);
            var bottom_button = "Who bought the red car.";
            $(".bottom_button").html(bottom_button);
            // exp.incorrect_attempts = 0;
        },
        button : function() {
            exp.bg_response = $('input[name="practice"]:checked').val();

            if (exp.bg_response == undefined) {
                $(".forced_choice_err").show();
            } else {
              // we hard-coded that the top is the correct answer
              if (exp.bg_response == "top") {
                exp.bg_choice = "correct";
                exp.selected_content = "Which car Hank bought.";
              } else {
                exp.bg_choice = "incorrect";
                exp.selected_content = "Who bought the red car.";
              }
              this.log_responses();
              _stream.apply(this);
            }
        },
        log_responses : function() {
          // console.log("bg_response: " + exp.bg_response)
          exp.data_trials.push({
            "trial_num" : 0,
            "item_id" : "203",
            "block_id" : "practice",
            "condition" : "practice_bg_bad_1",
            "verb": "NA",
            "task": "backgroundedness",
            "context": exp.context,  // check if the context matches with the condition
            "acceptability_rating" : "NA",
            "bg_response" : exp.bg_choice,
            "original_bg_choice" : exp.selected_content
          });
        }
      });

      slides.post_practice_bg_bad_1 = slide({
        name : "post_practice_bg_bad_1",
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
            $(".slider_err").hide(); // hide the error message   
            $(".forced_choice_err").hide();
        },
        
        present_handle : function(stim) {
            $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
            this.stim = stim;
            this.stim.trial_start = Date.now();      
            $(".slider_err").hide();   
            $(".forced_choice_err").hide();
            
            var context = this.stim.context_full;
            var target = this.stim.target_full;
            // // underline the target sentence
            // if (this.stim.task == "acceptability") {
            //   target = "<u>" + target + "</u>";
            // } else if (this.stim.task == "backgroundedness") {
            //   context = "<u>" + context + "</u>";
            // }
            $(".context").html(context);
            exp.context = context;
            // console.log("context: "+exp.context);
            $(".target").html(target);
            $(".slider_table").hide(); // hide the slider
            $('input[name=critical]').hide();
            $(".top_button").hide();
            $(".bottom_button").hide();

            $('input[name="critical"]:checked').removeAttr("checked"); // remove the previous response
            exp.acceptability_sliderPost = null; // remove the previous rating
            exp.bg_response = undefined; // remove the previous selection
            exp.bg_choice = undefined; // remove the recorded choice
            exp.selected_content = undefined; // remove the recorded choice
            
            if (this.stim.task == "acceptability") {
                this.init_sliders();
                exp.acceptability_sliderPost = null; // remove rating -> or else it will use the default (0.5)
                exp.question = "How natural/acceptable does <u>Scott</u>'s question sound?";
                $(".slider_table").show();
            } else if (this.stim.task == "backgroundedness") {
                $(".top_button").show();
                $(".bottom_button").show();
                exp.question = "What was <u>Hanako</u> talking about?";
                $('input[name=critical]').show();
                $('input[name="critical"]:checked').removeAttr("checked"); // remove response again
                exp.bg_response = undefined;
                exp.bg_choice = undefined;
                exp.selected_content = undefined;
      
                // the order of the buttons also need to be randomized
                var options = _.shuffle([this.stim.option_bg, this.stim.option_fg])
                // console.log("randomized order of choices: " + options)
                exp.top_button = options[0];
                $(".top_button").html(exp.top_button);
                exp.bottom_button = options[1];
                $(".bottom_button").html(exp.bottom_button);

            }
            $(".continue_button").show(); // show the belief button
            $(".question").html(exp.question);

            console.log(this.stim); 
  
        },
        
        button : function() {
          if (this.stim.task == "acceptability") {
            exp.bg_choice = "NA";
            exp.selected_content = "NA";
            exp.acceptability_sliderPost = exp.acceptability_sliderPost;
          } else {
            exp.bg_response = $('input[name="critical"]:checked').val();
            exp.acceptability_sliderPost = "NA";
          }

          if (this.stim.task=="acceptability" && exp.acceptability_sliderPost == null) {
              $(".slider_err").show();
          } else if (this.stim.task=="backgroundedness" && exp.bg_response == undefined) {
              $(".forced_choice_err").show();
          } else {
            // do the additional step of getting the result of bg_choice only 
            // for items in backgroundedness condition
            if (this.stim.task == "backgroundedness") {
              if (exp.bg_response == "top") {
                var selected_content = exp.top_button;
              } else {
                var selected_content = exp.bottom_button;
              }
              exp.selected_content = selected_content;
              console.log(exp.selected_content)
              // record whether the background or the foreground option is selected
              // for critical: option_bg is about the embedded content; option_fg is about the manner
              // for fillers: option_bg is the "correct" answer; option_fg is "incorrect" (not for analysis)
              if (selected_content == this.stim.option_bg) {
                exp.bg_choice = "embed";
              } else if (selected_content == this.stim.option_fg) {
                exp.bg_choice = "verb";
              }
              console.log(exp.bg_choice)
            }
            
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
                "block_id" : this.stim.block_id,
                "condition" : this.stim.condition,
                "verb": this.stim.verb,
                "task": this.stim.task,
                "context": exp.context,  // check if the context matches with the condition
                "acceptability_rating" : exp.acceptability_sliderPost,
                "bg_response" : exp.bg_choice,
                "original_bg_choice" : exp.selected_content,
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
            american : $('input[name="ame"]:checked').val(),
            enjoyment : $("#enjoyment").val(),
            asses : $('input[name="assess"]:checked').val(),
            age : $("#age").val(),
            fairprice : $("#fairprice").val(),
            gender : $("#gender").val(),
            education : $("#education").val(),
            problems : $("#problems").val(),
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
    
    var critical_mos = _.shuffle([
      {
        "item": "1",
        "verb": "whisper",
        "condition": "critical",
        "verb_focus": "Hanako said: John didn't <b>WHISPER</b> that Mary met with the lawyer.",
        "embed_focus": "Hanako said: John didn't whisper that Mary met with the <b>LAWYER</b>.",
        "target_full": "Scott said: Then who did John whisper that Mary met with?",
        "option_bg": "Who Mary met with, according to John.",
        "option_fg": "The way John said that Mary met with the lawyer."
      },
      {
        "item": "2",
        "verb": "stammer",
        "condition": "critical",
        "verb_focus": "Hanako said: Emma didn't <b>STAMMER</b> that Kevin lost the keys.",
        "embed_focus": "Hanako said: Emma didn't stammer that Kevin lost the <b>KEYS</b>.",
        "target_full": "Scott said: Then what did Emma stammer that Kevin lost?",
        "option_bg": "What Kevin lost, according to Emma.",
        "option_fg": "The way Emma said that Kevin lost the keys."
      },
      {
        "item": "3",
        "verb": "mumble",
        "condition": "critical",
        "verb_focus": "Hanako said: Howard didn't <b>MUMBLE</b> that Alex bought a birthday cake.",
        "embed_focus": "Hanako said: Howard didn't mumble that Alex bought a <b>BIRTHDAY CAKE</b>.",
        "target_full": "Scott said: Then what did Howard mumble that Alex bought?",
        "option_bg": "What Alex bought, according to Howard.",
        "option_fg": "The way Howard said that Alex bought a birthday cake."
      },
      {
        "item": "4",
        "verb": "mutter",
        "condition": "critical",
        "verb_focus": "Hanako said: Laura didn't <b>MUTTERE</b> that Brandon broke his laptop.",
        "embed_focus": "Hanako said: Laura didn't mutter that Brandon broke his <b>LAPTOP</b>.",
        "target_full": "Scott said: Then what did Laura mutter that Brandon broke?",
        "option_bg": "What Brandon broke, according to Laura.",
        "option_fg": "The way Laura said that Brandon broke his laptop."
      },
      {
        "item": "5",
        "verb": "shout",
        "condition": "critical",
        "verb_focus": "Hanako said: Bill didn't <b>SHOUT</b> that Dan knew the professor.",
        "embed_focus": "Hanako said: Bill didn't shout that Dan knew the <b>PROFESSOR</b>.",
        "target_full": "Scott said: Then who did Bill shout that Dan knew?",
        "option_bg": "Who Dan knew, according to Bill.",
        "option_fg": "The way Bill said that Dan knew the professor."
      },
      {
        "item": "6",
        "verb": "scream",
        "condition": "critical",
        "verb_focus": "Hanako said: Amy didn't <b>SCREAM</b> that Charlie saw the robber.",
        "embed_focus": "Hanako said: Amy didn't scream that Charlie saw the <b>ROBBER</b>.",
        "target_full": "Scott said: Then who did Amy scream that Charlie saw?",
        "option_bg": "Who Charlie saw, according to Amy.",
        "option_fg": "The way Amy said that Charlie saw the robber."
      },
      {
        "item": "7",
        "verb": "yell",
        "condition": "critical",
        "verb_focus": "Hanako said: Jake didn't <b>YELL</b> that Yumi found the wallet.",
        "embed_focus": "Hanako said: Jake didn't yell that Yumi found the <b>WALLET</b>.",
        "target_full": "Scott said: Then what did Jake yell that Yumi found?",
        "option_bg": "What Yumi found, according to Jake.",
        "option_fg": "The way Jake said that Yumi found the wallet."
      },
      {
        "item": "8",
        "verb": "groan",
        "condition": "critical",
        "verb_focus": "Hanako said: Ashley didn't <b>GROAN</b> that Hasan talked to the detectives.",
        "embed_focus": "Hanako said: Ashley didn't groan that Hasan talked to the <b>DETECTIVES</b>.",
        "target_full": "Scott said: Then who did Ashley groan that Hasan talked to?",
        "option_bg": "Who Hasan talked to, according to Ashley.",
        "option_fg": "The way Ashley said that Hasan talked to the detectives."
      },
      {
        "item": "9",
        "verb": "whine",
        "condition": "critical",
        "verb_focus": "Hanako said: Yash didn't <b>WHINE</b> that Ming forgot her phone.",
        "embed_focus": "Hanako said: Yash didn't whine that Ming forgot her <b>PHONE</b>.",
        "target_full": "Scott said: Then what did Yash whine that Ming forgot?",
        "option_bg": "What Ming forgot, according to Yash.",
        "option_fg": "The way Yash said that Ming forgot her phone."
      },
      {
        "item": "10",
        "verb": "murmur",
        "condition": "critical",
        "verb_focus": "Hanako said: Fatima didn't <b>MURMUR</b> that Omar had dinner with his manager.",
        "embed_focus": "Hanako said: Fatima didn't murmur that Omar had dinner with his <b>MANAGER</b>.",
        "target_full": "Scott said: Then who did Fatima murmur that Omar had dinner with?",
        "option_bg": "Who Omar had dinner with, according to Fatima.",
        "option_fg": "The way Fatima said that Omar had dinner with his manager."
      },
      {
        "item": "11",
        "verb": "shriek",
        "condition": "critical",
        "verb_focus": "Hanako said: Igor didn't <b>SHRIEK</b> that Penny won the lottery.",
        "embed_focus": "Hanako said: Igor didn't shriek that Penny won the <b>LOTTERY</b>.",
        "target_full": "Scott said: Then what did Igor shriek that Penny won?",
        "option_bg": "What Penny won, according to Igor.",
        "option_fg": "The way Igor said that Penny won the lottery."
      },
      {
        "item": "12",
        "verb": "moan",
        "condition": "critical",
        "verb_focus": "Hanako said: Chandler didn't <b>MOAN</b> that Tia brought her parents.",
        "embed_focus": "Hanako said: Chandler didn't moan that Tia brought her <b>PARENTS</b>.",
        "target_full": "Scott said: Then who did Chandler moan that Tia brought?",
        "option_bg": "Who Tia brought, according to Chandler.",
        "option_fg": "The way Chandler said that Tia brought her parents."
      }
    ]);

    var critical_say = _.shuffle([
      {
        "item": "301",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Hannah didn't <b>SAY</b> that George emailed the students.",
        "embed_focus": "Hanako said: Hannah didn't say that George emailed the <b>STUDENTS</b>.",
        "target_full": "Scott said: Then who did Hannah say that George emailed?",
        "option_bg": "Who George emailed, according to Hannah.",
        "option_fg": "What Hannah did with the message \"George emailed the students.\""
      },
      {
        "item": "302",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Emily didn't <b>SAY</b> that Paul drafted the letter.",
        "embed_focus": "Hanako said: Emily didn't say that Paul drafted the <b>LETTER</b>.",
        "target_full": "Scott said: Then what did Emily say that Paul drafted?",
        "option_bg": "What Paul drafted, according to Emily.",
        "option_fg": "What Emily did with the message \"Paul drafted the letter.\""
      },
      {
        "item": "303",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Sally didn't <b>SAY</b> that Tony would visit George.",
        "embed_focus": "Hanako said: Sally didn't say that Tony would visit <b>GEORGE</b>.",
        "target_full": "Scott said: Then who did Sally say that Tony would visit?",
        "option_bg": "Who Tony would visit, according to Sally.",
        "option_fg": "What Sally did with the message \"Tony would visit George.\""
      },
      {
        "item": "304",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Tim didn't <b>SAY</b> that Jason drank the beer.",
        "embed_focus": "Hanako said: Tim didn't say that Jason drank the <b>BEER</b>.",
        "target_full": "Scott said: Then what did Tim say that Jason drank?",
        "option_bg": "What Jason drank, according to Tim.",
        "option_fg": "What Tim did with the message \"Jason drank the beer.\""
      },
      {
        "item": "305",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Charlie didn't <b>SAY</b> that Maddie would call her roommate.",
        "embed_focus": "Hanako said: Charlie didn't say that Maddie would call her <b>ROOMMATE</b>.",
        "target_full": "Scott said: Then who did Charlie say that Maddie would call?",
        "option_bg": "Who Maddie would call, according to Charlie.",
        "option_fg": "What Charlie did with the message \"Maddie would call her roommate.\""
      },
      {
        "item": "306",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Kate didn't <b>SAY</b> that Zack broke the plates.",
        "embed_focus": "Hanako said: Kate didn't say that Zack broke the <b>PLATES</b>.",
        "target_full": "Scott said: Then what did Kate say that Zack broke?",
        "option_bg": "What Zack broke, according to Kate.",
        "option_fg": "What Kate did with the message \"Zack broke the plates.\""
      },
      {
        "item": "307",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Ben didn't <b>SAY</b> that Laura ordered pizza.",
        "embed_focus": "Hanako said: Ben didn't say that Laura ordered <b>PIZZA</b>.",
        "target_full": "Scott said: Then what did Ben say that Laura ordered?",
        "option_bg": "What Laura ordered, according to Ben.",
        "option_fg": "What Ben did with the message \"Laura ordered pizza.\""
      },
      {
        "item": "308",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Danny didn't <b>SAY</b> that Jennifer got a cat.",
        "embed_focus": "Hanako said: Danny didn't say that Jennifer got a <b>CAT</b>.",
        "target_full": "Scott said: Then what did Danny say that Jennifer got?",
        "option_bg": "What Jennifer got, according to Danny.",
        "option_fg": "What Danny did with the message \"Jennifer got a cat.\""
      },
      {
        "item": "309",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Nancy didn't <b>SAY</b> that Julian ate the cupcake.",
        "embed_focus": "Hanako said: Nancy didn't say that Julian ate the <b>CUPCAKE</b>.",
        "target_full": "Scott said: Then what did Nancy say that Julian ate?",
        "option_bg": "What Julian ate, according to Nancy.",
        "option_fg": "What Nancy did with the message \"Julian ate the cupcake.\""
      },
      {
        "item": "310",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Jessie didn't <b>SAY</b> that Michelle borrowed the computer.",
        "embed_focus": "Hanako said: Jessie didn't say that Michelle borrowed the <b>COMPUTER</b>.",
        "target_full": "Scott said: Then what did Jessie say that Michelle borrowed?",
        "option_bg": "What Michelle borrowed, according to Jessie.",
        "option_fg": "What Jessie did with the message \"Michelle borrowed the computer.\""
      },
      {
        "item": "311",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Tom didn't <b>SAY</b> that Sarah admired her friend.",
        "embed_focus": "Hanako said: Tom didn't say that Sarah admired her <b>FRIEND</b>.",
        "target_full": "Scott said: Then who did Tom say that Sarah admired?",
        "option_bg": "Who Sarah admired, according to Tom.",
        "option_fg": "What Tom did with the message \"Sarah admired her friend.\""
      },
      {
        "item": "312",
        "verb": "say",
        "condition": "critical",
        "verb_focus": "Hanako said: Noah didn't <b>SAY</b> that Sharon wanted a dog.",
        "embed_focus": "Hanako said: Noah didn't say that Sharon wanted a <b>DOG</b>.",
        "target_full": "Scott said: Then what did Noah say that Sharon wanted?",
        "option_bg": "What Sharon wanted, according to Noah.",
        "option_fg": "What Noah did with the message \"Sharon wanted a dog.\""
      }
    ])

    
    // fillers, a list (randomized) for each condition
    var filler_good = _.shuffle([
      {
        "item": "101",
        "verb": "think",
        "condition": "filler_good",
        "context_full": "Hanako said: Ollie didn't think that Doug would invite the <b>MAYOR</b>.",
        "target_full": "Scott said: Then who did Ollie think that Doug would invite?",
        "option_bg": "Who Doug would invite, according to Ollie.",
        "option_fg": "Who invited the mayor acoording to Ollie.",
        "task": "acceptability"
      },
      {
        "item": "102",
        "verb": "suggest",
        "condition": "filler_good",
        "context_full": "Hanako said: Grace didn't suggest that Karen wrote a <b>BOOK</b>.",
        "target_full": "Scott said: Then what did Grace suggest that Karen wrote?",
        "option_bg": "What Karen wrote, according to Grace.",
        "option_fg": "Who wrote a book, according to Grace.",
        "task": "acceptability"
      },
      {
        "item": "103",
        "verb": "believe",
        "condition": "filler_good",
        "context_full": "Hanako said: Jean didn't believe that Grant adopted a <b>KITTEN</b>.",
        "target_full": "Scott said: Then what did Jean believe that Grant adopted?",
        "option_bg": "What Grant adopted, according to Jean.",
        "option_fg": "Who adopted the kitten, according to Jean.",
        "task": "acceptability"
      },
      {
        "item": "104",
        "verb": "hope",
        "condition": "filler_good",
        "context_full": "Hanako said: <b>MATTHEW</b> didn't hope that Dana brought a gift.",
        "target_full": "Scott said: Then who hoped that Dana brought a gift?",
        "option_bg": "Who hoped that Dana brought a gift.",
        "option_fg": "What Dana brought, according to Matthew.",
        "task": "acceptability"
      },
      {
        "item": "105",
        "verb": "guess",
        "condition": "filler_good",
        "context_full": "Hanako said: <b>JULIE</b> didn't guess that Charlie received a message.",
        "target_full": "Scott said: Then who guessed that Charlie received a message?",
        "option_bg": "Who guessed that Charlie received a message.",
        "option_fg": "What Charlie received, according to Julie.",
        "task": "acceptability"
      },
      {
        "item": "106",
        "verb": "reveal",
        "condition": "filler_good",
        "context_full": "Hanako said: <b>JESS</b> didn't reveal that Todd went out with Betty.",
        "target_full": "Scott said: Then who revealed that Todd went out with Betty?",
        "option_bg": "Who revealed that Todd went out with Betty.",
        "option_fg": "Who Todd went out with, according to Jess.",
        "task": "acceptability"
      }
    ]);

    var filler_bad = _.shuffle([
      {
        "item": "107",
        "verb": "think",
        "condition": "filler_bad",
        "context_full": "Hanako said: Helen didn't think that Rob and <b>MARC</b> cut the bread. ",
        "target_full": "Scott said: Then who did Helen think that Rob and cut the bread?",
        "option_bg": "Who else cut the bread other than Rob, according to Helen.",
        "option_fg": "Who thought that Marc cut the bread.",
        "task": "acceptability"
      },
      {
        "item": "108",
        "verb": "suspect",
        "condition": "filler_bad",
        "context_full": "Hanako said: Tim didn't suspect that Liz and <b>JASON</b> drank the beer. ",
        "target_full": "Scott said: Then who did Tim suspect that Liz and drank the beer?",
        "option_bg": "Who else drank the beer other than Liz, according to Tim.",
        "option_fg": "Who suspected that Jason drank the beer.",
        "task": "acceptability"
      },
      {
        "item": "109",
        "verb": "believe",
        "condition": "filler_bad",
        "context_full": "Hanakko said: Larry didn't believe that Karl and <b>NICK</b> cooked dinner.",
        "target_full": "Scott said: Then who did Larry believe that Karl and cooked dinner?",
        "option_bg": "Who else cooked dinner other than Karl, according to Larry.",
        "option_fg": "Who believed that Nick cooked dinner.",
        "task": "acceptability"
      },
      {
        "item": "110",
        "verb": "imply",
        "condition": "filler_bad",
        "context_full": "Hanako said: Rui didn't imply that <b>CAROL</b>'s brother signed the contract.",
        "target_full": "Scott said: Then who did Rui imply that the brother of signed the contract?",
        "option_bg": "Whose brother signed the contract, according to Rui.",
        "option_fg": "What Carol's brother signed, according to Rui.",
        "task": "acceptability"
      },
      {
        "item": "111",
        "verb": "hope",
        "condition": "filler_bad",
        "context_full": "Hanako said: Frankie didn't hope that the agent of the <b>GUITARIST</b> would show up. ",
        "target_full": "Scott said: Then who did Frankie hope that the agent of would show up?",
        "option_bg": "Whose agent would show up, as Frankie hoped.",
        "option_fg": "What the agent of the guitarist did, according to Frankie.",
        "task": "acceptability"
      },
      {
        "item": "112",
        "verb": "guess",
        "condition": "filler_bad",
        "context_full": "Hanako said: Jessie didn't guess that the assistant to the <b>DIRECTOR</b> would give the presentation.",
        "target_full": "Scott said: Then who did Jessie guess that the assistant to would give the presentation?",
        "option_bg": "Whose assistant would give the presentation, as Jessie guessed.",
        "option_fg": "What the assistant to the director would do, according to Jessie.",
        "task": "acceptability"
      }
    ])

    // split critical items into half verb half embedded focus
    say_embed_focus = [];
    say_verb_focus = [];
    mos_embed_focus = [];
    mos_verb_focus = [];

    // 2(mos vs. say) x 2(embed vs. verb focus), and should be balanced

    for (var i=0; i<critical_mos.length/2; i++) {
        var stim = critical_mos[i];
        stim.condition = "verb_focus";
        stim.context_full = stim["verb_focus"];
        mos_verb_focus.push(stim)
    }
    for (var j=critical_mos.length/2; j<critical_mos.length; j++) {
        var stim = critical_mos[j];
        stim.condition = "embed_focus";
        stim.context_full = stim["embed_focus"];
        mos_embed_focus.push(stim)
    }

    for (var i=0; i<critical_say.length/2; i++) {
        var stim = critical_say[i];
        stim.condition = "verb_focus";
        stim.context_full = stim["verb_focus"];
        say_verb_focus.push(stim)
    }
    for (var j=critical_say.length/2; j<critical_say.length; j++) {
        var stim = critical_say[j];
        stim.condition = "embed_focus";
        stim.context_full = stim["embed_focus"];
        say_embed_focus.push(stim)
    }

    num_blocks = 6
    num_per_block = 6
    total_blocks = []
    exp.stims_block = []
    for (var i=0; i<num_blocks; i++) {
        // each block will have four critical items(one embed_focus, one verb_focus for say and mos) 
        // and four filler items(one in each condition)
        var block = [mos_embed_focus.pop(), mos_verb_focus.pop(), say_embed_focus.pop(), say_verb_focus.pop(),
          filler_good.pop(), filler_bad.pop()];
        // randomize the items within each block -> shuffle items to make sure different items will get different tasks
        block = _.shuffle(block);
        // split into half acceptability and half backgroundedness inside each block
        for (var j=0; j<block.length/2; j++) {
            block[j].task = "acceptability";
        }
        for (var j=block.length/2; j<block.length; j++) {
            block[j].task = "backgroundedness";
        }
        // shuffle again to ensure the tasks are randomized as well
        block = _.shuffle(block);
        console.log(block)
        total_blocks.push(block);
    }
    // randomize the order of blocks
	  total_blocks = _.shuffle(total_blocks); 
    console.log(total_blocks);

    // add block id (after shuffling) and add to exp.stims_block
    for (var b=0; b<num_blocks; b++) {
      var block =  total_blocks[b];
      for (var item=0; item<num_per_block; item++) {
        var stim = block[item];
        stim.block_id = b;
        exp.stims_block.push(jQuery.extend(true, {}, stim));
      }
    }

    // alternatively, flat and then add them to exp.stims_block
    // total_blocks = total_blocks.flat();

    // for (var k=0; k<num_per_block * num_blocks; k++) {
    //     var stim = total_blocks[k];
    //     exp.stims_block.push(jQuery.extend(true, {}, stim));
    // }

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
    exp.structure=["i0", "practice_acceptability_good_1","post_practice_acceptability_good_1", 
    "practice_bg_good_2", "post_practice_bg_good_2",
    "practice_acceptability_bad_2", "post_practice_acceptability_bad_2", 
    "practice_bg_bad_1", "post_practice_bg_bad_1",
    "last_reminder", "block1", 'questionaire', 'finished'];
    // console.log(exp.structure);

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 1 + 8 + 1 + 36 + 1; 
    $(".nQs").html(exp.nQs);

    $('.slide').hide(); //hide everything

    $("#start_button").click(function() {
        exp.go();
    });

    exp.go(); //show first slide
}
