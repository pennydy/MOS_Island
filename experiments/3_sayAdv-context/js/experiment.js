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
            var context = "Hanako said: Fiona didn't buy <strong>PINEAPPLES</strong>.";
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
          var context = "Hanako said: Vera didn't <strong>DRIVE</strong> to Michigan.";
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
          var context = "Hanako said: Prisha doesn't speak <strong>KOREAN</strong>.";
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
            var context = "Hanako said: Hank didn't buy the <strong>RED</strong> car.";
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
    
    var critical_verbs = _.shuffle([
        {
            "item": "1",
            "verb": "softly",
            "condition": "critical",
            "verb_focus": "Hanako said: John didn't say <strong>SOFTLY</strong> that Mary met with the lawyer.",
            "embed_focus": "Hanako said: John didn't say softly that Mary met with the <strong>LAWYER</strong>.",
            "target_full": "Scott said: Then who did John say softly that Mary met with?",
            "option_bg": "Who Mary met with, according to John.",
            "option_fg": "The way John said that Mary met with the lawyer."
          },
          {
            "item": "2",
            "verb": "quietly",
            "condition": "critical",
            "verb_focus": "Hanako said: Emma didn't say <strong>QUIETLY</strong> that Kevin lost the keys.",
            "embed_focus": "Hanako said: Emma didn't say quietly that Kevin lost the <strong>KEYS</strong>.",
            "target_full": "Scott said: Then what did Emma say quietly that Kevin lost?",
            "option_bg": "What Kevin lost, according to Emma.",
            "option_fg": "The way Emma said that Kevin lost the keys."
          },
          {
            "item": "3",
            "verb": "loudly",
            "condition": "critical",
            "verb_focus": "Hanako said: Howard didn't say <strong>LOUDLY</strong> that Alex bought a birthday cake.",
            "embed_focus": "Hanako said: Howard didn't say loudly that Alex bought a <strong>BIRTHDAY CAKE</strong>.",
            "target_full": "Scott said: Then what did Howard say loudly that Alex bought?",
            "option_bg": "What Alex bought, according to Howard.",
            "option_fg": "The way Howard said that Alex bought a birthday cake."
          },
          {
            "item": "4",
            "verb": "bluntly",
            "condition": "critical",
            "verb_focus": "Hanako said: Laura didn't say <strong>BLUNTLY</strong> that Brandon broke his laptop.",
            "embed_focus": "Hanako said: Laura didn't say bluntly that Brandon broke his <strong>LAPTOP</strong>.",
            "target_full": "Scott said: Then what did Laura say bluntly that Brandon broke?",
            "option_bg": "What Brandon broke, according to Laura.",
            "option_fg": "The way Laura said that Brandon broke his laptop."
          },
          {
            "item": "5",
            "verb": "cheerfully",
            "condition": "critical",
            "verb_focus": "Hanako said: Bill didn't say <strong>CHEERFULLY</strong> that Dan knew the professor.",
            "embed_focus": "Hanako said: Bill didn't say cheerfully that Dan knew the <strong>PROFESSOR</strong>.",
            "target_full": "Scott said: Then who did Bill say cheerfully that Dan knew?",
            "option_bg": "Who Dan knew, according to Bill.",
            "option_fg": "The way Bill said that Dan knew the professor."
          },
          {
            "item": "6",
            "verb": "wearily",
            "condition": "critical",
            "verb_focus": "Hanako said: Amy didn't say <strong>WEARILY</strong> that Charlie saw the robber.",
            "embed_focus": "Hanako said: Amy didn't say wearily that Charlie saw the <strong>ROBBER</strong>.",
            "target_full": "Scott said: Then who did Amy say wearily that Charlie saw?",
            "option_bg": "Who Charlie saw, according to Amy.",
            "option_fg": "The way Amy said that Charlie saw the robber."
          },
          {
            "item": "7",
            "verb": "sternly",
            "condition": "critical",
            "verb_focus": "Hanako said: Jake didn't say <strong>STERNLY</strong> that Yumi found the wallet.",
            "embed_focus": "Hanako said: Jake didn't say sternly that Yumi found the <strong>WALLET</strong>.",
            "target_full": "Scott said: Then what did Jake say sternly that Yumi found?",
            "option_bg": "What Yumi found, according to Jake.",
            "option_fg": "The way Jake said that Yumi found the wallet."
          },
          {
            "item": "8",
            "verb": "gently",
            "condition": "critical",
            "verb_focus": "Hanako said: Ashley didn't say <strong>GENTLY</strong> that Hasan talked to the detectives.",
            "embed_focus": "Hanako said: Ashley didn't say gently that Hasan talked to the <strong>DETECTIVES</strong>.",
            "target_full": "Scott said: Then who did Ashley say gently that Hasan talked to?",
            "option_bg": "Who Hasan talked to, according to Ashley.",
            "option_fg": "The way Ashley said that Hasan talked to the detectives."
          },
          {
            "item": "9",
            "verb": "wistfully",
            "condition": "critical",
            "verb_focus": "Hanako said: Yash didn't say <strong>WISTFULLY</strong> that Ming forgot her phone.",
            "embed_focus": "Hanako said: Yash didn't say wistfully that Ming forgot her <strong>PHONE</strong>.",
            "target_full": "Scott said: Then what did Yash say wistfully that Ming forgot?",
            "option_bg": "What Ming forgot, according to Yash.",
            "option_fg": "The way Yash said that Ming forgot her phone."
          },
          {
            "item": "10",
            "verb": "ruefully",
            "condition": "critical",
            "verb_focus": "Hanako said: Fatima didn't say <strong>RUEFULLY</strong> that Omar had dinner with his manager.",
            "embed_focus": "Hanako said: Fatima didn't say ruefully that Omar had dinner with his <strong>MANAGER</strong>.",
            "target_full": "Scott said: Then who did Fatima say ruefully that Omar had dinner with?",
            "option_bg": "Who Omar had dinner with, according to Fatima.",
            "option_fg": "The way Fatima said that Omar had dinner with his manager."
          },
          {
            "item": "11",
            "verb": "calmly",
            "condition": "critical",
            "verb_focus": "Hanako said: Igor didn't say <strong>CALMLY</strong> that Penny won the lottery.",
            "embed_focus": "Hanako said: Igor didn't say calmly that Penny won the <strong>LOTTERY</strong>.",
            "target_full": "Scott said: Then what did Igor say calmly that Penny won?",
            "option_bg": "What Penny won, according to Igor.",
            "option_fg": "The way Igor said that Penny won the lottery."
          },
          {
            "item": "12",
            "verb": "dryly",
            "condition": "critical",
            "verb_focus": "Hanako said: Chandler didn't say <strong>DRYLY</strong> that Tia brought her parents.",
            "embed_focus": "Hanako said: Chandler didn't say dryly that Tia brought her <strong>PARENTS</strong>.",
            "target_full": "Scott said: Then who did Chandler say dryly that Tia brought?",
            "option_bg": "Who Tia brought, according to Chandler.",
            "option_fg": "The way Chandler said that Tia brought her parents."
          }
        ]);

    
    // fillers, a list (randomized) for each condition
    var filler_good_1 = _.shuffle([
      {
        "item": "101",
        "verb": "say",
        "condition": "filler_good_1",
        "context_full": "Hanako said: Hannah didn't say that George emailed the <strong>STUDENTS</strong>.",
        "target_full": "Scott said: Then who did Hannah say that George emailed?",
        "option_bg": "Who George emailed, according to Hannah.",
        "option_fg": "Who emailed the students, according to Hannah."
      },
      {
        "item": "102",
        "verb": "think",
        "condition": "filler_good_1",
        "context_full": "Hanako said: Ollie didn't think that Doug would invite the <strong>MAYOR</strong>.",
        "target_full": "Scott said: Then who did Ollie think that Doug would invite?",
        "option_bg": "Who Doug would invite, according to Ollie.",
        "option_fg": "Who invited the mayor, according to Ollie."
      },
      {
        "item": "103",
        "verb": "suspect",
        "condition": "filler_good_1",
        "context_full": "Hanako said: Nancy didn't suspect that Julian ate the <strong> CUPCAKE</strong>.",
        "target_full": "Scott said: Then what did Nancy suspect that Julian ate?",
        "option_bg": "What Julian ate, according to Nancy.",
        "option_fg": "Who ate the cupcake, according to Nancy."
      },
      {
        "item": "104",
        "verb": "suggest",
        "condition": "filler_good_1",
        "context_full": "Hanako said: Grace didn't sugguest that Karen wrote a <strong>BOOK</strong>.",
        "target_full": "Scott said: Then what did Grace suggst that Karen wrote?",
        "option_bg": "What Karen wrote, according to Grace.",
        "option_fg": "Who wrote a book, according to Grace."
      },
      {
        "item": "105",
        "verb": "believe",
        "condition": "filler_good_1",
        "context_full": "Hanako said: Danny didn't believe that Jennifer got a <strong>CAT</strong>.",
        "target_full": "Scott said: Then what did Danny believe that Jennifer got?",
        "option_bg": "What Jennifer got, according to Danny.",
        "option_fg": "Who got a cat, according to Danny."
      },
      {
        "item": "106",
        "verb": "expect",
        "condition": "filler_good_1",
        "context_full": "Hanako said: Charlie didn't expect that Maddie would call her <strong>ROOMMATE</strong>.",
        "target_full": "Scott said: Then who did Charlie expect that Maddie would call?",
        "option_bg": "Who Maddie would call, according to Charlie.",
        "option_fg": "Who would call Maddie's roommate, according to Charlie."
      }
    ]);
    
    var filler_good_2 = _.shuffle([
      {
        "item": "107",
        "verb": "imply",
        "condition": "filler_good_2",
        "context_full": "Hanako said: <strong>RONALD</strong> didn't imply that Jacy rented the truck.",
        "target_full": "Scott said: Then who implied that Jacy rented the truck?",
        "option_bg": "Who implied that Jacy rented the truck.",
        "option_fg": "What Jacy rented, according to Ronald."
      },
      {
        "item": "108",
        "verb": "hope",
        "condition": "filler_good_2",
        "context_full": "Hanako said: <strong>MATTHEW</strong> didn't hope that Dana brought a gift.",
        "target_full": "Scott said: Then who hoped that Dana brought a gift?",
        "option_bg": "Who hoped that Dana brought a gift.",
        "option_fg": "What Dana brought, according to Matthew."
      },
      {
        "item": "109",
        "verb": "insist",
        "condition": "filler_good_2",
        "context_full": "Hanako said: <strong>KATE</strong> didn't insist that Zack broke the plates.",
        "target_full": "Scott said: Then who insisted that Zack broke the plates?",
        "option_bg": "Who insisted that Zack broke the plates.",
        "option_fg": "What Zack broke, according to Kate."
      },
      {
        "item": "110",
        "verb": "guess",
        "condition": "filler_good_2",
        "context_full": "Hanako said: <strong>JULIAN</strong> didn't guess that Charlie received a message.",
        "target_full": "Scott said: Then who guessed that Charlie received a message?",
        "option_bg": "Who guessed that Charlie received a message.",
        "option_fg": "What Charlie received, according to Julian."
      },
      {
        "item": "111",
        "verb": "reveal",
        "condition": "filler_good_2",
        "context_full": "Hanako said: <strong>JESS</strong> didn't reveal that Todd went out with Betty.",
        "target_full": "Scott said: Then who revealed that Todd went out with Betty?",
        "option_bg": "Who revealed that Todd went out with Betty.",
        "option_fg": "Who Todd went out with, according to Jess."
      },
      {
        "item": "112",
        "verb": "expect",
        "condition": "filler_good_2",
        "context_full": "Hanako said: <strong>SALLY</strong> didn't expect that Tony would visit George.",
        "target_full": "Scott said: Then who expected that Tony would visit George?",
        "option_bg": "Who expected that Tony would visit George.",
        "option_fg": "Who Tony would visit, according to Sally."
      }
    ]);

    var filler_bad_1 = _.shuffle([
      {
        "item": "113",
        "verb": "say",
        "condition": "filler_bad_1",
        "context_full": "Hanako said: Emily didn't say that Kevin and <strong>PAUL</strong> drafted the letter.",
        "target_full": "Scott said: Then who did Emily say that Kevin and drafted the letter?",
        "option_bg": "Who else drafted the letter other than Kevin, according to Emily.",
        "option_fg": "Who said that Kevin and Paul drafted the letter."
      },
      {
        "item": "114",
        "verb": "think",
        "condition": "filler_bad_1",
        "context_full": "Hanako said: Helen didn't think that Rob and <strong>MARC</strong> cut the bread. ",
        "target_full": "Scott said: Then who did Helen think that Rob and cut the bread?",
        "option_bg": "Who else cut the bread other than Rob, according to Helen.",
        "option_fg": "Who thought that Rob and Marc cut the bread."
      },
      {
        "item": "115",
        "verb": "suspect",
        "condition": "filler_bad_1",
        "context_full": "Hanako said: Tim didn't suspect that Liz and <strong>JASON</strong> drank the beer. ",
        "target_full": "Scott said: Then who did Tim suspect that Liz and drank the beer?",
        "option_bg": "Who else drank the beer other than Liz, according to Tim.",
        "option_fg": "Who suspected that Liz and Jason drank the beer."
      },
      {
        "item": "116",
        "verb": "suggest",
        "condition": "filler_bad_1",
        "context_full": "Hanako said: Tony didn't suggest that Frank and <strong>LISA</strong> were in the office.",
        "target_full": "Scott said: Then who did Tony suggest that Frank and were in the office?",
        "option_bg": "Who else was in the office other than Frank, according to Tony.",
        "option_fg": "Who suggested that Frank and Lisa were in the office."
      },
      {
        "item": "117",
        "verb": "believe",
        "condition": "filler_bad_1",
        "context_full": "Hanako said: Larry didn't believe that Karl and <strong>NICK</strong> cooked dinner.",
        "target_full": "Scott said: Then who did Larry believe that Karl and cooked dinner?",
        "option_bg": "Who else cooked dinner other than Karl, according to Larry.",
        "option_fg": "Who believed that Karl and Nick cooked dinner."
      },
      {
        "item": "118",
        "verb": "expect",
        "condition": "filler_bad_1",
        "context_full": "Hanako said: Ben didn't expect that Judith and <strong>LAURA</strong> would order pizza.",
        "target_full": "Scott said: Then who did Ben expect that Judith and would order pizza?",
        "option_bg": "Who else would order pizza other than Judith, according to Ben.",
        "option_fg": "Who expected that Judith and Laura would order pizza."
      }
    ]);
    
    var filler_bad_2 = _.shuffle([
      {
        "item": "119",
        "verb": "imply",
        "condition": "filler_bad_2",
        "context_full": "Hanako said: Rui didn't imply that <strong>CAROL</strong>'s brother signed the contract.",
        "target_full": "Scott said: Then who did Rui imply that the brother of signed the contract?",
        "option_bg": "Whose brother signed the contract, according to Rui.",
        "option_fg": "What Carol's brother signed, according to Rui."
      },
      {
        "item": "120",
        "verb": "hope",
        "condition": "filler_bad_2",
        "context_full": "Hanako said: Frankie didn't hope that the agent of the <strong>GUITARIST</strong> would show up. ",
        "target_full": "Scott said: Then who did Frankie hope that the agent of would show up?",
        "option_bg": "Whose agent would show up, according to Frankie.",
        "option_fg": "What the agent of the guitarist did, according to Frankie."
      },
      {
        "item": "121",
        "verb": "insist",
        "condition": "filler_bad_2",
        "context_full": "Hanako said: Ted didn't insist that <strong>EVA</strong>'s uncle should stay for dinner. ",
        "target_full": "Scott said: Then who did Ted insist that the uncle of should stay for dinner?",
        "option_bg": "Whose uncle should stay for dinner, according to Ted.",
        "option_fg": "What Eva's uncle should do, according to Ted."
      },
      {
        "item": "122",
        "verb": "guess",
        "condition": "filler_bad_2",
        "context_full": "Hanako said: Jessie didn't guess that the assistant to the <strong>DIRECTOR</strong> would give the presentation.",
        "target_full": "Scott said: Then who did Jessie guess that the assistant to would give the presentation?",
        "option_bg": "Whose assistant would give the presentation, according to Jessie.",
        "option_fg": "What the assistant to the director would do, according to Jessie."
      },
      {
        "item": "123",
        "verb": "reveal",
        "condition": "filler_bad_2",
        "context_full": "Hanako said: Donald didn't reveal that <strong>KRISHNA</strong>'s parents were moving to Texas.",
        "target_full": "Scott said: Then who did Donald reveal that the parents of were moving to Texas?",
        "option_bg": "Whose parents were moving to Texas, according to Donald.",
        "option_fg": "Where Krishna's parents were moving to, according to Donald."
      },
      {
        "item": "124",
        "verb": "expect",
        "condition": "filler_bad_2",
        "context_full": "Hanako said: Jamal didn't expect that the book about <strong>ASTRONOMY</strong> was on the shelf.",
        "target_full": "Scott said: Then what did Jamal expect that the book about was on the shelf?",
        "option_bg": "Which book was on the shelf, according to Jamal.",
        "option_fg": "Where the book about astronomy was, according to Jamal."
      }
    ])

    // split critical items into half verb half embedded focus
    embed_focus = [];
    verb_focus = [];

    for (var i=0; i<critical_verbs.length/2; i++) {
        var stim = critical_verbs[i];
        stim.condition = "verb_focus";
        stim.context_full = stim["verb_focus"];
        verb_focus.push(stim)
    }
    for (var j=critical_verbs.length/2; j<critical_verbs.length; j++) {
        var stim = critical_verbs[j];
        stim.condition = "embed_focus";
        stim.context_full = stim["embed_focus"];
        embed_focus.push(stim)
    }

    num_blocks = 6
    num_per_block = 6
    total_blocks = []
    exp.stims_block = []
    for (var i=0; i<num_blocks; i++) {
        // each block will have two critical items(one embed_focus, one verb_focus) and four filler items(one in each condition)
        // num_per_block == block.length == 6
        var block = [embed_focus.pop(), verb_focus.pop(),filler_good_1.pop(), filler_good_2.pop(), filler_bad_1.pop(), filler_bad_2.pop()];
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
