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
            var context = "Hanako said: Fiona didn't buy pineapple.";
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
            "context": exp.context,  // check if the context matches with the condition
            "acceptability_rating" : exp.sliderPost,
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
          var context = "Hanako said: Prisha doesn't speak Korean.";
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
            "context": exp.context,  // check if the context matches with the condition
            "acceptability_rating" : exp.sliderPost,
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
        },
        
        present_handle : function(stim) {
            $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
            this.stim = stim;
            this.stim.trial_start = Date.now();      
            $(".slider_err").hide();   
            
            var context = "Hanako said: " + this.stim.context;
            var target_question = "Scott said: " + this.stim.target_question;
            $(".context").html(context);
            exp.context = context;
            // console.log("context: "+exp.context);
            $(".target").html(target_question);
            $(".slider_table").hide(); // hide the slider

            exp.acceptability_sliderPost = null; // remove the previous rating
            this.init_sliders();
            exp.acceptability_sliderPost = null; // remove rating -> or else it will use the default (0.5)
            exp.question = "How natural/acceptable does <u>Scott</u>'s question sound?";
            $(".slider_table").show();

            $(".continue_button").show(); // show the belief button
            $(".question").html(exp.question);

            console.log(this.stim.verb, this.stim.condition)
  
        },
        
        button : function() {
          console.log(exp.acceptability_sliderPost);
          if (exp.acceptability_sliderPost == null) {
              $(".slider_err").show();
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
              "trial_num" : order,
              "item_id" : this.stim.item,
              "block_id" : this.stim.block_id,
              "condition" : this.stim.condition,
              "verb": this.stim.verb,
              "context": exp.context,  // check if the context matches with the condition
              "acceptability_rating" : exp.acceptability_sliderPost,
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

  var names = _.shuffle(["John", "Emma", "Howard", "Laura", "Bill", "Amy", "Jake", "Ashley", 
    "Yash", "Fatima", "Igor", "Chandler", "Hannah", "Ollie", "Nancy", "Grace", "Danny", "Charlie",
    "Ronald", "Matthew", "Kate", "Julian", "Jess", "Sally", "Emily", "Helen", "Tim", "Tony", "Larry",
    "Ben", "Rui", "Frankie", "Ted", "Jessie", "Donald", "Jamal"])
    
  var adverbs = _.shuffle(['softly', 'quietly', 'loudly', 'bluntly', 'cheerfully', 'wearily', 
  'sternly', 'gently', 'wistfully', 'ruefully', 'calmly', 'dryly'])    
    
  var filler_verbs = _.shuffle([
    'speculate', 'think', 'suspect', 'suggest', 'believe', 'expect', 'imply', 'hope', 
    'insist', 'guess', 'reveal', 'expect'
  ])

  var verbs_past_tense = {
    "speculate": "speculated",
    "think": "thought",
    "suspect": "suspected",
    "suggest": "suggested",
    "believe": "believed",
    "expect": "expected",
    "imply": "implied",
    "hope": "hoped",
    "insist": "insisted",
    "guess": "guessed",
    "reveal": "revealed",
    "expect": "expected"
  }


  // a shuffled list of critical items
  var critical_items = _.shuffle([
    {
      "item": "1",
      "condition": "critical",
      "context": "Mary met with the lawyer.",
      "target_question": "Then who did SPEAKER VERB that Mary met with?"
    },
    {
      "item": "2",
      "condition": "critical",
      "context": "Kevin lost the keys.",
      "target_question": "Then what did SPEAKER VERB that Kevin lost?"
    },
    {
      "item": "3",
      "condition": "critical",
      "context": "Alex bought a birthday cake.",
      "target_question": "Then what did SPEAKER VERB that Alex bought?"
    },
    {
      "item": "4",
      "condition": "critical",
      "context": "Brandon broke his laptop.",
      "target_question": "Then what did SPEAKER VERB that Brandon broke?"
    },
    {
      "item": "5",
      "condition": "critical",
      "context": "Dan knew the professor.",
      "target_question": "Then who did SPEAKER VERB that Dan knew?"
    },
    {
      "item": "6",
      "condition": "critical",
      "context": "Charlie saw the robber.",
      "target_question": "Then who did SPEAKER VERB that Charlie saw?"
    },
    {
      "item": "7",
      "condition": "critical",
      "context": "Yumi found the wallet.",
      "target_question": "Then what did SPEAKER VERB that Yumi found?"
    },
    {
      "item": "8",
      "condition": "critical",
      "context": "Hasan talked to the detectives.",
      "target_question": "Then who did SPEAKER VERB that Hasan talked to?"
    },
    {
      "item": "9",
      "condition": "critical",
      "context": "Ming forgot her phone. ",
      "target_question": "Then what did SPEAKER VERB that Ming forgot?"
    },
    {
      "item": "10",
      "condition": "critical",
      "context": "Omar had dinner with his manager.",
      "target_question": "Then who did SPEAKER VERB that Omar had dinner with?"
    },
    {
      "item": "11",
      "condition": "critical",
      "context": "Penny won the lottery.",
      "target_question": "Then what did SPEAKER VERB that Penny won?"
    },
    {
      "item": "12",
      "condition": "critical",
      "context": "Tia brought her parents.",
      "target_question": "Then who did SPEAKER VERB that Tia brought?"
    }
      ]);
    
    // fillers, a list (randomized) for each condition
    var filler_good_items = _.shuffle([
      {
        "item": "101",
        "condition": "filler_good",
        "context": "George emailed the students.",
        "target_question": "Then who VERB that George emailed the students?"
      },
      {
        "item": "102",
        "condition": "filler_good",
        "context": "Doug would invite the mayor.",
        "target_question": "Then who VERB that Doug would invite the mayor?"
      },
      {
        "item": "103",
        "condition": "filler_good",
        "context": "Julian ate the cupcake.",
        "target_question": "Then who VERB that Julian ate the cupcake?"
      },
      {
        "item": "104",
        "condition": "filler_good",
        "context": "Karen wrote a book.",
        "target_question": "Then who VERB that Karen wrote a book?"
      },
      {
        "item": "105",
        "condition": "filler_good",
        "context": "Jennifer got a cat.",
        "target_question": "Then who VERB that Jennifer got a cat?"
      },
      {
        "item": "106",
        "condition": "filler_good",
        "context": "Maddie would call her roommate.",
        "target_question": "Then who VERB that Maddie would call her roommate?"
      },
      {
        "item": "107",
        "condition": "filler_good",
        "context": "Jacy rented the truck.",
        "target_question": "Then who VERB that Jacy rented the truck?"
      },
      {
        "item": "108",
        "condition": "filler_good",
        "context": "Dana brought a gift.",
        "target_question": "Then who VERB that Dana brought a gift?"
      },
      {
        "item": "109",
        "condition": "filler_good",
        "context": "Zack broke the plates.",
        "target_question": "Then who VERB that Zack broke the plates?"
      },
      {
        "item": "110",
        "condition": "filler_good",
        "context": "Charlie received a message.",
        "target_question": "Then who VERB that Charlie received a message?"
      },
      {
        "item": "111",
        "condition": "filler_good",
        "context": "Todd went out with Betty.",
        "target_question": "Then who VERB that Todd went out with Betty?"
      },
      {
        "item": "112",
        "condition": "filler_good",
        "context": "Tony would visit George.",
        "target_question": "Then who VERB that Tony would visit George?"
      }
    ]);

    var filler_bad_items = _.shuffle([
      {
        "item": "113",
        "condition": "filler_bad",
        "context": "Kevin and Paul drafted the letter.",
        "target_question": "Then who did SPEAKER VERB that Kevin and drafted the letter?"
      },
      {
        "item": "114",
        "condition": "filler_bad",
        "context": "Rob and Marc cut the bread. ",
        "target_question": "Then who did SPEAKER VERB that Rob and cut the bread?"
      },
      {
        "item": "115",
        "condition": "filler_bad",
        "context": "Liz and Jason drank the beer. ",
        "target_question": "Then who did SPEAKER VERB that Liz and drank the beer?"
      },
      {
        "item": "116",
        "condition": "filler_bad",
        "context": "Frank and Lisa were in the office.",
        "target_question": "Then who did SPEAKER VERB that Frank and were in the office?"
      },
      {
        "item": "117",
        "condition": "filler_bad",
        "context": "Karl and Nick cooked dinner.",
        "target_question": "Then who did SPEAKER VERB that Karl and cooked dinner?"
      },
      {
        "item": "118",
        "condition": "filler_bad",
        "context": "Judith and Laura would order pizza.",
        "target_question": "Then who did SPEAKER VERB that Judith and would order pizza?"
      },
      {
        "item": "119",
        "condition": "filler_bad",
        "context": "Carol's brother signed the contract.",
        "target_question": "Then who did SPEAKER VERB that the brother of signed the contract?"
      },
      {
        "item": "120",
        "condition": "filler_bad",
        "context": "the agent of the guitarist would show up.",
        "target_question": "Then who did SPEAKER VERB that the agent of would show up?"
      },
      {
        "item": "121",
        "condition": "filler_bad",
        "context": "Eva's uncle should stay for dinner.",
        "target_question": "Then who did SPEAKER VERB that the uncle of should stay for dinner?"
      },
      {
        "item": "122",
        "condition": "filler_bad",
        "context": "the assistant to the director would give the presentation.",
        "target_question": "Then who did SPEAKER VERB that the assistant to would give the presentation?"
      },
      {
        "item": "123",
        "condition": "filler_bad",
        "context": "Krisha's parents were moving to Texas.",
        "target_question": "Then who did SPEAKER VERB that the parents of were moving to Texas?"
      },
      {
        "item": "124",
        "condition": "filler_bad",
        "context": "the book about astronomy was on the shelf.",
        "target_question": "Then what did SPEAKER VERB that the book about was on the shelf?"
      }
    ])

    // split critical items into half verb half embedded focus
    say = [];
    say_adv = [];
    filler_good = []
    filler_bad = []

    for (var i=0; i<critical_items.length/2; i++) {
        var stim = critical_items[i];
        var name = names.pop();
        stim.condition = "say";
        stim.verb = "say";
        stim.name = name;
        var target_question = stim["target_question"].replace("SPEAKER", name);
        target_question = target_question.replace("VERB", stim.verb);
        var context = name + " didn't say that " + stim["context"];
        stim.target_question = target_question;
        stim.context = context;
        say.push(stim)
    }
    for (var j=critical_items.length/2; j<critical_items.length; j++) {
        var stim = critical_items[j];
        var name = names.pop();
        var adv = adverbs[j];
        stim.condition = "say_adv";
        stim.verb = "say " + adv;
        stim.name = name;
        var target_question = stim["target_question"].replace("SPEAKER", name);
        target_question = target_question.replace("VERB", stim.verb);
        var context = name + " didn't say " + adv + " that " + stim["context"];
        stim.target_question = target_question;
        stim.context = context;
        say_adv.push(stim)
    }

    for (var k=0; k<filler_good_items.length; k++){
      var stim = filler_good_items[k];
      var name = names.pop();
      stim.verb = filler_verbs[k];
      var verb = verbs_past_tense[stim.verb]; // use the past tense
      stim.name = name;
      var target_question = stim["target_question"].replace("VERB", verb);
      var context = name + " didn't " + stim.verb + " that " + stim["context"];
      stim.target_question = target_question;
      stim.context = context;
      filler_good.push(stim)
    }
    filler_good = _.shuffle(filler_good)

    for (var k=0; k<filler_bad_items.length; k++){
      var stim = filler_bad_items[k];
      var name = names.pop();
      var verb = filler_verbs[k];
      stim.verb = verb;
      stim.name = name;
      var target_question = stim["target_question"].replace("VERB", stim.verb);
      target_question = target_question.replace("SPEAKER", name);
      var context = name + " didn't " + stim.verb + " that " + stim["context"];
      stim.target_question = target_question;
      stim.context = context;
      filler_bad.push(stim)
    }
    filler_bad = _.shuffle(filler_bad)


    num_blocks = 6;
    num_per_block = 6;
    total_blocks = [];
    exp.stims_block = [];

    for (var i=0; i<num_blocks; i++) {
      // each block will have two critical items(one say, one say_adv) and four filler items(one in each condition)
      // num_per_block == block.length == 6
      var block = [say.pop(), say_adv.pop(), filler_good.pop(), filler_bad.pop(), filler_good.pop(), filler_bad.pop()];
      // randomize the items within each block -> shuffle items to make sure different items will get different tasks
      block = _.shuffle(block);
      total_blocks.push(block);
    }
    // randomize the order of blocks
	  total_blocks = _.shuffle(total_blocks); 

    // add block id (after shuffling) and add to exp.stims_block
    for (var b=0; b<num_blocks; b++) {
      var block = total_blocks[b];
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
    "practice_acceptability_bad_2", "post_practice_acceptability_bad_2", 
    "last_reminder", "block1", 'questionaire', 'finished'];
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
