import os
import torch
import datasets
import evaluate

os.environ["CUDA_VISIBLE_DEVICES"]="0"

from transformers import (
    AutoTokenizer,
    AutoConfig,
    AutoModel,
    AutoModelForCausalLM,
    GPT2LMHeadModel,
    GPTJForCausalLM,
    GPTNeoForCausalLM,
    PreTrainedTokenizerFast,
    DataCollatorForLanguageModeling,
    Trainer,
    TrainingArguments,
    EarlyStoppingCallback
)

context_length = 1024

# load custom tokenizer
tokenizer_dir = 'tokenizer'
tokenizer_pt = "candor_tokenizer.json"
tokenizer_path = os.path.join(tokenizer_dir,tokenizer_pt)

tokenizer = PreTrainedTokenizerFast(
	tokenizer_file=tokenizer_path,
	model_max_length = context_length,
        unk_token = '<unk>',
        pad_token = '<pad>',
        mask_token = '<mask>',
        eos_token = '<eos>')

# load dataset
# ROOT directory with train and valid files
ROOT = ''
# path to training file
train_file = 'data/CandorTrain_forInfill.txt' 
# path to test file
valid_file = 'data/SWBDTest_forInfill.txt'
train_filepath = os.path.join(ROOT,train_file)
valid_filepath = os.path.join(ROOT,valid_file)


# initiate dataset object
raw_dataset = datasets.load_dataset('text', data_files={'train': train_file,'valid':valid_file})


# helper function for tokenization
def tokenize(element):
    outputs = tokenizer(
        element["text"],
        # truncate to max length
        truncation=True,
        # hard limit on length of sequences
        max_length=context_length,
        # pad to max length
        #padding='longest',
        return_overflowing_tokens=True,
    )
    return {"input_ids": outputs['input_ids']}


# init collator for lm
data_collator = DataCollatorForLanguageModeling(
    tokenizer=tokenizer, mlm=False)


# Tokenize datasets
tokenized_datasets = raw_dataset.map(tokenize, batched=True, remove_columns=raw_dataset["train"].column_names)

# define gpt2 config
config = AutoConfig.from_pretrained(
        "gpt2",
        vocab_size=len(tokenizer),
        n_ctx=context_length,
        bos_token_id = tokenizer.eos_token_id,
        eos_token_id = tokenizer.eos_token_id
)

training_args = TrainingArguments(
#run inference
    output_dir="models/gpt2-for-infill",
    overwrite_output_dir=True,
    num_train_epochs=10,
    evaluation_strategy = "epoch",
    logging_strategy = 'steps',
    weight_decay = 0.01,
    logging_steps = 100,
    save_strategy = "epoch",
    save_total_limit = 2,
    metric_for_best_model="eval_loss",
    load_best_model_at_end=True,
    per_device_train_batch_size=4,
    per_device_eval_batch_size=4,
)


# initialize model architecture without pretrained weights
model = GPT2LMHeadModel(config)


trainer = Trainer(
    model=model,
    args=training_args,
    data_collator=data_collator,
    train_dataset=tokenized_datasets['train'],
    eval_dataset=tokenized_datasets['valid'],
    callbacks = [EarlyStoppingCallback(early_stopping_patience=2)]
)


trainer.train()
