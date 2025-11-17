"""
Generate preprocessed word-aligned conversations by combining speaker turns from terminal and disfluency files. 
Stores each processed file under 'proc' directory. 
Requires accesses to Switchboard NXT annotations: https://groups.inf.ed.ac.uk/switchboard/
"""

import re
import os
import numpy as np
import pandas as pd
import xml.etree.ElementTree as ET

ROOT = "xml"
term_DIR = "terminals"
disf_DIR = "disfluency"

def extract_terminals(term_filepath):
    # Get terminals
    term_tree = ET.parse(term_filepath)
    term_root = term_tree.getroot()
    terminals = term_root.findall('word')
    # get ID, word, POS, and time
    wIDs = []
    words = []
    POS_tags = []
    start_times = []
    end_times = []
    for term in terminals:
        term_attributes = list(term.attrib.values())
        if "non-aligned" not in term_attributes:
            POS = term_attributes[0]
            ID = term_attributes[1]
            end_time = term_attributes[-3]
            start_time = term_attributes[-2]
            word = term_attributes[-1].lower()
            wIDs.append(ID)
            words.append(word)
            POS_tags.append(POS)
            start_times.append(start_time)
            end_times.append(end_time)
    # construct dataframe
    terminal_data = {'wID':wIDs,'word':words,'POS':POS_tags,'start_time':start_times,'end_time':end_times}
    df = pd.DataFrame.from_dict(terminal_data)
    # compute duration
    df["start_time"].replace('n/a', '0.0',inplace=True)
    df["end_time"].replace('n/a', '0.0',inplace=True)
    return df

def adjust_split_words(df):
    # fix split words
    st_idx = np.where(df['start_time'].values == '0.0')[0]
    idx_word = df.loc[st_idx]['word']
    idx_endtime = df.loc[st_idx]['end_time']
    # combine split words
    for i in st_idx:
        df.loc[i-1]['word'] = df.iloc[i-1]['word'] + " " + idx_word[i]
        df.loc[i-1]['end_time'] = idx_endtime[i]
    # delete contraction-only row
    df.drop(st_idx,inplace=True)
    return df

def extract_disfluencies(disf_filepath):
    disf_tree = ET.parse(disf_filepath)
    disf_root = disf_tree.getroot()
    disfluencies = disf_root.findall('disfluency')
    pattern = r'\(.*\)$'
    reparandum_ids = []
    repair_ids = []
    for disfluency in disfluencies:
        # get reparandums and repairs
        reparandums = list(disfluency)[0]
        repairs = list(disfluency)[1]
        for reparandum in reparandums:
            repr = re.findall(pattern,list(reparandum.attrib.values())[0])
            if len(repr):
                reparandum_ids.append((re.sub(r'[(|)]','',repr[0])))
        for repair in repairs:
            rep = re.findall(pattern,list(repair.attrib.values())[0])
            if len(rep):
                repair_ids.append(re.sub(r'[(|)]','',rep[0]))
    reparandum_ids = np.array(reparandum_ids)
    repair_ids = np.array(repair_ids)
    return reparandum_ids, repair_ids

def preprocess_switchboard_xml(swbd_ID,sp_ID):
    term_file = "sw%s.%s.terminals.xml" % (swbd_ID,sp_ID)
    disf_file = "sw%s.%s.disfluency.xml" % (swbd_ID,sp_ID)
    # get disfluencies
    disf_filepath = os.path.join(ROOT,disf_DIR,disf_file)
    reparandum_ids, repair_ids = extract_disfluencies(disf_filepath)
    # get terminals
    term_filepath = os.path.join(ROOT,term_DIR,term_file)
    terminals_df = extract_terminals(term_filepath)
    # mark disfluencies in terminals
    M, N = terminals_df.shape
    disfluency_markers = []
    for idx, row in terminals_df.iterrows():
        if row['wID'] in reparandum_ids:
            disfluency_markers.append('reparandum')
        elif row['wID'] in repair_ids:
            disfluency_markers.append('repair')
        else:
            disfluency_markers.append('fluent')
    terminals_df['fluency_status'] = disfluency_markers
     # fix contractions
    terminals_df = adjust_split_words(terminals_df)
    # remove word segments with missing end times
    terminals_df = terminals_df[terminals_df["end_time"] != '0.0']
    terminals_df["uttrID"] = terminals_df["wID"].apply(lambda x: x.split("_")[0][1:])
    return terminals_df

def combine_and_preprocess(df_A,df_B):
    df_combined = pd.concat([df_A,df_B],axis=0)
    df_combined["start_time"] = df_combined["start_time"].values.astype(np.float64)
    df_combined["end_time"] = df_combined["end_time"].values.astype(np.float64)
    df_combined = adjust_split_words(df_combined)
    df_combined.sort_values(by="start_time",inplace=True)
    # compute durations and onset latencies
    start_times = df_combined["start_time"].values
    end_times = df_combined["end_time"].values
    durations = end_times - start_times
    N = len(durations)
    onset_latencies = np.zeros(N)
    onset_latencies[0] = start_times[0]
    for i in range(N-1):
        prev_ET = end_times[i]
        next_ST = start_times[i+1]
        onset_latency = next_ST - prev_ET
        onset_latencies[i+1] = onset_latency
    df_combined["duration"] = durations
    df_combined["onset_latencies"] = onset_latencies
    df_preprocessed = df_combined[["uttrID","wID","word","POS","start_time","end_time","duration","fluency_status"]]
    return df_preprocessed

def main():
	ROOT = "xml"
	term_DIR = "terminals"
	disf_DIR = "disfluency"
	output_DIR = "proc"

	fileIDs = [file.split('.')[0][2:] for file in os.listdir(os.path.join(ROOT,term_DIR))]
	for file in fileIDs:   
    	# preprocess swbd conversation files
    	swbd_A = preprocess_switchboard_xml(file,'A')
    	swbd_B = preprocess_switchboard_xml(file,'B')
    	# combine
    	swbd_comb = combine_and_preprocess(swbd_A,swbd_B)
    	outfile = "preproc_sw%s.csv" % file
    	swbd_comb.to_csv(os.path.join(output_DIR,outfile),float_format='%.5f',index=False)

