"""
Combine individual preprocessed conversation files (preprocessed using SWBD_parse_xml.py, stored under 'proc' directory) and combines into 
a single file.
"""

import re
import os
import numpy as np
import pandas as pd

ROOT = os.path.join("proc")

uttrCtr = 1
speaker_IDs = []
uttr_counts = []
uttr_words = []
uttr_STs = []
uttr_ETs = []
utterances = []
uttr_durations = []
uttr_fluencies = []
global_word_IDs = []
POS_tags = []
uWID = 0
for file in os.listdir(ROOT):
    if file.startswith("disf"):
        filepath = os.path.join(ROOT,file)
        swbd_df = pd.read_csv(filepath)
        uttrID = np.unique(swbd_df["uttrID"].values)
        for ID in uttrID:
            swbd_uttr_ID = swbd_df[swbd_df["uttrID"] == ID]
            POS = swbd_uttr_ID["POS"].values
            words = swbd_uttr_ID["word"].values
            durations = swbd_uttr_ID["duration"].values
            STs = swbd_uttr_ID["start_time"].values
            ETs = swbd_uttr_ID["end_time"].values
            fluency_status = swbd_uttr_ID["fluency_status"].values
            sp_IDs = swbd_uttr_ID["sp_ID"].values
            # construct utterance
            uttr = " ".join(words)
            #doc = nlp(uttr)
            if len(uttr) > 0:
                utterances.append(uttr)
            N = len(words)
            for i in range(N):
                uttr_words.append(words[i])
                uttr_counts.append(uttrCtr)
                global_word_IDs.append(uWID)
                uttr_durations.append(durations[i])
                POS_tags.append(POS[i])
                uttr_fluencies.append(fluency_status[i])
                speaker_IDs.append(sp_IDs[i])
                uWID += 1
            uttrCtr += 1


#uttr = {"utterances":utterances}
#df = pd.DataFrame(data=uttr)

# to datafarme
combined_data = {"speakerID":speaker_IDs,"uttrID":uttr_counts,"uWordID":global_word_IDs,"word":uttr_words,"POS":POS_tags,"duration":uttr_durations,"fluency":uttr_fluencies}
combined_df = pd.DataFrame(data=combined_data)
# to csv
combined_df.to_csv("SWBD_data_wordDurations.csv",index=False)
