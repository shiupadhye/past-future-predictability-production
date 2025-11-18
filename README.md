# past-future-predictability-production


Code and repository for the paper "Back to the Future: The Role of Past and Future Context Predictability in Incremental Language Production"

### Scripts

- language-modeling: Scripts for language modeling training and inference

- preprocessing: Scripts for data preprocessing

- Study1-probabilistic-reduction: Scripts for running duration analyses

- Study2-substitution-model: Scripts for modeling substitution choices in naturalistic productions 

### Trained Language Model
The infill-adapted GPT-2 small language model can be found at: https://huggingface.co/shiupadhye/GPT2-small-infill-adapted-CANDOR

### Data availability

- Word-aligned duration data for study 1 were extracted from the publicly available Switchboard NXT annotations: https://groups.inf.ed.ac.uk/switchboard/. Since the aligned duration data could allow reconstruction of the original corpus (which is under LDC licensing), they cannot be redistributed. Code for extracting durations from the XML files can be found under preprocessing/duration-extraction/.

- Substitution sentences for Study 2 were derived from Switchboard NXT annotations, but underwent substantial preprocessing, de-identification, and transformation (e.g., removal of disfluencies, normalization, and conversion into isolated sentence frames). These edited stimuli do not contain reconstructable Switchboard text. The data can be found at https://osf.io/s2v7t/overview?view_only=5fc92815101c4c3fab933cfd06446e63 under Google Drive/Study2-substitution-model. Please do not share without permission.



