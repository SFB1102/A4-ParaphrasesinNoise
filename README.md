# Paraphrases in Noise:
### This repository contains the perception data of 300 paraphrase triplets in noise:

## Perception experiment data
### PiN :
900 paraphrase pairs in 3 different listening conditions.
### PiN <sub> both </sub>
This is a subset of PiN dataset with 332 paraphrase pairs which were identified as 'exact paraphrases' by both of the 2 annotators.
### PiN <sub> either </sub>
This is a subset of PiN dataset with 596 paraphrase pairs which were identified as 'exact paraphrases' by either of the 2 annotators.

## Audio stimuli:
PiN_audio_stimuli.zip folder consists of all 900 utterances synthesized using Google API.


## Linear Regression Models:
To model the intelligibility-gain among paraphrases using lingustic and acoustic features, refer to scripts folder.
We have also included our model estimations for all noise conditions in scripts/PiN_linear_regression.R file.


Please use the following citation to refer to this work/dataset:


```
@inproceedings{chingacham21_interspeech,
  author={Anupama Chingacham and Vera Demberg and Dietrich Klakow},
  title={{Exploring the Potential of Lexical Paraphrases for Mitigating Noise-Induced Comprehension Errors}},
  year=2021,
  booktitle={Proc. Interspeech 2021},
  pages={1713--1717},
  doi={10.21437/Interspeech.2021-306}
}
```


Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
