# %% [markdown]
# # Using the Switching Regression to be Robust to Misclassification for Ethiopian Maize Adoption

# %%

# %%
import pandas as pd
import numpy as np
from itertools import permutations
from linearmodels import PanelOLS
import sys
sys.path.append("../uganda-uber-switching-reg/uganda_uber_switching_reg")
from mle import DriverSpecificProbUberMLE


# %%
df = pd.read_stata("data/full_panel.zip")

trajectories = (
    df
    .dropna(subset= ['impmaize'])
    .groupby(['holder_id'])['impmaize']
    .agg(trajectories = list)
    .assign(len_traj = lambda df: df['trajectories'].apply(lambda x: len(x)))
    .query("len_traj == 3")
    .drop(['len_traj'], axis=1)
    .assign(trajectories = lambda df: df['trajectories'].astype(str))
    .pipe(pd.get_dummies)
    .rename(lambda x: x.replace('.0', '').replace(',', '').replace('[', '').replace(']', '').replace(' ', ''), axis=1)
    )

# merge with df

merged_df = (
    df
    .merge(trajectories, 
           left_on= ['holder_id'], 
           right_index=True)

    )




# %%
# create misclassification matrices for each trajectory

purity_95 = pd.DataFrame(data=[[.863, .1370],
                              [.2958, .7042]], index=[0, 1], 
                                columns=[0,1])

dna_dtm = pd.DataFrame(data=[[.9709, .0291],
                              [.8678, .1322]], index=[0,1], 
                                columns=[0,1])

dna_2010 = pd.DataFrame(data=[[.859, .141],
                              [.6281, .3719]], index=[0,1], 
                                columns=[0,1])

# %%
trajectory_nums = trajectories.columns.str.replace("trajectories_", '').tolist()

# %%
# Now the magic...
# Need to take these static probabilities and turn the into a probability for each trajectory
# So my confusion matrix for each will be 2^3 x 2^3

trajectory_nums = trajectories.columns.str.replace("trajectories_", '').tolist()

def confusion_matrix(misclass_mat):
    confusion_matrix = pd.DataFrame(index=trajectory_nums, columns=trajectory_nums)
    
    for traj_self_reported in trajectory_nums:
        for traj_misclassified in trajectory_nums:
            traj_int_self_reported = [int(i) for i in list(traj_self_reported)]
            traj_int_misclassified = [int(i) for i in list(traj_misclassified)]
            confusion_matrix.loc[traj_self_reported, traj_misclassified] = \
                misclass_mat.loc[traj_int_self_reported[0], traj_int_misclassified[0]] * \
                    misclass_mat.loc[traj_int_self_reported[1], traj_int_misclassified[1]] * \
                        misclass_mat.loc[traj_int_self_reported[2], traj_int_misclassified[2]]
    
    return confusion_matrix
    

# %%
confusion_matrix(purity_95)

# %%
merged_df.columns.tolist()

# %%
# Let's see what happens!

trajectory_df = (
    merged_df
    .set_index(['holder_id', 'wave'])
    .filter(like='trajectories_')
    .idxmax(axis=1)
    .reset_index()
    .rename({0 : 'trajectories'}, axis=1)
    .assign(trajectories = lambda df: df['trajectories'].str.replace('trajectories_', ''))
    .merge(merged_df, on = ['holder_id', 'wave'])
    )

# %%
full_df = (
    trajectory_df
    .merge(trajectory_df[['wave', 'trajectories', 'impmaize']]
           .drop_duplicates(), on=['wave'], 
           suffixes=('_true', '_misclass'))
    .set_index(['holder_id', 'wave', 'trajectories_misclass'])
    .query("YIELD_selfr_tr !=0")
    .assign(log_yield = lambda df: np.log(df['YIELD_selfr_tr']))
    .dropna(subset=['log_yield', 'impmaize_misclass', 'yrseduc', "age_head", "sex_head",  'title', "parcesizeHA", "hhlabor", "hiredlabor"])
    )

# %%
classifier_pred = (
    full_df
    .filter(like='trajectories_')
    .reset_index()
    .drop(columns=['trajectories_misclass', 'trajectories_true'])
    .drop_duplicates()
    .set_index(['holder_id', 'wave'])
    )

# %%
# from numpy import full


# PanelOLS.from_formula("log_yield ~ -1 + " + ' + '.join(no_stayers) + ' + ' \
#         + ' + '.join([f"{s}:impmaize_misclass" for s in switchers]) , data=full_df.reset_index('trajectories_misclass').query("trajectories_true==trajectories_misclass")).fit()

# %%
no_stayers = classifier_pred.drop(columns=['trajectories_111']).columns.tolist()
switchers = classifier_pred.drop(columns=['trajectories_000','trajectories_111']).columns.tolist()

mod = DriverSpecificProbUberMLE.from_formula("log_yield ~ -1 + " + ' + '.join(no_stayers) + ' + ' \
        + ' + '.join([f"{s}:impmaize_misclass" for s in switchers]) , 
                                       data=full_df, 
                                       classifier_pred = classifier_pred,
                                       check_absorbed=False,
                                       cm = confusion_matrix(dna_dtm).values.T)

sr, pols = mod.fit(method='bfgs', cov_type='cluster', 
        cov_kwds = {'groups':full_df.reset_index('trajectories_misclass').query("trajectories_true==trajectories_misclass").trajectories_true})

# %%
pols.summary

# %%
sr.summary()


