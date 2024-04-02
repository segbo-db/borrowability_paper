import pandas as pd
from scipy.stats import pearsonr

segbo_data = pd.read_csv('../data/segbo/cldf/values.csv')
segbo_data_borrowing_counts = segbo_data.Value.value_counts()
segbo_data_borrowing_counts = segbo_data_borrowing_counts[segbo_data_borrowing_counts >= 10]
segbo_borrowing_frequencies = segbo_data_borrowing_counts.to_dict()

model_1_borrowabilities = pd.read_csv('model_1_borrowability.csv')
model_1_borrowabilities = model_1_borrowabilities[model_1_borrowabilities['Borrowability'].notna()]
model_1_dict = {
    k: v
    for k, v in zip(model_1_borrowabilities['Segment'],
                    model_1_borrowabilities['Borrowability'])
}

model_2_borrowabilities = pd.read_csv('../neighbor-graph_model/borrowability_on_the_graph.csv')
model_2_borrowabilities = model_2_borrowabilities[model_2_borrowabilities['Borrowability'].notna()]
model_2_dict = {
    k: v for k, v in zip(model_2_borrowabilities['Phoneme'],
                         model_2_borrowabilities['Borrowability'])
}

common_segments = (
        set(model_1_dict.keys()) &
        set(model_2_dict.keys()) &
        set(segbo_borrowing_frequencies.keys())
)
keys = sorted(common_segments)

segbo_vector = [segbo_borrowing_frequencies[k] for k in keys]
model_1_vector = [model_1_dict[k] for k in keys]
model_2_vector = [model_2_dict[k] for k in keys]

print('SEGBO v Model 1:', pearsonr(segbo_vector, model_1_vector).correlation, 'p-value:', pearsonr(segbo_vector, model_1_vector).pvalue)
print('SEGBO v Model 2:', pearsonr(segbo_vector, model_2_vector).correlation, 'p-value:', pearsonr(segbo_vector, model_2_vector).pvalue)