from collections import defaultdict, Counter
import pandas as pd


def load_cldf_dataset(path_to_values, path_to_languages):
    values = pd.read_csv(path_to_values)
    languages = pd.read_csv(path_to_languages)
    return pd.merge(left=values, right=languages, how="left",
                    left_on="Language_ID", right_on="ID")


def get_frequencies_w_inventory_collapsing(dataset):
    glottocode_to_inventory = defaultdict(set)
    for row in dataset.itertuples():
        if not pd.isnull(row.Language_ID):
            glottocode_to_inventory[row.Language_ID].add(row.Value)
    print(f'{len(glottocode_to_inventory)} languages')
    frequencies_absolute = Counter()
    for segments in glottocode_to_inventory.values():
        for segment in segments:
            frequencies_absolute[segment] += 1
    frequencies_relative = {
        segment: count / len(glottocode_to_inventory)
        for segment, count in frequencies_absolute.items()
    }
    return frequencies_absolute, frequencies_relative


def borrowability_score(q_s, f_s, divide_by_six=False):
    if divide_by_six:
        return q_s / f_s / (1 - f_s) / 6
    else:
        return q_s / f_s / (1 - f_s)


segbo = load_cldf_dataset('../data/segbo/cldf/values.csv',
                          '../data/segbo/cldf/languages.csv')
phoible = load_cldf_dataset('../data/phoible/cldf/values.csv',
                            '../data/phoible/cldf/languages.csv')
n_phoible_inventories = len(phoible.Language_ID.unique())
print(f'{n_phoible_inventories=}')
print(f'{len(segbo.Language_ID.unique())=}')
print(f'{len(set(segbo.Language_ID) - set(phoible.Language_ID))=}')
phoible_langs = set(phoible.Language_ID)
segbo = segbo.loc[segbo.Language_ID.map(lambda gltc: gltc in phoible_langs)]
print(f'{len(segbo.Language_ID.unique())=}')
(
    phoible_frequencies_absolute,
    _
) = get_frequencies_w_inventory_collapsing(phoible)
(
    segbo_frequencies_absolute,
    _
) = get_frequencies_w_inventory_collapsing(segbo)
segbo_frequencies_relative = {
    segment: count_segbo / n_phoible_inventories
    for segment, count_segbo in segbo_frequencies_absolute.items()
}
phoible_greater_or_equal = {}
phoible_strictly_greater = {}
for segment, count_segbo in segbo_frequencies_absolute.items():
    if count_segbo >= phoible_frequencies_absolute[segment]:
        print(segment, count_segbo, phoible_frequencies_absolute[segment])
        phoible_greater_or_equal[segment] = count_segbo
        phoible_strictly_greater[segment] = count_segbo + 1
    else:
        phoible_greater_or_equal[segment] = phoible_frequencies_absolute[
            segment]
        phoible_strictly_greater[segment] = phoible_frequencies_absolute[
            segment] + 1
phoible_freqs_relative = {
    segment: count / n_phoible_inventories
    for segment, count in phoible_greater_or_equal.items()
}
phoible_freqs_relative_laplace = {
    segment: count / n_phoible_inventories
    for segment, count in phoible_strictly_greater.items()
}
for segment, f_s in sorted(phoible_freqs_relative.items(),
                           key=lambda el: el[1], reverse=True)[:10]:
    print(f'{segment}: {f_s}, {phoible_freqs_relative_laplace[segment]}')

borrowability_scores = {}
borrowability_scores_laplace = {}
for segment in segbo_frequencies_relative:
    borrowability_scores[segment] = {
        'Segment': segment,
        'Borrowability': borrowability_score(
            segbo_frequencies_relative[segment],
            phoible_freqs_relative[segment]
        ),
        'PHOIBLE_frequency_absolute': phoible_greater_or_equal[segment],
        'PHOIBLE_frequency_relative': phoible_freqs_relative[segment],
        'SEGBO_frequency_absolute': segbo_frequencies_absolute[segment],
        'SEGBO_frequency_relative': segbo_frequencies_relative[segment]
    }
    borrowability_scores_laplace[segment] = {
        'Segment': segment,
        'Borrowability': borrowability_score(
            segbo_frequencies_relative[segment],
            phoible_freqs_relative_laplace[segment]
        ),
        'PHOIBLE_frequency_absolute': phoible_strictly_greater[segment],
        'PHOIBLE_frequency_relative': phoible_freqs_relative_laplace[segment],
        'SEGBO_frequency_absolute': segbo_frequencies_absolute[segment],
        'SEGBO_frequency_relative': segbo_frequencies_relative[segment]
    }

borrowability_df = pd.DataFrame.from_dict(
    borrowability_scores
).T.sort_values(by='Borrowability', ascending=False)
print(borrowability_df.loc[
    borrowability_df.SEGBO_frequency_absolute >= 10])
print(borrowability_df.loc[
    borrowability_df.SEGBO_frequency_absolute <= 2][:10])
# Merge with PHOIBLE features for analysis
phoible_features = pd.read_csv('../data/phoible/cldf/parameters.csv')
borrowability_df_w_features = pd.merge(borrowability_df, phoible_features,
                                       left_on='Segment', right_on='Name')
borrowability_df_w_features.to_csv('model_1_borrowability_w_features.csv',
                                   index=False)
