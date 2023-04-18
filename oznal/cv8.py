from math import sqrt

tp = int(input('Insert True Positives: '))
fp = int(input('Insert False Positives: '))
tn = int(input('Insert True Negatives: '))
fn = int(input('Insert False Negatives: '))

sensitivity = tp / (tp + fn) #also called recall
specificity = tn / (tn + fp)
precision = tp / (tp + fp) # also called positive predictive value
negative_predictive_value = tn / (tn + fn)
accuracy = (tp + tn) / (tp + fp + tn + fn)
f1_score = (2 * precision * sensitivity) / (precision + sensitivity)

# Interval spolahlivosti
sensitivity_lower = sensitivity - sqrt(sensitivity * 1.96 * (1 - sensitivity) / (tp + fn))
sensitivity_higher = sensitivity + sqrt(sensitivity * 1.96 * (1 - sensitivity) / (tp + fn))
specificity_lower = specificity - sqrt(specificity * 1.96 * (1 - specificity) / (tn + fp))
specificity_higher = specificity + sqrt(specificity * 1.96 * (1 - specificity) / (tn + fp))

# Likelihood Ratio (LR)
lr_plus = sensitivity / (1 - specificity)
lr_minus = (1 - sensitivity) / specificity

# Diagnostic Odds Ratio (DOR)
dor = lr_plus / lr_minus

print(
    '\nPredicted positives: ', tp + fp,
    '\nPredicted negatives: ', tn + fn,
    '\nReal positives: ', tp + fn,
    '\nReal negatives: ', tn + fp,
    '\nSum: ', tp + fp + fn + tn,
    '\nSensitivity / recall: ', round(sensitivity, 3),
    '\nSpecificity:', round(specificity, 3),
    '\nPrecision (positive predictive value):', round(precision, 3),
    '\nNegative predictive value:', round(negative_predictive_value, 3),
    '\nAccuracy:', round(accuracy, 3),
    '\nF1 score:', round(f1_score, 3),
    '\nInterval for sensitivity: ', round(sensitivity_lower, 3), '<->', round(sensitivity_higher, 3),
    '\nInterval for specificity: ', round(specificity_lower, 3), '<->', round(specificity_higher, 3),
    '\nLR plus:', round(lr_plus, 3),
    '\nLR minus::', round(lr_minus, 3),
    '\nDOR:', round(dor, 3),
)