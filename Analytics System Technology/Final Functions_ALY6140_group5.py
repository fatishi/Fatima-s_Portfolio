"""
< Capstone: Project Draft >


Copyright (c) 2022 
Licensed
Written by <Fatima Nurmakhamadova, and Min-Chi Tsai> 

"""

#Import packages
import pandas as pd
from sklearn.metrics import roc_curve, auc

def read_file(filename):
    """
    Import and read in a csv, txt,
    or xlsx file.
    
    This function imports the file with csv, 
    txt, or xlsx format, reads in, then 
    assigns to a data structure and returns it.
    
    Parameters
    ----------
    filename
        The file name to be imported.
    
    Returns
    -------
    DataFrame
        If the format is csv, txt, or xlsx,
        it returns the head and tail of the dataframe.
    Message
        If the file has another format, it will return
        a message to import file with a right type.     
    """
    if filename.endswith(".csv"):
        df = pd.read_csv(filename, sep=',', header=0) 
        return df
    elif filename.endswith(".txt"):
        df = pd.read_table(filename, delimiter = "|")
        return df
    elif filename.endswith(".xlsx"):
        df = pd.read_excel(filename)
        return df
    else:
        print("Put the file with csv, txt, or excel format.")
        

def unique_features(df):
    """
    Takes a dataframe, and shows the unique 
    features of each column. 
    
    Parameters
    ----------
    df
        The dataset to be analyzed.
        
    Returns
    -------
    Unique Values
        Analyzes a dataframe and returns all 
        unique values that each column has.
    """
    for col in df:
        print ('\nUnique Features in Column %s'%col)
        print (df[col].unique())        
        

def filter_dataset(df, col, values):
    """
    Takes a dataframe, column name,
    and values, then shows the dataset 
    without those values. 
    
    This function helps to filter and 
    show the dataset removing indicated
    specific values in specific column. 
    
    Parameters
    ----------
    df
        The dataset to be filtered.
    col
        The column to be filtered.
    values : int or str
        The value or values to be filtered. 
        
    Returns
    -------
    DataFrame
        Filtered dataframe without specified values.
    """
    return df[~df[col].isin(values)]

    
    # Create a function to plot roc curve


def roc_curve_plot(X_test,y_test,y_pred_proba,model):
    """
    This function will make a Receiver Operating Characteristics (ROC) Curve linechart based on metrics and matplotlib visulization.
    Arguments
    ----------
    y_test:             List of testing targets
    y_pred_proba :      List of predicted probability
        
    Returns:            ROC Curve linechart
    -------
    
    """

    # Calculate fpr, tpr, and thresholds for test set
    fpr, tpr, thresholds = roc_curve(y_test, y_pred_proba)
    auc = metrics.roc_auc_score(y_test, y_pred_proba)

    # Plot the training FPR and TPR
    plt.plot(fpr, tpr, label = "ROC")
    plt.text(0.65, 0.3, "AUC="+str(round(auc,2)), ha='center',fontsize=14,weight='bold')
  
    # Plot positive sloped 1:1 line for reference
    plt.plot([0,1],[0,1])
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.ylabel('True Positive Rate')
    plt.xlabel('False Positive Rate')
    plt.legend(loc=4)
    
    
    
def make_confusion_matrix(cf,
                          group_names=None,
                          categories='auto',
                          count=True,
                          percent=True,
                          cbar=True,
                          xyticks=True,
                          xyplotlabels=True,
                          sum_stats=True,
                          figsize=None,
                          cmap='Blues',
                          title=None):
    '''
    This function will make a pretty plot of an sklearn Confusion Matrix cm using a Seaborn heatmap visualization.
    Arguments
    ---------
    cf:            confusion matrix to be passed in
    group_names:   List of strings that represent the labels row by row to be shown in each square.
    categories:    List of strings containing the categories to be displayed on the x,y axis. Default is 'auto'
    count:         If True, show the raw number in the confusion matrix. Default is True.
    normalize:     If True, show the proportions for each category. Default is True.
    cbar:          If True, show the color bar. The cbar values are based off the values in the confusion matrix.
                   Default is True.
    xyticks:       If True, show x and y ticks. Default is True.
    xyplotlabels:  If True, show 'True Label' and 'Predicted Label' on the figure. Default is True.
    sum_stats:     If True, display summary statistics below the figure. Default is True.
    figsize:       Tuple representing the figure size. Default will be the matplotlib rcParams value.
    cmap:          Colormap of the values displayed from matplotlib.pyplot.cm. Default is 'Blues'
                   See http://matplotlib.org/examples/color/colormaps_reference.html
                   
    title:         Title for the heatmap. Default is None.
    Returns:       Confusion Matrix Heatmap
    -------
    
    '''


    # CODE TO GENERATE TEXT INSIDE EACH SQUARE
    blanks = ['' for i in range(cf.size)]

    if group_names and len(group_names)==cf.size:
        group_labels = ["{}\n".format(value) for value in group_names]
    else:
        group_labels = blanks

    if count:
        group_counts = ["{0:0.0f}\n".format(value) for value in cf.flatten()]
    else:
        group_counts = blanks

    if percent:
        group_percentages = ["{0:.2%}".format(value) for value in cf.flatten()/np.sum(cf)]
    else:
        group_percentages = blanks

    box_labels = [f"{v1}{v2}{v3}".strip() for v1, v2, v3 in zip(group_labels,group_counts,group_percentages)]
    box_labels = np.asarray(box_labels).reshape(cf.shape[0],cf.shape[1])


    # CODE TO GENERATE SUMMARY STATISTICS & TEXT FOR SUMMARY STATS
    if sum_stats:
        #Accuracy is sum of diagonal divided by total observations
        accuracy  = np.trace(cf) / float(np.sum(cf))

        #if it is a binary confusion matrix, show some more stats
        if len(cf)==2:
            #Metrics for Binary Confusion Matrices
            precision = cf[1,1] / sum(cf[:,1])
            recall    = cf[1,1] / sum(cf[1,:])
            f1_score  = 2*precision*recall / (precision + recall)
            stats_text = "\n\nAccuracy={:0.3f}\nPrecision={:0.3f}\nRecall={:0.3f}\nF1 Score={:0.3f}".format(
                accuracy,precision,recall,f1_score)
        else:
            stats_text = "\n\nAccuracy={:0.3f}".format(accuracy)
    else:
        stats_text = ""


    # SET FIGURE PARAMETERS ACCORDING TO OTHER ARGUMENTS
    if figsize==None:
        #Get default figure size if not set
        figsize = plt.rcParams.get('figure.figsize')

    if xyticks==False:
        #Do not show categories if xyticks is False
        categories=False


    # MAKE THE HEATMAP VISUALIZATION
    plt.figure(figsize=figsize)
    sns.heatmap(cf,annot=box_labels,fmt="",cmap=cmap,cbar=cbar,xticklabels=categories,yticklabels=categories)

    if xyplotlabels:
        plt.ylabel('True label')
        plt.xlabel('Predicted label' + stats_text)
    else:
        plt.xlabel(stats_text)
    
    if title:
        plt.title(title)
