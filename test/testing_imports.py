# this is an old homework from a cv class, just figure it's something that's big and easy to mess up

'''
The following code for downloading, importing and displaying the Fashion MNIST dataset is adapted
from ChatGPT, accessed February 27, 2024
'''
import numpy as np
import requests
import gzip 
import os
import cv2
import matplotlib.pyplot as plt
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix

def download_fashion_mnist(url, file_name):
    # Check if file already exists
    if not os.path.exists(file_name):
        # Download the file
        r = requests.get(url)
        with open(file_name, 'wb') as f:
            f.write(r.content)

def load_fashion_mnist(image_file, label_file):
    # Read image file
    with gzip.open(image_file, 'rb') as f:
        images = np.frombuffer(f.read(), np.uint8, offset=16).reshape(-1, 28, 28)

    # Read label file
    with gzip.open(label_file, 'rb') as f:
        labels = np.frombuffer(f.read(), np.uint8, offset=8)

    return images, labels

# URLs for Fashion MNIST dataset
image_url = 'http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/train-images-idx3-ubyte.gz'
label_url = 'http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/train-labels-idx1-ubyte.gz'

# File names to save the downloaded files
image_file = 'train-images-idx3-ubyte.gz'
label_file = 'train-labels-idx1-ubyte.gz'

# Download Fashion MNIST dataset
download_fashion_mnist(image_url, image_file)
download_fashion_mnist(label_url, label_file)

# Load Fashion MNIST dataset
train_images, train_labels = load_fashion_mnist(image_file, label_file)

# URLs for Fashion MNIST test dataset
test_image_url = 'http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/t10k-images-idx3-ubyte.gz'
test_label_url = 'http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/t10k-labels-idx1-ubyte.gz'

# File names to save the downloaded files for test dataset
test_image_file = 't10k-images-idx3-ubyte.gz'
test_label_file = 't10k-labels-idx1-ubyte.gz'

# Download Fashion MNIST dataset
download_fashion_mnist(test_image_url, test_image_file)
download_fashion_mnist(test_label_url, test_label_file)

# Load Fashion MNIST dataset
test_images, test_labels = load_fashion_mnist(test_image_file, test_label_file)

# Class labels
class_names = ['T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 
               'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot']

with gzip.open("train-images-idx3-ubyte.gz", 'rb') as f, gzip.open("train-labels-idx1-ubyte.gz", 'rb') as label_f:
    magic_number = int.from_bytes(f.read(4), 'big')
    number_of_images_train = int.from_bytes(f.read(4), 'big')
    rows = int.from_bytes(f.read(4), 'big')
    cols = int.from_bytes(f.read(4), 'big')

    label_magic_number = int.from_bytes(label_f.read(4), 'big')
    number_of_labels = int.from_bytes(label_f.read(4), 'big')
    
    buf = f.read(rows * cols * number_of_images_train)
    data = np.frombuffer(buf, dtype=np.uint8)
    training_images = data.reshape(number_of_images_train, rows, cols)
    training_images_normalized = (training_images - training_images.mean(axis=1, keepdims=True)) / (training_images.std(axis=1, keepdims=True) + 1e-10)
    training_images_normalized = training_images_normalized.reshape(training_images_normalized.shape[0], -1)

    label_buf = label_f.read(number_of_labels)
    training_labels = np.frombuffer(label_buf, dtype=np.uint8)

with gzip.open("t10k-images-idx3-ubyte.gz", 'rb') as f, gzip.open("t10k-labels-idx1-ubyte.gz", 'rb') as label_f:
    magic_number = int.from_bytes(f.read(4), 'big')
    number_of_images_test = int.from_bytes(f.read(4), 'big')
    rows = int.from_bytes(f.read(4), 'big')
    cols = int.from_bytes(f.read(4), 'big')

    label_magic_number = int.from_bytes(label_f.read(4), 'big')
    number_of_labels = int.from_bytes(label_f.read(4), 'big')
    
    buf = f.read(rows * cols * number_of_images_test)
    data = np.frombuffer(buf, dtype=np.uint8)
    test_images = data.reshape(number_of_images_test, rows, cols)
    test_images_normalized = (test_images - test_images.mean(axis=1, keepdims=True)) / (test_images.std(axis=1, keepdims=True) + 1e-10)
    test_images_normalized = test_images_normalized.reshape(test_images_normalized.shape[0], -1)

    label_buf = label_f.read(number_of_labels)
    test_labels = np.frombuffer(label_buf, dtype=np.uint8)
    

random_selection_train = np.random.choice(number_of_images_train, 10, replace=False)

# for i in random_selection_train:
#     plt.figure(figsize=(2, 2))
#     plt.imshow(training_images[i], cmap="gray", interpolation="none")
#     plt.title(f'Training {class_names[training_labels[i]]}')
#     plt.show()

random_selection_test = np.random.choice(number_of_images_test, 10, replace=False)

# for i in random_selection_test:
#     plt.figure(figsize=(2, 2))
#     plt.imshow(test_images[i], cmap="gray", interpolation="none")
#     plt.title(f'Testing {class_names[test_labels[i]]}')
#     plt.show()

unique, counts = np.unique(training_labels, return_counts=True)
training_label_counts = {class_names[label]: count for label, count in zip(unique, counts)}

print(training_label_counts)

unique, counts = np.unique(test_labels, return_counts=True)
test_label_counts = {class_names[label]: count for label, count in zip(unique, counts)}

print(test_label_counts)

knn = KNeighborsClassifier(n_neighbors=5)

random_test = np.random.choice(10000, size=25, replace=False)

knn.fit(test_images_normalized[random_test], test_labels[random_test])

out = knn.predict(training_images_normalized[random_test])

right, wrong = 0, 0

for i in range(25):
    if out[i] == train_labels[random_test[i]]:
        # plt.figure(figsize=(1, 1))
        # plt.imshow(train_images[random_test[i]], cmap="gray", interpolation="none")
        # plt.title(f'Correct: {class_names[out[i]]}')
        # plt.show()
        
        right += 1
    else:
        # plt.figure(figsize=(1, 1))
        # plt.imshow(train_images[random_test[i]], cmap="gray", interpolation="none")
        # plt.title(f'Wrong: {class_names[out[i]]} // Should be: {class_names[train_labels[random_test[i]]]}')
        # plt.show()
    
        wrong += 1

print("accuracy " + str(right / (wrong + right)))

'''
Test code for error metrics. Leave this unchanged.
'''
test_pairs = [
    (1, 1),
    (1, 4),
    (0, 0),
    (0, 2),
    (3, 1),
    (4, 2),
    (4, 4),
    (2, 2),
    (2, 2),
    (3, 3),
    (4, 0),
    (4, 4),
    (1, 1),
    (0, 0),
    (0, 2),
    (1, 4),
    (1, 1),
    (0, 0),
    (0, 1),
    (2, 3),
]

random_train = np.random.choice(len(train_images), size=1000, replace=False)

validation = training_images_normalized[random_train]
validation_labels = training_labels[random_train]

train_indicies = np.setdiff1d(np.arange(len(training_images_normalized)), random_train)

training_images = training_images_normalized[train_indicies]
training_labels = training_labels[train_indicies]

knn = KNeighborsClassifier(n_neighbors=15, weights='uniform')

knn.fit(training_images, training_labels)

out = knn.predict(validation)

print("accuracy computed by scikit:", accuracy_score(validation_labels, out))

# the logic would be the same for the test pairs

def compute_accuracy(test_pairs, num_classes):

    test_pairs = np.array(test_pairs)

    correct_test = (test_pairs[:, 0] == test_pairs[:, 1])

    print("test accuracy:", correct_test.sum() / len(test_pairs))
    
    correct = np.sum((out == validation_labels).astype(int))

    return correct / len(validation_labels)

def compute_per_class_accuracy(test_pairs, num_classes):
    
    true_positives = np.zeros(10)
    total_instances = np.zeros(10)
    
    for i in range(len(validation_labels)):
        total_instances[validation_labels[i]] += 1
        
        if out[i] == validation_labels[i]:
            true_positives[validation_labels[i]] += 1
    
    per_class_accuracy = true_positives / total_instances
    
    per_class_accuracy = np.nan_to_num(per_class_accuracy)
    
    return per_class_accuracy

def compute_confusion_matrix(pairs, num_classes):

    cm = np.zeros((10, 10), dtype=int)
    
    # Fill the confusion matrix
    for actual, predicted in zip(training_labels, validation_labels):
        cm[actual][predicted] += 1

    return cm

num_classes = 5
accuracy = compute_accuracy(test_pairs, num_classes)
print(f'accuracy: {accuracy:.2f}')

per_class_accuracy = compute_per_class_accuracy(test_pairs, num_classes)
print()
print('Per class accuracy')
for i, acc in enumerate(per_class_accuracy):
    print(f'{i}: {acc:4.2f}')

cm = compute_confusion_matrix(test_pairs, num_classes)
print(f'\nConfusion matrix')
for i in range(num_classes):
    print(f'{i:2d}:', end='')
    for j in range(num_classes):
        print(f' {cm[i, j]:2d}', end='')
    print()


# For me, it always tended to do pants the best, closely followed by bags and sneakers and sandals. I'm surprised that sneakers and sandals are at the top
# giving how similar they are as an article. I would've though that pants would easily out do them on most runs, but this isn't the case. A natural answer
# to important parameteres is the number of neighbors, given that all sets were equal the higher the k the better the results. I got 100% accurary with 
# weights being set to distance for sandals and 100% for pants on uniform
