require("keras")
require("ggplot2")
# require("tidyverse")
# require("h2o")
require("reticulate")
# specify the directory of your conda environment, you need to have python installed in order to implement keras package
# use_python("/Users/alexyoung/anaconda3/bin/python3", required=TRUE)
use_python("/Users/alexyoung/anaconda3/bin/python3", required=TRUE)

mnist <- dataset_mnist()
train_images <- mnist$train$x
train_images <- array_reshape(train_images, c(nrow(train_images), 28*28))
train_images <- train_images / 255

input_img <- layer_input(shape = c(28*28)) 

# Encoder
encoded <- input_img %>% 
  layer_dense(units = 1024, activation = "relu") %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 4, activation = "relu") %>% 
  layer_dense(units = 2, activation = "sigmoid")

# Decoder
decoded <- encoded %>% 
  layer_dense(units = 4, activation = "relu") %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1024, activation = "relu") %>% 
  layer_dense(units = 28*28, activation = "sigmoid")

# combines the encoder and decoder into the Autoencoder model
autoencoder <- keras_model(input = input_img, output = decoded)
pdf("mnist_ae.pdf");
plot(autoencoder, show_shapes = T, show_layer_activations = T);
dev.off()


autoencoder %>% compile(
  optimizer = 'adam', 
  loss = 'mean_squared_error'
)

summary(autoencoder)

history <- autoencoder %>% fit(
  train_images, train_images,
  epochs = 200,
  batch_size = 256,
  shuffle = TRUE,
  validation_split = 0.2,
  getOption("keras.fit_verbose", default = 0)
)
plot(history)

encoder <- keras_model(inputs = input_img, outputs = encoded)
encoded_images <- predict(encoder, train_images)

# Convert to a data frame for easier viewing
encoded_df <- as.data.frame(encoded_images)
head(encoded_df)





# Plotting the bottleneck
subsample <- sample(1:length(mnist$train$y),size = 2e4)
labels_train <- mnist$train$y[subsample]
ae <- ggplot(encoded_df[subsample,], aes(x = V1, y = V2, 
                             color = as.factor(labels_train), 
                             label=labels_train)) +
  geom_point(alpha = 0.4, size = 0.5,) +
  geom_text() + 
  labs(title = "Autoencoder projection",
       x = "Latent Space 1",
       y = "Latent Space 2",
       color = "Digit") +
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme_minimal()
ae

