library(torch)
library(tidyverse)
torch::cuda_is_available()

# create dataset ----
x<- sim.mar(len=5000, n_series=5)

x_train <- x$time_series[1:4000,] %>%as.matrix()
x_valid <- x$time_series[4001:5000,] %>%  as.matrix()



#create torch specific data loader ---- 
torch_dataset <- torch::dataset(
  name = "torch_dataset",
  
  initialize = function(x, n_timesteps, sample_frac = 1) {
    
    self$n_timesteps <- n_timesteps
    self$x <- torch_tensor(x)
    
    n <- nrow(self$x) - self$n_timesteps
    
    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))
    
  },
  
  .getitem = function(i) {
    
    start <- self$starts[i]
    end <- start + self$n_timesteps - 1
    
    list(
      x = self$x[start:end,],
      y = self$x[end + 1,]
    )
    
  },
  
  .length = function() {
    length(self$starts) 
  }
)

n_timesteps<- 2
train_ds  <-  torch_dataset(x_train,n_timesteps=n_timesteps)
length(train_ds)
train_ds[1]

batch_size <- 32
train_dl <- train_ds %>% dataloader(batch_size = batch_size, shuffle = TRUE)
length(train_dl)

b <- train_dl %>% dataloader_make_iter() %>% dataloader_next()
b

valid_ds <- torch_dataset(x_valid, n_timesteps, sample_frac = 1)
valid_dl <- valid_ds %>% dataloader(batch_size = batch_size)




#torch MLP ---- 
model <- nn_sequential(
  
  # Layer 1
  nn_linear(10, 20),
  nn_relu(), 
  
  # Layer 2
  nn_linear(20, 32),
  nn_relu(),
  
  # Layer 3
  nn_linear(32,5)
)

net<- model

# torch RNN  ----
model <- nn_module(
  
  initialize = function(type, input_size, hidden_size, num_layers = 1, dropout = 0) {
    
    self$type <- type
    self$num_layers <- num_layers
    
    self$rnn <- if (self$type == "gru") {
      nn_gru(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    } else {
      nn_lstm(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    }
    
    self$output <- nn_linear(hidden_size, 1)
    
  },
  
  forward = function(x) {
    
    # list of [output, hidden]
    # we use the output, which is of size (batch_size, n_timesteps, hidden_size)
    x <- self$rnn(x)[[1]]
    
    # from the output, we only want the final timestep
    # shape now is (batch_size, hidden_size)
    x <- x[ , dim(x)[2], ]
    
    # feed this to a single output neuron
    # final shape then is (batch_size, 1)
    x %>% self$output() 
  }
  
)
net <- model(type ="gru", input_size = 5*n_timesteps, hidden_size = 120)

# prepare training ----

#send net to device training is done on 
device<- torch_device(if (cuda_is_available()) "cuda" else "cpu")
net <- net$to(device = torch_device(if (cuda_is_available()) "cuda" else "cpu"))


#initialize optimzer 
optimizer <- optim_adam(net$parameters, lr = 0.001)




train_batch <- function(b) {
  
  optimizer$zero_grad()
  b$x<- torch_reshape(b$x,shape=c(dim(b$x)[1],prod(dim(b$x)[2:3])))
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$backward()
  optimizer$step()
  
  loss$item()
}

valid_batch <- function(b) {
  
  b$x<- torch_reshape(b$x,shape=c(dim(b$x)[1],prod(dim(b$x)[2:3])))
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$item()
  
}

#training loop 
num_epochs <- 10
for (epoch in 1:num_epochs) {
  
  #set model to train (contrary to $eval() where no parameters can be changed)
  net$train()
  train_loss <- c()
  
  coro::loop(for (b in train_dl) {
    
    if (dim(b$x)[1]== batch_size){
      loss <-train_batch(b)
      train_loss <- c(train_loss, loss)
    }
  })
  
  cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, mean(train_loss)))
  
  net$eval()
  valid_loss <- c()
  
  coro::loop(for (b in valid_dl) {
    if (dim(b$x)[1]== batch_size){
      loss <- valid_batch(b)
      valid_loss <- c(valid_loss, loss)
    }
})
  
  cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, mean(valid_loss)))
}

saved_params<- as.numeric(net$parameters$`0.weight`$cpu())

#training loop for first weight layer and L1 Generalization (Neural Granger) ---- 

#set input layers which should be optimized

#freeze layers except input ones 
input_weights<- which(startsWith(names(net$parameters),"0."))
if (length(input_weights)<1) input_weights<-c(1)

for (i in model$parameters[-1]){
  i$requires_grad <- F
}


optimizer <- optim_adam(net$parameters, lr = 0.001)

train_generalized <- function(b) {
  
  optimizer$zero_grad()
  b$x<- torch_reshape(b$x,shape=c(dim(b$x)[1],prod(dim(b$x)[2:3])))
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  #regularization loss 
  regularize <- 0.1*(torch_sum(torch_norm(torch_cat(net$parameters[1]),p=1L,dim=1L)))
  loss <- nnf_mse_loss(output, target)
  
  loss<- torch_add(loss,regularize)
  loss$backward()
  optimizer$step()
  
  loss$item()
}


for (epoch in 1:10) {
  
  net$train()
  train_loss <- c()
  
  coro::loop(for (b in train_dl) {
    loss <-train_generalized(b)
    train_loss <- c(train_loss, loss)
    
  })
  cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, mean(train_loss)))
  
  net$eval()
  valid_loss <- c()

  coro::loop(for (b in valid_dl) {
    loss <- valid_batch(b)
    valid_loss <- c(valid_loss, loss)
  })

  cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, mean(valid_loss)))
}

saved_params[1:10]
as.numeric(net$parameters$`0.weight`$cpu())
net$parameters$`0.weight`


