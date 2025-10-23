# 🚀 NeuralForge

**An autonomous ML framework using meta-learning and genetic algorithms to evolve neural architectures in real-time with automatic feature engineering and zero-downtime deployments**

[![Haskell](https://img.shields.io/badge/Haskell-%235e5086.svg?style=flat&logo=haskell&logoColor=white)](https://www.haskell.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status: Active](https://img.shields.io/badge/Status-Active-success.svg)]()

---

## 📋 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Architecture](#architecture)
- [Quick Start](#quick-start)
- [Installation](#installation)
- [Usage](#usage)
  - [CLI Usage](#cli-usage)
  - [REST API Usage](#rest-api-usage)
- [Configuration](#configuration)
- [Examples](#examples)
- [Development](#development)
- [Contributing](#contributing)

---

## 🎯 Overview

NeuralForge is an autonomous machine learning framework written in Haskell that combines:

- **🧬 Genetic Algorithms**: Evolutionary architecture search for optimal neural network designs
- **🧠 Meta-Learning**: Transfer knowledge from past experiments to accelerate new ones
- **⚡ Auto Feature Engineering**: Automatic creation of polynomial features and interactions
- **🔄 Zero-Downtime Deployment**: Blue-green deployment with automatic rollback
- **🎨 Real-time Evolution**: Continuously improve model architecture during training

---

## ✨ Features

### Core Capabilities

- ✅ **Automatic Neural Architecture Search (NAS)** using genetic algorithms
- ✅ **Meta-learning** with knowledge base of past experiments
- ✅ **Automatic feature engineering** (polynomial, interactions, scaling)
- ✅ **Multiple activation functions** (ReLU, Tanh, Sigmoid, ELU, Leaky ReLU)
- ✅ **Dynamic hyperparameter optimization**
- ✅ **Zero-downtime deployments** with health checks
- ✅ **REST API** for training, prediction, and evolution
- ✅ **CLI utility** with colorful progress displays
- ✅ **Model versioning** and checkpoint management
- ✅ **TensorBoard integration** for visualization

### Evolution Features

- 🧬 **Population-based search** with configurable size
- 🎯 **Tournament selection** for parent selection
- 🔀 **Crossover and mutation** operators
- 🏆 **Elitism** to preserve best individuals
- 📊 **Fitness evaluation** on validation data
- 📈 **Real-time statistics** and progress tracking

---

## 🏗️ Architecture

```
NeuralForge/
├── main.hs           # Entry point and CLI router
├── pipeline.hs       # Training and evolution pipeline
├── genetics.hs       # Genetic algorithm implementation
├── api_server.hs     # REST API server
├── cli.hs            # CLI utilities and display
├── config.yaml       # AutoML configuration
└── testdata.csv      # Sample dataset
```

### Component Interaction

```
┌─────────────┐
│   main.hs   │  ← Entry Point
└──────┬──────┘
       │
   ┌───┴────┐
   │        │
   ▼        ▼
┌──────┐ ┌─────────┐
│ CLI  │ │   API   │
└──┬───┘ └────┬────┘
   │          │
   └────┬─────┘
        │
        ▼
  ┌──────────┐
  │ Pipeline │  ← Training & Evolution
  └─────┬────┘
        │
        ▼
   ┌──────────┐
   │ Genetics │  ← GA Engine
   └──────────┘
```

---

## 🚀 Quick Start

### Prerequisites

- GHC 8.10+ or Stack
- cabal-install 3.0+

### Installation

```bash
# Clone the repository
git clone https://github.com/SnakeEye-sudo/NeuralForge.git
cd NeuralForge

# Install dependencies
cabal update
cabal install --only-dependencies

# Build the project
cabal build

# Or using Stack
stack build
```

### Run Your First Training

```bash
# Train with default configuration
cabal run neuralforge -- train config.yaml

# Or run evolution for 50 generations
cabal run neuralforge -- evolve config.yaml 50
```

---

## 💻 Usage

### CLI Usage

#### Training a Model

```bash
# Basic training
neuralforge train config.yaml

# With custom epochs (via config)
# Edit config.yaml and set epochs: 200
neuralforge train config.yaml
```

#### Evolving Architecture

```bash
# Evolve for 100 generations
neuralforge evolve config.yaml 100

# The best architecture will be displayed
# Generation 100
# Best Fitness: 0.9723
# Best Architecture: [128, 256, 128, 64, 32]
```

#### Making Predictions

```bash
# Run predictions on new data
neuralforge predict models/best_model.bin testdata.csv

# Output:
# Predictions:
# 0.8234
# 0.1456
# 0.9123
# ...
```

#### Starting API Server

```bash
# Start on default port (8080)
neuralforge serve

# Start on custom port
neuralforge serve 3000
```

#### Exporting Models

```bash
# Export trained model
neuralforge export models/model.bin exports/production_model.onnx
```

#### Check Status

```bash
# View system status
neuralforge status

# ✓ NeuralForge System Status
#   - Core Engine: Running
#   - Evolution Engine: Ready
#   - API Server: Available
#   - GPU Acceleration: Enabled
```

---

### REST API Usage

#### Health Check

```bash
curl http://localhost:8080/api/health

# Response:
{
  "status": "ok",
  "message": "NeuralForge API is running"
}
```

#### Train Model

```bash
curl -X POST http://localhost:8080/api/train \
  -H "Content-Type: application/json" \
  -d '{
    "configPath": "config.yaml",
    "epochs": 100
  }'

# Response:
{
  "status": "success",
  "message": "Training started",
  "data": "Training with 100 epochs"
}
```

#### Evolve Architecture

```bash
curl -X POST http://localhost:8080/api/evolve \
  -H "Content-Type: application/json" \
  -d '{
    "config": "config.yaml",
    "generations": 50
  }'

# Response:
{
  "status": "success",
  "message": "Evolution started",
  "data": "Running 50 generations"
}
```

#### Make Predictions

```bash
curl -X POST http://localhost:8080/api/predict \
  -H "Content-Type: application/json" \
  -d '{
    "modelPath": "models/best_model.bin",
    "dataPath": "testdata.csv"
  }'

# Response:
{
  "status": "success",
  "message": "Predictions generated",
  "data": "Model: models/best_model.bin"
}
```

---

## ⚙️ Configuration

Edit `config.yaml` to customize NeuralForge behavior:

### Training Configuration

```yaml
training:
  dataPath: "./testdata.csv"
  outputPath: "./models/neuralforge_model.bin"
  epochs: 100
  validationSplit: 0.2
  earlyStoppingPatience: 10
```

### Architecture Configuration

```yaml
architecture:
  layers: [128, 64, 32]
  activation: "relu"
  learningRate: 0.001
  batchSize: 32
  dropout: 0.3
  optimizer: "adam"
```

### Auto Feature Engineering

```yaml
autoFeatureEngineering:
  enabled: true
  polynomialDegree: 2
  interactions: true
  scaling: "standard"
  featureSelection:
    enabled: true
    method: "mutual_info"
    threshold: 0.05
```

### Evolution Configuration

```yaml
evolution:
  populationSize: 50
  generations: 100
  tournamentSize: 5
  mutationRate: 0.1
  crossoverRate: 0.8
  elitism: 5
```

### Search Space

```yaml
searchSpace:
  layers:
    min: 2
    max: 5
  neuronsPerLayer:
    min: 32
    max: 512
  activations:
    - relu
    - tanh
    - sigmoid
    - elu
    - leaky_relu
  learningRate:
    min: 0.0001
    max: 0.1
    log_scale: true
```

---

## 📚 Examples

### Example 1: Basic Classification

```bash
# 1. Prepare your data in CSV format (like testdata.csv)
# 2. Configure training parameters
# 3. Run training
neuralforge train config.yaml

# 4. Monitor progress with colorful CLI output
# [████████████████████████████████████████] 100% | Epoch: 100/100 | Loss: 0.0234
```

### Example 2: Architecture Evolution

```bash
# Let NeuralForge find the best architecture
neuralforge evolve config.yaml 50

# Watch as it evolves:
# Generation 1
# Best Fitness: 0.7234
# Best Architecture: [256, 128, 64]
#
# Generation 50
# Best Fitness: 0.9723
# Best Architecture: [384, 256, 192, 96, 48]
```

### Example 3: API Integration

```python
import requests

# Start evolution via API
response = requests.post('http://localhost:8080/api/evolve', json={
    'config': 'config.yaml',
    'generations': 100
})

print(response.json())
# {'status': 'success', 'message': 'Evolution started', ...}
```

---

## 🛠️ Development

### Building from Source

```bash
# Using Cabal
cabal clean
cabal configure
cabal build

# Using Stack
stack clean
stack build
```

### Running Tests

```bash
# Run test suite
cabal test

# Or with Stack
stack test
```

### Code Structure

- **main.hs**: Entry point, command-line argument parsing
- **pipeline.hs**: Training pipeline, evolution orchestration
- **genetics.hs**: GA operators (selection, crossover, mutation)
- **api_server.hs**: WAI/Warp-based REST API
- **cli.hs**: Terminal UI utilities with ANSI colors

---

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

---

## 📄 License

This project is licensed under the MIT License.

---

## 🙏 Acknowledgments

- Inspired by AutoML research and neural architecture search
- Built with Haskell's powerful type system and functional paradigm
- Leveraging genetic algorithms for optimization

---

## 📞 Contact

**Er. Sangam Krishna**  
GitHub: [@SnakeEye-sudo](https://github.com/SnakeEye-sudo)  
Email: krishna.sangam11rsm@gmail.com

---

**⭐ Star this repository if you find it useful!**
