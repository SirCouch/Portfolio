import tkinter as tk
from transformers import AutoTokenizer, AutoModelForSeq2SeqLM

# Initialize the tokenizer and model
tokenizer = AutoTokenizer.from_pretrained('t5-base')
model = AutoModelForSeq2SeqLM.from_pretrained('t5-base', return_dict=True)

def summarize():
    # Get the text from the input_text widget
    text = input_text.get("1.0", "end-1c")

    # Use the model to summarize the text
    inputs = tokenizer.encode("summarize: " + text, return_tensors='pt', max_length=50000, truncation=True)
    summary_ids = model.generate(inputs, max_length=15000, min_length=80, length_penalty=5., num_beams=2)
    summary = tokenizer.decode(summary_ids[0])

    # Display the summary in the output_text widget
    output_text.delete("1.0", tk.END)
    output_text.insert(tk.END, summary)

# Create the root widget
root = tk.Tk()

# Add the input label, text and button widgets
input_label = tk.Label(root, text="Input Text:")
input_label.pack()
input_text = tk.Text(root, height=10)
input_text.pack()
summarize_button = tk.Button(root, text="Summarize", command=summarize)
summarize_button.pack()

# Add the output label and text widgets
output_label = tk.Label(root, text="Summarized Text:")
output_label.pack()
output_text = tk.Text(root, height=10)
output_text.pack()

# Start the event loop
root.mainloop()
#%%
