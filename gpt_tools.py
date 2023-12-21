import csv
import openai

# Your OpenAI API key
api_key = 'YOUR_API_KEY_HERE'
openai.api_key = api_key

# Function to read the CSV file and return tool names and prompts
def read_csv(file_path):
    data = []
    with open(file_path, newline='') as csvfile:
        reader = csv.reader(csvfile)
        next(reader)  # Skip the header
        for row in reader:
            data.append({
                'tool_name': row[0],
                'prompt': row[1]
            })
    return data

# Function to start a chat with ChatGPT
def start_chat(prompt):
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {
                "role": "system",
                "content": prompt
            }
        ]
    )
    return response['id']

# Function to continue the chat with ChatGPT
def continue_chat(chat_id, user_input):
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=[
            {
                "role": "user",
                "content": user_input
            }
        ],
        chat_id=chat_id
    )
    return response['choices'][0]['message']['content']

# Function to delete the chat
def delete_chat(chat_id):
    openai.ChatCompletion.delete(chat_id)

#Function to save the chat log
def save_chat_log(tool_number, chat_log):
    file_name = f"ai_tools_{tool_number}.log"
    with open(file_name, 'a') as log_file:
        log_file.write(chat_log)
        log_file.write("\n\n")

# Main function
def main():
    file_path = 'tool_prompts.csv'  # Replace with your CSV file path
    tools_data = read_csv(file_path)

    while True:
        print("Available AI Tools:")
        for idx, tool in enumerate(tools_data):
            print(f"{idx + 1}. {tool['tool_name']}")

        tool_choice = input("Enter the number of the desired tool (or 'exit' to quit): ")

        if tool_choice.lower() == 'exit':
            break

        try:
            tool_index = int(tool_choice) - 1
            selected_tool = tools_data[tool_index]
        except (ValueError, IndexError):
            print("Invalid input. Please enter a valid tool number.")
            continue

        tool_number = tool_index + 1  # Tool number for log file naming

        # Start a chat with the selected tool's initial prompt
        chat_id = start_chat(selected_tool['prompt'])
        chat_log = f"Tool: {selected_tool['tool_name']}\n"

        while True:
            user_input = input("Enter your prompt (or 'exit' to quit this tool): ")

            if user_input.lower() == 'exit':
                delete_chat(chat_id)
                save_chat_log(tool_number, chat_log)
                break

            # Continue the chat with user input
            response = continue_chat(chat_id, user_input)
            print("AI Response:", response)

            chat_log += f"User: {user_input}\nAI: {response}\n\n"

            additional_prompt = input("Do you want to add another prompt? (yes/no): ")
            if additional_prompt.lower() == 'no':
                delete_chat(chat_id)
                save_chat_log(tool_number, chat_log)
                break

if __name__ == "__main__":
    main()
