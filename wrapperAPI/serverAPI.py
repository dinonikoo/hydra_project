from flask import Flask,jsonify,request
app = Flask(__name__)

@app.route('/')
def hello():
    return "Hello World!"

@app.route('/translate',methods=["POST"])
def translate():
    try:
        # Получаем JSON из запроса
        data = request.get_json()
        
        # Проверяем обязательные поля
        if not data or 'source_code' not in data:
            return jsonify({"error": "Invalid request"}), 400
            
        source_code = data['source_code']
        source_lang = data.get('source_lang', 'haskell')
        target_lang = data.get('target_lang', 'java')
        
        # Здесь должна быть ваша логика трансляции
        translated_code = f"Translated {source_code} from {source_lang} to {target_lang}"
        
        return jsonify({
            "success": True,
            "translated_code": translated_code
        })
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)