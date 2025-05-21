from flask import Flask,jsonify,request,send_file
from flask_cors import CORS
import zipfile
import io

from parceResponce import *
app = Flask(__name__)
CORS(app)

def get_generated_files_texts(directory):
    result = {}
    for root, _, files in os.walk(directory):
        for fname in files:
            path = os.path.join(root, fname)
            with open(path, 'r', encoding='utf-8') as f:
                result[fname] = f.read()
    return result


@app.route('/')
def hello():
    return "Hello World!"

def make_zip_from_folder(folder_path):
    memory_file = io.BytesIO()
    with zipfile.ZipFile(memory_file, 'w', zipfile.ZIP_DEFLATED) as zf:
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                full_path = os.path.join(root, file)
                arcname = os.path.relpath(full_path, folder_path)
                zf.write(full_path, arcname)
    memory_file.seek(0)
    return memory_file


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

        if os.path.exists(os.path.join(current_dir,"hydra")): shutil.rmtree(os.path.join(current_dir,"hydra"))

        strToFile(source_code,source_lang)
        print("Перевод из строки в начальный файл")
        res = fileToDSL(source_lang)
        print("Перевод из файла в DSL")
        if res==False: 
            return jsonify({
            "success": False
            })
        # with open(os.path.join(current_dir,"genDSL.hs"),'r') as f: res = f.read()
        responce = DSLToLang(res,target_lang)
        print("Перевод из DSL в другой язык")
        if responce.ok:
            zip_memory_file = make_zip_from_folder(current_dir)  # путь к папке с результатом
            return send_file(
                zip_memory_file,
                mimetype='application/zip',
                as_attachment=True,
                download_name='result.zip'
            )
        else:
            return jsonify({
            "success": False
            })

        
        # Здесь должна быть ваша логика трансляции
        # translated_code = f"Translated {source_code} from {source_lang} to {target_lang}"
        
        # return jsonify({
        #     "success": True,
        #     "translated_code": translated_code
        # })
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)