from flask import Flask, render_template, request,send_file,send_from_directory
from werkzeug import secure_filename
from extract import *

app = Flask(__name__)

@app.route('/upload')
def uploadafile():
   return render_template('homepage.html')
	
@app.route('/uploaders')
def upload_file():
      temp=display()
      return render_template('PLEASEWORK.html',temp=temp)

@app.route('/conflict', methods = ['GET', 'POST'])
def stocks():
   return render_template('PLEASEWORK.html')
   
if __name__ == '__main__':
   app.run(debug = True)


