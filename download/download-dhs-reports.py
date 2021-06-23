import os
from pdfrw import PdfReader
import urllib

os.chdir('/home/mattcoop/mortalityblob/mortnew/admin_matching/DHS reports/')

# Download all DHS final reports
for i in range(400):
    urllib.request.urlretrieve("https://dhsprogram.com/pubs/pdf/FR" + str(i)
            + "/FR" + str(i) + ".pdf", 
            "FR" + str(i) + ".pdf")

# Delete files smaller than 100kb (Html with errors)
os.system('find . -size -100k -delete')

# Rename file to match title (in metadata)
def renameFileToPDFTitle(path, fileName):
    fullName = os.path.join(path, fileName)
    # Extract pdf title from pdf file
    newName = PdfReader(fullName).Info.Title
    # Remove surrounding brackets that some pdf titles have
    newName = newName.strip('()').replace('/', '') + '.pdf'
    newFullName = os.path.join(path, newName)
    os.rename(fullName, newFullName)


fs = [f for f in os.listdir('.') if f[:2] == 'FR']

for fileName in fs:
    # Rename only pdf files
    fullName = os.path.join('.', fileName)
    if (not os.path.isfile(fullName) or fileName[-4:] != '.pdf'):
        continue
    try:
        renameFileToPDFTitle('.', fileName)
    except:
        pass

#Manual last one
os.rename('FR126.pdf', 'Armenia DHS 2000.pdf')
