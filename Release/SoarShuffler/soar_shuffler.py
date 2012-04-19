import os
import zipfile
import shutil
import pickle
import re

SPL = dict()
Repository_Dir = "C:\\Soar\\repository\\trunk\\"
Output_Dir = "C:\\Soar\\output\\"

def clean_output_dir():
    if (os.path.exists(Output_Dir)):
        shutil.rmtree(Output_Dir)
        while (os.path.exists(Output_Dir)):
            pass
    os.makedirs(Output_Dir)

def find_latest_date(projectName):
    latest_date = -1
    for a,b in SPL[projectName]["copyList"]:
        source = os.path.join(Repository_Dir, a)
        for root, dirs, files in os.walk(source):
            for name in files:
                this_time = os.path.getmtime(os.path.join(root, name))
                if (this_time > latest_date):
                    latest_date = this_time
            for name in dirs:
                this_time = os.path.getmtime(os.path.join(root, name))
                if (this_time > latest_date):
                    latest_date = this_time
    return latest_date
            
def load_project_list():
    with open('Soar_Projects_Filelist.py', 'r') as f_filelist:
        for line in f_filelist:
            line = str.strip(line)
            if (not line[0]=="#"):
                split_entry = str.split(line,"=")
                if (len(line) > 0):
                    if (len(split_entry) == 1):
                        SPL[split_entry[0]] = dict([('copyList', list()), ('type', 'zip'), ('out', '')])
                        current_project = split_entry[0]
                    elif (split_entry[0] in ('type', 'out')):
                        SPL[current_project][split_entry[0]] = split_entry[1]
                    else:
                        SPL[current_project]['copyList'].append((split_entry[0],split_entry[1]))
    print "Project list loaded."

				
def prune_and_pickle(shouldPrune):  	
    with open('Soar_Projects_Touchdates.txt', 'r') as f_touchdates:
        SPL2 = pickle.load(f_touchdates)
    for p,d in SPL.iteritems():
        SPL[p]["touchdate"] = find_latest_date(p)
    with open('Soar_Projects_Touchdates.txt', 'w') as f_touchdates:
        pickle.dump(SPL,f_touchdates)

    if shouldPrune:
        # Remove entries from SPL that have not been touched since last time
        valid_keys1 = SPL.keys()
        valid_keys2 = SPL2.keys()
        for p,d in SPL2.iteritems():
            if (p in valid_keys1 and p in valid_keys2):
                if (SPL[p]["touchdate"] == SPL2[p]["touchdate"]):
                    print "Skipping", p, "because it has not been modified."
                    del SPL[p]
    
def copy_project(projectName):
    destination_path = os.path.join(Output_Dir, SPL[projectName]['out'])
    print "Copying project", projectName
    if (not os.path.exists(os.path.dirname(destination_path))):
        os.makedirs(os.path.dirname(destination_path))

    for a,b in SPL[projectName]["copyList"]:
        source = os.path.join(Repository_Dir, a)
        if b == "top":
            destination = destination_path
        else:
            destination = os.path.join(destination_path,b)
            print "Checking if destination", destination, "exists"
            if (not os.path.exists(destination)):
                print "Creating directory"
                os.makedirs(destination)
        print "--> Starting", source, "|", destination
        if (os.path.isdir(source)):
            shutil.copytree(source, os.path.join(destination,os.path.basename(source)))
        else:
            shutil.copy2(source, destination)		
      
def zip_project(projectName):
    destination_zip = os.path.join(Output_Dir, SPL[projectName]['out'],(projectName+".zip"))
    print "Zipping project", projectName
    if (not os.path.exists(os.path.dirname(destination_zip))):
        os.makedirs(os.path.dirname(destination_zip))

    with zipfile.ZipFile(destination_zip, 'w', compression=zipfile.ZIP_DEFLATED) as dest_zip:
        for a,b in SPL[projectName]["copyList"]:
            source = os.path.join(Repository_Dir, a)
            if b == "top":
                destination = ""
            else:
                destination = b
            if (os.path.isdir(source)):
                for root, dirs, files in os.walk(source):
                    for f in files:
                        fname = os.path.join(root, f)
                        dname = os.path.join(destination, os.path.relpath(fname, source))
                        #print "-- Adding", fname, dname
                        dest_zip.write(fname, dname , zipfile.ZIP_DEFLATED)
                    if not files and not dirs:
                        dname = os.path.join(destination, os.path.relpath(root, source)) + "/"
                        zipInfo = zipfile.ZipInfo(dname + "/")
                        #Some web sites suggest using 48 or 64.  Don't know if this line is necessary at all.
                        zipInfo.external_attr = 16
                        dest_zip.writestr(zipInfo, "")
            else:
                dname = os.path.join(destination, os.path.basename(source))
                #print "-- Adding", source, dname
                dest_zip.write(source, dname , zipfile.ZIP_DEFLATED)
        for zinfo in dest_zip.filelist:
            if (re.search('\\.sh', zinfo.filename)):
                zinfo.external_attr = 2180841472
                zinfo.internal_attr = 1
                zinfo.create_system = 3
            if (re.search('\\.command', zinfo.filename)):
                zinfo.external_attr = 2180988928
                zinfo.internal_attr = 0
                zinfo.create_system = 3


def specialize_project(projectName, platformName):
    print "Specializing", projectName,"for",platformName
    SPL_New = dict()
    SPL_New['type']='zip'
    SPL_New['out']=SPL[projectName]['out']
    SPL_New['copyList']=list()
    for a,b in SPL[projectName]["copyList"]:
        if ((platformName == "windows_64") or (platformName == "windows_32")):
            a = re.sub("RELEASE_DIR", "win", a)
            a = re.sub("\.LAUNCH_EXTENSION", ".bat", a)
            if (platformName == "windows_64"):
                a = re.sub("COMPILE_DIR", "windows64", a)
            else:
                a = re.sub("COMPILE_DIR", "windows32", a)
            if (re.search("\.DLL_EXTENSION",a)):
                a = re.sub("\.DLL_EXTENSION", ".dll", a)
                a = re.sub("\\\\lib", "\\\\", a)
        elif ((platformName == "linux_64") or (platformName == "linux_32")):
            a = re.sub("\.DLL_EXTENSION", ".so", a)
            a = re.sub("RELEASE_DIR", "linux", a)
            a = re.sub("\.LAUNCH_EXTENSION", ".sh", a)
            if (platformName == "linux_64"):
                a = re.sub("COMPILE_DIR", "linux64", a)
            else:
                a = re.sub("COMPILE_DIR", "linux32", a)
        elif (platformName == "OSX") :
            a = re.sub("COMPILE_DIR", "mac64", a)
            a = re.sub("RELEASE_DIR", "osx", a)
            a = re.sub("\.LAUNCH_EXTENSION", ".command", a)
            if (re.search("libJava_sml_ClientInterface",a)):
                a = re.sub("\.DLL_EXTENSION", ".jnilib", a)
            elif (re.search("_Python_sml_ClientInterface",a)):
                a = re.sub("\.DLL_EXTENSION", ".so", a)
            else:
                a = re.sub("\.DLL_EXTENSION", ".dylib", a)
        SPL_New['copyList'].append((a,b))
        #print "Adding", a, b
    return SPL_New
            
def print_attr(fileName):
    with zipfile.ZipFile(fileName, 'r') as dest_zip:
        for zinfo in dest_zip.filelist:
            print zinfo.filename, zinfo.internal_attr, zinfo.external_attr, zinfo.create_system
    
def doit(shouldPrune=True):
    if (not shouldPrune): clean_output_dir()
    load_project_list()
    SPL_New = dict()
    for p, i in SPL.iteritems():
        if SPL[p]["type"] == "multiplatform-zip":
            SPL_New[p + "-Windows_32bit"] = specialize_project(p,"windows_32")
            SPL_New[p + "-Windows_64bit"] = specialize_project(p,"windows_64")
            SPL_New[p + "-Linux_32bit"] = specialize_project(p,"linux_32")
            SPL_New[p + "-Linux_64bit"] = specialize_project(p,"linux_64")
            SPL_New[p + "-OSX"] = specialize_project(p,"OSX")
    for p, i in SPL_New.iteritems():
        SPL[p] = i
    prune_and_pickle(shouldPrune)
    for p, i in SPL.iteritems():
        if SPL[p]["type"] == "zip":
            zip_project(p)
        elif SPL[p]["type"] == "copy":
            copy_project(p)
