package edu.umich.dice3;

public class FreeDiceAgentConfiguration {
	
    public final String sourceFile;
    public final String writeFile;
    public final String gpFile;
    public final String qnaFile;
    public final String reteFile;
    
    
    public FreeDiceAgentConfiguration(String parentPath, String beginTime, String sourceFile, String writeFile, String qnaFile, String gpFile) {
        this(parentPath, beginTime, sourceFile, writeFile, qnaFile, gpFile, null);
    }
        
	public FreeDiceAgentConfiguration(String parentPath, String beginTime, String sourceFile, String writeFile, String qnaFile, String gpFile, String writeOverride) {
	    String slash = System.getProperty("file.separator");
		this.sourceFile = sourceFile == null ? null : parentPath + slash + sourceFile;
		this.qnaFile = qnaFile == null ? null : parentPath + slash + qnaFile;
        this.gpFile = gpFile == null ? null : parentPath + slash + gpFile;
        
        if (writeOverride != null)
        {
            this.writeFile = parentPath + slash + writeOverride;
        }
        else
        {
            String writeBase = removeExtension(writeFile);
            this.writeFile = writeFile == null ? null : parentPath + slash + writeBase + "_" + beginTime + ".soar";
        }
        this.reteFile = this.writeFile == null ? null : removeExtension(this.writeFile) + ".rete"; 
	}
	
	/**
	 * Removes the file extension from the String.
	 * @return <em>file</em>, up to its first period.
	 */
	private String removeExtension(String file)
	{
	    if (file == null) return null;
	    int index = file.lastIndexOf('.');
	    if (index == -1) return file;
	    return file.substring(0, index);
	}
	   
    public String getEscapedWriteFile()
    {
        return writeFile.replace("\\", "\\\\");
    }
    
    public String getEscapedSourceFile()
    {
        return sourceFile.replace("\\", "\\\\");
    }
    
    public String getEscapedGpFile()
    {
        return gpFile.replace("\\", "\\\\");
    }
    
    public String getEscapedReteFile()
    {
        if (reteFile == null) return null;
        return reteFile.replace("\\", "\\\\");
    }
    
    public String getEscapedRetePathWithAppending(String append)
    {
        return removeExtension(getEscapedReteFile()) + append + ".rete";
    }
    
    public String getEscapedWritePathWithAppending(String append)
    {
        return removeExtension(getEscapedWriteFile()) + append + ".soar";
    }
    
    public String getEscapedRlUpdateLogPathWithAppending(String append)
    {
        if (writeFile == null) {
            return null;
        }
        return removeExtension(getEscapedWriteFile()) + "_rl-update-log_" + append + ".txt";
    }
}
