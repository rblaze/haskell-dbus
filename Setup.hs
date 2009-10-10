import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.PackageDescription
import Distribution.Verbosity
import System.FilePath

main = defaultMainWithHooks simpleUserHooks
	{ hookedPreProcessors = [("nw", nwPreprocessor)]
	, hookedPrograms = [notangleProgram]
	}

notangleProgram :: Program
notangleProgram = simpleProgram "notangle"

nwPreprocessor :: BuildInfo -> LocalBuildInfo -> PreProcessor
nwPreprocessor _ lbi = PreProcessor
	{ platformIndependent = True
	, runPreProcessor = \(inBaseDir, inRelativeFile)
	                     (outBaseDir, outRelativeFile) verbosity -> do
		let inFile = normalise (inBaseDir  </> inRelativeFile)
		let outFile = normalise (outBaseDir </> outRelativeFile)
		let cppInput = replaceExtension outFile ".cpphs"
		
		-- Run notangle
		debug verbosity ("Tangling " ++ inFile ++ " to " ++ cppInput)
		hs <- rawSystemProgramStdoutConf verbosity notangleProgram
			(withPrograms lbi)
			["-R" ++ outRelativeFile, "DBus.nw", inFile]
		writeUTF8File cppInput hs
		
		-- Run cpphs
		debug verbosity ("Preprocessing " ++ cppInput)
		rawSystemProgramConf verbosity cpphsProgram
			(withPrograms lbi)
			[cppInput, "--hashes", "--noline", "-O" ++ outFile]
	}
