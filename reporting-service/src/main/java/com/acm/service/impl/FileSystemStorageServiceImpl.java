/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.service.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.FileSystemUtils;

import com.acm.ApplicationProperties;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.FileSystemStorageService;

// TODO: Auto-generated Javadoc
/**
 * {@link FileSystemStorageServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class FileSystemStorageServiceImpl implements FileSystemStorageService {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(FileSystemStorageServiceImpl.class);

	/** The root location. */
	private final Path rootLocation;

	/** The root location jrxml. */
	private final Path rootLocationJrxml;

	/** The properties. */
	private final ApplicationProperties properties;

	/**
	 * Instantiates a new file system storage service.
	 *
	 * @param properties the properties
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public FileSystemStorageServiceImpl(ApplicationProperties properties) throws IOException {

		this.properties = properties;
		this.rootLocation = Paths.get(properties.getStorageLocation().getURL().getPath());
		this.rootLocationJrxml = Paths.get(properties.getStorageLocationJrxml().getURL().getPath());

	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#init()
	 */
	@Override
	public void init() {

		try {
			initializeStorage(rootLocation);
		}
		catch (ResourcesNotFoundException e) {
			logger.error("failed to initialize File System Storage {}", e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#deleteAll()
	 */
	@Override
	public void deleteAll() {

		try {
			delete(rootLocation);
		}
		catch (ResourcesNotFoundException e) {
			logger.error("failed to delete File System Storage {}", e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#jrxmlFileExists(java.lang. String)
	 */
	@Override
	public boolean jrxmlFileExists(String file) {

		try {
			Path reportFile = Paths.get(properties.getStorageLocationJrxml().getURI());
			reportFile = reportFile.resolve(file + ".jrxml");
			return Files.exists(reportFile);
		}
		catch (IOException e) {
			logger.error("Error while trying to get file URI : {}", e.getMessage());
			return false;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#loadJrxmlFile(java.lang.String)
	 */
	@Override
	public String loadJrxmlFile(String file) {

		Path reportFile = rootLocationJrxml;
		reportFile = reportFile.resolve(file + ".jrxml");
		return reportFile.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#jasperFileExists(java.lang. String)
	 */
	@Override
	public boolean jasperFileExists(String file) {

		Path reportFile = rootLocation;
		reportFile = reportFile.resolve(file + ".jasper");
		return Files.exists(reportFile);
	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#loadJasperFile(java.lang.String)
	 */
	@Override
	public File loadJasperFile(String file) {

		Path reportFile = rootLocation;
		reportFile = reportFile.resolve(file + ".jasper");
		return reportFile.toFile();
	}

	/*
	 * (non-Javadoc)
	 * @see com.juliuskrah.jasper.storage.StorageService#loadJasperFile(java.lang.String)
	 */
	@Override
	public File loadFile(String file) {

		Path reportFile = rootLocation;
		reportFile = reportFile.resolve(file);
		return reportFile.toFile();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.FileSystemStorageService#saveFile(byte[])
	 */
	@Override
	public void saveFile(byte[] bytes) throws IOException {

		File tempFile = File.createTempFile("IScore_", ".dlt", rootLocation.toFile());
		FileOutputStream fos = new FileOutputStream(tempFile);
		fos.write(bytes);
		fos.close();
	}

	/**
	 * static method add to factorize source code.
	 *
	 * @param rootLocation the rootLocation to be deleted
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public static void delete(Path rootLocation) throws ResourcesNotFoundException {

		try {
			FileSystemUtils.deleteRecursively(rootLocation);
		}
		catch (IOException e) {
			throw new ResourcesNotFoundException("Could not delete files and folders");
		}
	}

	/**
	 * static method to initialize storage.
	 *
	 * @param rootLocation the rootLocation to create directory
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public static void initializeStorage(Path rootLocation) throws ResourcesNotFoundException {

		try {
			Files.createDirectory(rootLocation);
		}
		catch (IOException e) {
			throw new ResourcesNotFoundException("Could not initialize storage");
		}
	}
}
