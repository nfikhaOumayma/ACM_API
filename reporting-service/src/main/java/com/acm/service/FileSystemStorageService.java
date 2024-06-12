/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.File;
import java.io.IOException;

// TODO: Auto-generated Javadoc
/**
 * {@link FileSystemStorageService} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
public interface FileSystemStorageService {

	/**
	 * Inits the.
	 */
	void init();

	/**
	 * Delete all.
	 */
	void deleteAll();

	/**
	 * Jrxml file exists.
	 *
	 * @param file the file
	 * @return true, if successful
	 */
	boolean jrxmlFileExists(String file);

	/**
	 * Jasper file exists.
	 *
	 * @param file the file
	 * @return true, if successful
	 */
	boolean jasperFileExists(String file);

	/**
	 * Load jrxml file.
	 *
	 * @param file the file
	 * @return the string
	 */
	String loadJrxmlFile(String file);

	/**
	 * Load jasper file.
	 *
	 * @param file the file
	 * @return the file
	 */
	File loadJasperFile(String file);

	/**
	 * Load file.
	 *
	 * @param file the file
	 * @return the file
	 */
	File loadFile(String file);

	/**
	 * Save file.
	 *
	 * @param bytes the bytes
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	void saveFile(byte[] bytes) throws IOException;

}
