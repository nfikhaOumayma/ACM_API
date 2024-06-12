/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.io.IOException;

/**
 * {@link IScroeFileService} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
public interface IScroeFileService {

	/**
	 * Generate file.
	 *
	 * @author YesserSomai
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	byte[] generateFile(String startDate, String endDate) throws IOException;

	/**
	 * Gets the reject file.
	 *
	 * @return the reject file
	 */
	byte[] getRejectFile();
}
