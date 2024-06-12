/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonConstants;
import com.acm.service.IScroeFileService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.IScoreDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link IScroeFileServiceImpl} class.
 *
 * @author YesserSomai
 * @since 1.0.5
 */
@Service
public class IScroeFileServiceImpl implements IScroeFileService {

	/** Default Mode is INFO. */
	private static final Logger logger = LoggerFactory.getLogger(IScroeFileServiceImpl.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The file reject. */
	private ByteArrayOutputStream fileReject = null;

	/** The file system storage service impl. */
	@Autowired
	FileSystemStorageServiceImpl fileSystemStorageServiceImpl;

	/**
	 * Generate file.
	 *
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return the byte[]
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.IScroeFileService#generateFile()
	 */
	@Override
	public byte[] generateFile(String startDate, String endDate) throws IOException {

		logger.info("START generating I-SCORE.dlt File");

		String formatedStartDate = "";
		String formatedEndDate = "";
		try {
			formatedStartDate = DateUtil
					.formatDate((new SimpleDateFormat("yyyy-MM-dd")).parse(startDate), "MMddYYYY");
			formatedEndDate = DateUtil
					.formatDate((new SimpleDateFormat("yyyy-MM-dd")).parse(endDate), "MMddYYYY");
		}
		catch (ParseException e) {
			// TODO Auto-generated catch block
			logger.error("Fails to format Start or End dates : '{}' - '{}'", startDate, endDate);
		}

		// Set the cache

		byte[] body = null;
		// Byte stream read String data
		List<IScoreDTO> iScoreDTOs = transversClient.findIScore(startDate, endDate);
		logger.info("Loaded I-score data from ABACUS :: {} records loaded ", iScoreDTOs.size());
		// create system date + end of month date with format = MMddYYYY

		// create file body
		ByteArrayOutputStream file = new ByteArrayOutputStream();
		fileReject = null;
		int countReject = 0;
		// write into file body
		file.write((CommonConstants.I_SCORE_HEADER_1 + formatedStartDate + "|" + formatedEndDate
				+ CommonConstants.I_SCORE_HEADER_2).getBytes(StandardCharsets.UTF_8));

		int countOK = 0;
		for (IScoreDTO iScoreDTO : iScoreDTOs) {
			if (!ACMValidationUtils.isNullOrEmpty(iScoreDTO.getiScoreCustomer())
					&& !ACMValidationUtils.isNullOrEmpty(iScoreDTO.getiScoreLoan())) {
				file.write((iScoreDTO.getiScoreLoan() + "\r\n").getBytes(StandardCharsets.UTF_8));
				file.write(
						(iScoreDTO.getiScoreCustomer() + "\r\n").getBytes(StandardCharsets.UTF_8));
				countOK++;
			}
			else {
				if (countReject == 0) {
					fileReject = new ByteArrayOutputStream();
					fileReject.write(
							"I-Score Rejected Records : \r\n".getBytes(StandardCharsets.UTF_8));
				}
				countReject++;
				fileReject.write(((ACMValidationUtils.isNullOrEmpty(iScoreDTO.getiScoreLoan())
						? "-- NO CNCF !"
						: iScoreDTO.getiScoreLoan()) + "\r\n").getBytes(StandardCharsets.UTF_8));
				fileReject.write(((ACMValidationUtils.isNullOrEmpty(iScoreDTO.getiScoreCustomer())
						? "-- NO CNCS ! "
						: iScoreDTO.getiScoreCustomer()) + "\r\n")
								.getBytes(StandardCharsets.UTF_8));
			}
		}

		file.write(("TLTL|MF00180001|0000000000|" + countOK).getBytes(StandardCharsets.UTF_8));
		// processing
		fileSystemStorageServiceImpl.saveFile(file.toByteArray());
		logger.info("I-score dlt file Saved localy on Report directory");

		ByteArrayInputStream iScoreByteArrayInputStream =
				new ByteArrayInputStream(file.toByteArray());
		body = new byte[iScoreByteArrayInputStream.available()];
		iScoreByteArrayInputStream.read(body);

		logger.info("I-score dlt generation DONE : Returning byte");
		return file.toByteArray();
	}

	/**
	 * Gets the reject file.
	 *
	 * @return the reject file
	 */
	/* (non-Javadoc)
	 * @see com.acm.service.IScroeFileService#getRejectFile()
	 */
	@Override
	public byte[] getRejectFile() {

		if (!ACMValidationUtils.isNullOrEmpty(fileReject)) {
			return fileReject.toByteArray();
		}
		return null;
	}

}
