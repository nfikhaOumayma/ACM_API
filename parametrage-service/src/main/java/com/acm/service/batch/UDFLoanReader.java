/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ItemReader;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.service.UserDefinedFieldsService;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link UDFLoanReader} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class UDFLoanReader implements ItemReader<UserDefinedFieldsLinksDTO> {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(UDFLoanReader.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The user defined fields service. */
	@Autowired
	private UserDefinedFieldsService userDefinedFieldsService;

	/** The count. */
	private int count = 0;

	/** The job parameters. */
	private JobParameters jobParameters;

	/** The user defined fields links abacus. */
	private List<UserDefinedFieldsLinksDTO> userDefinedFieldsLinksAbacus;

	/**
	 * Before step.
	 *
	 * @param stepExecution the step execution
	 */
	@BeforeStep
	public void beforeStep(StepExecution stepExecution) {

		JobExecution jobExecution = stepExecution.getJobExecution();
		jobParameters = jobExecution.getJobParameters();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public UserDefinedFieldsLinksDTO read() {

		logger.debug("### init UDFLoan reader process");
		// load data from ABACUS
		// load data from ABACUS
		if (userDefinedFieldsLinksAbacus == null) {
			userDefinedFieldsLinksAbacus = load();
		}
		// reading founded list if not empty
		if (count < userDefinedFieldsLinksAbacus.size()) {
			UserDefinedFieldsLinksDTO userDefinedFieldsLinksDTO =
					userDefinedFieldsLinksAbacus.get(count);
			// find && setting udf field object
			List<UserDefinedFieldsDTO> userDefinedFieldsDTOs = userDefinedFieldsService
					.find(userDefinedFieldsLinksDTO.getUserDefinedFieldsDTO());
			if (!ACMValidationUtils.isNullOrEmpty(userDefinedFieldsDTOs)) {
				userDefinedFieldsLinksDTO.setUserDefinedFieldsDTO(userDefinedFieldsDTOs.get(0));
				count++;
				return userDefinedFieldsLinksDTO;
			}
			else {
				count++;
				return null;
			}
		}
		else {
			count = 0;
		}
		return null;
	}

	/**
	 * Load data from ABACUS using idAccountExtern as parameters.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	private List<UserDefinedFieldsLinksDTO> load() {

		try {
			// check passed params
			if (jobParameters != null
					&& !ACMValidationUtils.isNullOrEmpty(jobParameters.getParameters())
					&& !ACMValidationUtils
							.isNullOrEmpty(jobParameters.getLong("idAccountExtern"))) {
				// listing Map of params
				jobParameters.getParameters()
						.forEach((k, v) -> logger.debug("KEY = {} , VALUE = {}", k, v));
			}
			else {
				logger.error(
						"****** Failed to load list UDF from ABACUS : Loan ID_ACCOUNT_EXTERN is missing ******");
				return new ArrayList<>();
			}

			// loading list customer from ABACUS DB
			List<UserDefinedFieldsLinksDTO> list =
					transversClient.loadUDFByLoan(jobParameters.getLong("idAccountExtern"));
			logger.debug("List UDFs Loan size = {}", list.size());
			return list;
		}
		catch (FeignException e) {
			logger.error("Failed to get list");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
		return new ArrayList<>();
	}
}
