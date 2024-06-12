/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link JobInvokerController} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/batch-jobs")
public class JobInvokerController {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(JobInvokerController.class);

	/** The job launcher. */
	@Autowired
	private JobLauncher jobLauncher;

	/** The import UDF customer job. */
	@Autowired
	@Qualifier("importUDFCustomerJob")
	private Job importUDFCustomerJob;

	/** The import UDF loan job. */
	@Autowired
	@Qualifier("importUDFLoanJob")
	private Job importUDFLoanJob;

	/** The process customer job. */
	@Autowired
	@Qualifier("processCustomerJob")
	private Job processCustomerJob;
	/** The process branch change job. */
	@Autowired
	@Qualifier("processBranchChangeJob")
	private Job processBranchChangeJob;

	/** The process transfer portfolio job. */
	@Autowired
	@Qualifier("processTransferPortfolioJob")
	private Job processTransferPortfolioJob;

	/** The collection proccess. */
	@Autowired
	@Qualifier("collectionProccess")
	private Job collectionProccess;

	/**
	 * run manually the job called "importUDFCustomerJob" to load UDF from Abacus-DB to ACM-DB.
	 *
	 * @author HaythemBenizid
	 * @param customerIdExtern the customer id extern
	 * @return the string
	 * @throws Exception the exception
	 */
	@GetMapping("/import-UDF-customer-job/{customerIdExtern}")

	/**
	 * Handle import UDF customer job.
	 *
	 * @param customerIdExtern the customer id extern
	 * @return the string
	 * @throws Exception the exception
	 */
	public String handleImportUDFCustomerJob(
			@PathVariable("customerIdExtern") Long customerIdExtern) throws Exception {

		JobParameters jobParameters =
				new JobParametersBuilder().addLong("time", System.currentTimeMillis())
						.addLong("customerIdExtern", customerIdExtern).toJobParameters();
		JobExecution jobExecution = jobLauncher.run(importUDFCustomerJob, jobParameters);
		BatchStatus batchStatus = jobExecution.getStatus();
		logger.info("batchStatus: {}", batchStatus);
		return "BATCH [importUDFCustomerJob] HAS BEEN INVOKED";
	}

	/**
	 * Handle collection.
	 * 
	 * @author idridi
	 * @return the string
	 * @throws Exception the exception
	 */
	//@Scheduled(cron = "#{@getCollectionCronValue}")
	@GetMapping("/get-collection-by-batch")
	public String handleCollection() throws Exception {

		JobParameters jobParameters = new JobParametersBuilder()
				.addLong("time", System.currentTimeMillis()).toJobParameters();
		JobExecution jobExecution = jobLauncher.run(collectionProccess, jobParameters);
		BatchStatus batchStatus = jobExecution.getStatus();
		logger.info("batchStatus: {}", batchStatus);
		return JSONObject.quote("Collection Job has been invoked");
	}

	/**
	 * Handle import customers job.
	 * 
	 * @author idridi
	 * @return the string
	 * @throws Exception the exception
	 */
	@GetMapping("/import-customers-job")
	public String handleImportCustomersJob() throws Exception {

		JobParameters jobParameters = new JobParametersBuilder()
				.addLong("time", System.currentTimeMillis()).toJobParameters();
		JobExecution jobExecution = jobLauncher.run(processCustomerJob, jobParameters);
		BatchStatus batchStatus = jobExecution.getStatus();
		logger.info("batchStatus: {}", batchStatus);
		return JSONObject.quote("BATCH [processCustomerJob] HAS BEEN INVOKED");
	}

	/**
	 * run manually the job called "importUDFLoanJob" to load UDF from Abacus-DB to ACM-DB.
	 *
	 * @author HaythemBenizid
	 * @param idAccountExtern the id account extern
	 * @return the string
	 * @throws Exception the exception
	 */
	@GetMapping("/import-UDF-loan-job/{idAccountExtern}")
	public String handleImportUDFLoanJob(@PathVariable("idAccountExtern") Long idAccountExtern)
			throws Exception {

		JobParameters jobParameters =
				new JobParametersBuilder().addLong("time", System.currentTimeMillis())
						.addLong("idAccountExtern", idAccountExtern).toJobParameters();
		JobExecution jobExecution = jobLauncher.run(importUDFLoanJob, jobParameters);
		BatchStatus batchStatus = jobExecution.getStatus();
		logger.info("batchStatus: {}", batchStatus);
		return "BATCH [importUDFLoanJob] HAS BEEN INVOKED";
	}

	/**
	 * Update customers branches.
	 * 
	 * @author mlamloum
	 * @return the string
	 * @throws Exception the exception
	 */
	@GetMapping("/update-customers-branches")
	public String updateCustomersBranches() throws Exception {

		JobParameters jobParameters = new JobParametersBuilder()
				.addLong("time", System.currentTimeMillis()).toJobParameters();
		JobExecution jobExecution = jobLauncher.run(processBranchChangeJob, jobParameters);
		BatchStatus batchStatus = jobExecution.getStatus();
		logger.info("batchStatus: {}", batchStatus);
		return JSONObject.quote("BATCH [updateCustomersBranches] HAS BEEN INVOKED");
	}

	/**
	 * Syncronized portfolio job.
	 *
	 * @return the string
	 * @throws Exception the exception
	 */

	@GetMapping("/syncronized-portfolio-job")
	public String syncronizedPortfolioJob() throws Exception {

		JobParameters jobParameters = new JobParametersBuilder()
				.addLong("time", System.currentTimeMillis()).toJobParameters();
		JobExecution jobExecution = jobLauncher.run(processTransferPortfolioJob, jobParameters);
		BatchStatus batchStatus = jobExecution.getStatus();
		logger.info("batchStatus: {}", batchStatus);
		return JSONObject.quote("BATCH [syncronizedPortfolioJob] HAS BEEN INVOKED");
	}
}
