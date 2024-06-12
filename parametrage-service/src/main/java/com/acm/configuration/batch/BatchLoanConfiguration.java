/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.batch;

import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.launch.support.SimpleJobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import com.acm.service.batch.LoanReader;
import com.acm.service.batch.LoanWriter;
import com.acm.utils.models.Loan;

/**
 * {@link BatchLoanConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
// @Configuration
// @Import({BatchScheduler.class})
public class BatchLoanConfiguration {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(BatchLoanConfiguration.class);

	/** The job launcher. */
	@Autowired
	private SimpleJobLauncher simpleJobLauncher;

	/** The listener. */
	@Autowired
	private JobExecutionListener listener;

	/** The job builder factory. */
	private JobBuilderFactory jobBuilderFactory;

	/** The step builder factory. */
	private StepBuilderFactory stepBuilderFactory;

	/**
	 * Sets the step builder factory.
	 *
	 * @param stepBuilderFactory the new step builder factory
	 */
	@Autowired
	public void setStepBuilderFactory(StepBuilderFactory stepBuilderFactory) {

		this.stepBuilderFactory = stepBuilderFactory;
	}

	/**
	 * Sets the job builder factory.
	 *
	 * @param jobBuilderFactory the new job builder factory
	 */
	@Autowired
	public void setJobBuilderFactory(JobBuilderFactory jobBuilderFactory) {

		this.jobBuilderFactory = jobBuilderFactory;
	}

	/**
	 * Perform.
	 *
	 * @throws Exception the exception
	 */
	// @Scheduled(cron = "${cron.expression.loans.load}") // => load value from .properties file
	//@Scheduled(cron = "#{@getLoansCronValue}") // => load value from DB ACM_ENVIRONNEMENT
	public void perform() throws Exception {

		// EXAMPLE : cron = "1 53/3 17 * * ?") : job to start at '5 PM 53 minutes 1 second' and run
		// for every 3 minutes till 6 PM
		// 0 0/5 * * * ? : -->Every 5 mins
		// 0 0/10 * * * ? : -->Every 10 mins
		logger.info("--- --- LOAN Job Started at : {} --- ---", new Date());
		JobParameters param = new JobParametersBuilder()
				.addString("LoanJobID", String.valueOf(System.currentTimeMillis()))
				.toJobParameters();
		JobExecution execution = simpleJobLauncher.run(importLoanJob(), param);
		logger.info("--- --- LOAN Job finished with status : {} --- ---", execution.getStatus());
	}

	/**
	 * Step 1.
	 *
	 * @return the step
	 */
	@Bean
	public Step stepLoanJob1() {

		return stepBuilderFactory.get("stepLoanJob1").<Loan, Loan>chunk(200).reader(readerLoan())
				.writer(writerLoan()).build();
	}

	/**
	 * Import loan job.
	 *
	 * @return the job
	 */
	@Bean
	public Job importLoanJob() {

		return jobBuilderFactory.get("importLoanJob").incrementer(new RunIdIncrementer())
				.flow(stepLoanJob1()).end().listener(listener).build();
	}

	/**
	 * Reader loan.
	 *
	 * @return the loan reader
	 */
	@Bean
	public LoanReader readerLoan() {

		return new LoanReader();
	}

	/**
	 * Writer.
	 *
	 * @return the loan writer
	 */
	@Bean
	public LoanWriter writerLoan() {

		return new LoanWriter();
	}
}
