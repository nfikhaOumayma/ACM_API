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
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.launch.support.SimpleJobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;

import com.acm.service.batch.AddressCustomerItemProcessor;
import com.acm.service.batch.AddressCustomerReader;
import com.acm.service.batch.AddressCustomerWriter;
import com.acm.utils.dtos.AddressDTO;

/**
 * {@link BatchAddressCustomerConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
// @Configuration
// @Import({BatchScheduler.class})
public class BatchAddressCustomerConfiguration {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(BatchAddressCustomerConfiguration.class);

	/** The job launcher. */
	@Autowired
	private SimpleJobLauncher simpleJobLauncher;

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
	// @Scheduled(cron = "${cron.expression.address.load}") // => load value from .properties file
	//@Scheduled(cron = "#{@getAddressCustomerCronValue}") // => load value from DB ACM_ENVIRONNEMENT
	public void perform() throws Exception {

		// EXAMPLE : cron = "1 53/3 17 * * ?") : job to start at '5 PM 53 minutes 1 second' and run
		// for every 3 minutes till 6 PM
		// 0 0/5 * * * ? : -->Every 5 mins
		// 0 0/10 * * * ? : -->Every 10 mins
		logger.info("--- --- AddressCUSTOMER Job Started at : {} --- ---", new Date());
		JobParameters param = new JobParametersBuilder()
				.addString("AddressCustomerJobID", String.valueOf(System.currentTimeMillis()))
				.toJobParameters();
		JobExecution execution = simpleJobLauncher.run(importAddressCustomerJob(), param);
		logger.info("--- --- AddressCUSTOMER Job finished with status : {} --- ---",
				execution.getStatus());
	}

	/**
	 * Step address customer job 1.
	 *
	 * @return the step
	 */
	@Bean
	public Step stepAddressCustomerJob1() {

		return stepBuilderFactory.get("stepAddressCustomerJob1").<AddressDTO, AddressDTO>chunk(400)
				.reader(readerAddressCustomer()).processor(processorAddressCustomer())
				.writer(writerAddressCustomer()).build();
	}

	/**
	 * Import address customer job.
	 *
	 * @return the job
	 */
	@Bean
	public Job importAddressCustomerJob() {

		return jobBuilderFactory.get("importAddressCustomerJob").incrementer(new RunIdIncrementer())
				.flow(stepAddressCustomerJob1()).end().build();
	}

	/**
	 * Reader address customer.
	 *
	 * @return the address customer reader
	 */
	@Bean
	public AddressCustomerReader readerAddressCustomer() {

		return new AddressCustomerReader();
	}

	/**
	 * Processor address customer.
	 *
	 * @return the address customer item processor
	 */
	@Bean
	public AddressCustomerItemProcessor processorAddressCustomer() {

		return new AddressCustomerItemProcessor();
	}

	/**
	 * Writer address customer.
	 *
	 * @return the address customer writer
	 */
	@Bean
	public AddressCustomerWriter writerAddressCustomer() {

		return new AddressCustomerWriter();
	}
}
