/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.batch;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.acm.service.batch.CustomerProcessor;
import com.acm.service.batch.CustomerReader;
import com.acm.service.batch.CustomerWriter;
import com.acm.service.batch.JobCompletionListener;
import com.acm.utils.dtos.CustomerDTO;

/**
 * The Class BatchCustomerConfiguration.
 */
@Configuration
public class BatchCustomerConfiguration {

	/** The job builder factory. */
	@Autowired
	public JobBuilderFactory jobBuilderFactory;

	/** The step builder factory. */
	@Autowired
	public StepBuilderFactory stepBuilderFactory;

	/**
	 * Process customer job.
	 *
	 * @return the job
	 */
	@Bean
	public Job processCustomerJob() {

		return jobBuilderFactory.get("processCustomerJob").incrementer(new RunIdIncrementer())
				.listener(listener()).flow(customerStep()).end().build();
	}

	/**
	 * Customer step.
	 *
	 * @return the step
	 */
	@Bean
	public Step customerStep() {

		return stepBuilderFactory.get("customerStep").<CustomerDTO, CustomerDTO>chunk(400)
				.reader(customerReader()).processor(customerProcessor()).writer(customerWriter())
				.build();
	}

	/**
	 * Customer reader.
	 *
	 * @return the customer reader
	 */
	@Bean
	public CustomerReader customerReader() {

		return new CustomerReader();
	}

	/**
	 * Customer writer.
	 *
	 * @return the customer writer
	 */
	@Bean
	public CustomerWriter customerWriter() {

		return new CustomerWriter();
	}

	/**
	 * Customer processor.
	 *
	 * @return the customer processor
	 */
	@Bean
	public CustomerProcessor customerProcessor() {

		return new CustomerProcessor();
	}

	/**
	 * Listener.
	 *
	 * @return the job execution listener
	 */
	@Bean
	public JobExecutionListener listener() {

		return new JobCompletionListener();
	}

}
