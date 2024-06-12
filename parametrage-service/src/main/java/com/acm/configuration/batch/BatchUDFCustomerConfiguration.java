/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.batch;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.acm.service.batch.UDFCustomerReader;
import com.acm.service.batch.UDFCustomerWriter;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * {@link BatchUDFCustomerConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
@Configuration
public class BatchUDFCustomerConfiguration {

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
	 * Step 1.
	 *
	 * @return the step
	 */
	@Bean
	public Step stepUDFCustomerJob1() {

		return stepBuilderFactory.get("stepUDFCustomerJob1")
				.<UserDefinedFieldsLinksDTO, UserDefinedFieldsLinksDTO>chunk(400)
				.reader(readerUDFCustomer()).writer(writerUDFCustomer()).build();
	}

	/**
	 * Import customer job.
	 *
	 * @return the job
	 */
	@Bean
	public Job importUDFCustomerJob() {

		return jobBuilderFactory.get("importUDFCustomerJob").incrementer(new RunIdIncrementer())
				.flow(stepUDFCustomerJob1()).end().build();
	}

	/**
	 * Reader customer.
	 *
	 * @return the customer reader
	 */
	@Bean
	public UDFCustomerReader readerUDFCustomer() {

		return new UDFCustomerReader();
	}

	/**
	 * Writer.
	 *
	 * @return the customer writer
	 */
	@Bean
	public UDFCustomerWriter writerUDFCustomer() {

		return new UDFCustomerWriter();
	}
}
