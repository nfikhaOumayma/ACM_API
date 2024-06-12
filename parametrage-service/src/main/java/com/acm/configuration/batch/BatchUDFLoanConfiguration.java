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

import com.acm.service.batch.UDFLoanReader;
import com.acm.service.batch.UDFLoanWriter;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;

/**
 * {@link BatchUDFLoanConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
@Configuration
public class BatchUDFLoanConfiguration {

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
	public Step stepUDFLoanJob1() {

		return stepBuilderFactory.get("stepUDFLoanJob1")
				.<UserDefinedFieldsLinksDTO, UserDefinedFieldsLinksDTO>chunk(400)
				.reader(readerUDFLoan()).writer(writerUDFLoan()).build();
	}

	/**
	 * Import loan job.
	 *
	 * @return the job
	 */
	@Bean
	public Job importUDFLoanJob() {

		return jobBuilderFactory.get("importUDFLoanJob").incrementer(new RunIdIncrementer())
				.flow(stepUDFLoanJob1()).end().build();
	}

	/**
	 * Reader loan.
	 *
	 * @return the loan reader
	 */
	@Bean
	public UDFLoanReader readerUDFLoan() {

		return new UDFLoanReader();
	}

	/**
	 * Writer.
	 *
	 * @return the loan writer
	 */
	@Bean
	public UDFLoanWriter writerUDFLoan() {

		return new UDFLoanWriter();
	}
}
