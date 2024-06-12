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

import com.acm.service.batch.JobCompletionListener;
import com.acm.service.batch.TransferPortfolioProcessor;
import com.acm.service.batch.TransferPortfolioReader;
import com.acm.service.batch.TransferPortfolioWriter;
import com.acm.utils.dtos.CUAccountPortfolioTransferredDTO;

/**
 * The Class BatchTransferPortfolioConfiguration.
 */
@Configuration
public class BatchTransferPortfolioConfiguration {
	/** The job builder factory. */
	@Autowired
	public JobBuilderFactory jobBuilderFactory;

	/** The step builder factory. */
	@Autowired
	public StepBuilderFactory stepBuilderFactory;

	/**
	 * Process transfer portfolio job.
	 *
	 * @return the job
	 */
	@Bean
	public Job processTransferPortfolioJob() {

		return jobBuilderFactory.get("processTransferPortfolioJob")
				.incrementer(new RunIdIncrementer()).listener(listener())
				.flow(TransferPortfolioStep()).end().build();
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

	/**
	 * Transfer portfolio step.
	 *
	 * @return the step
	 */
	@Bean
	public Step TransferPortfolioStep() {

		return stepBuilderFactory.get("TransferPortfolioStep")
				.<CUAccountPortfolioTransferredDTO, CUAccountPortfolioTransferredDTO>chunk(400)
				.reader(TransferPortfolioReader()).writer(TransferPortfolioWriter()).build();
	}

	/**
	 * Transfer portfolio reader.
	 *
	 * @return the transfer portfolio reader
	 */
	@Bean
	public TransferPortfolioReader TransferPortfolioReader() {

		return new TransferPortfolioReader();
	}

	/**
	 * Transfer portfolio writer.
	 *
	 * @return the transfer portfolio writer
	 */
	@Bean
	public TransferPortfolioWriter TransferPortfolioWriter() {

		return new TransferPortfolioWriter();
	}

	/**
	 * Transfer portfolio processor.
	 *
	 * @return the transfer portfolio processor
	 */
	@Bean
	public TransferPortfolioProcessor TransferPortfolioProcessor() {

		return new TransferPortfolioProcessor();
	}
}
