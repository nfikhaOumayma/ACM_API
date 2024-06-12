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

import com.acm.service.batch.BranchChangeReader;
import com.acm.service.batch.BranchChangeWriter;
import com.acm.service.batch.JobCompletionListener;
import com.acm.utils.dtos.BranchChangeDTO;

/**
 * {@link BatchBranchChangeConfiguration} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Configuration
public class BatchBranchChangeConfiguration {
	/** The job builder factory. */
	@Autowired
	public JobBuilderFactory jobBuilderFactory;

	/** The step builder factory. */
	@Autowired
	public StepBuilderFactory stepBuilderFactory;

	/**
	 * Process branch change job.
	 * 
	 * @author mlamloum
	 * @return the job
	 */
	@Bean
	public Job processBranchChangeJob() {

		return jobBuilderFactory.get("processBranchChangeJob").incrementer(new RunIdIncrementer())
				.listener(listener()).flow(branchChangeStep()).end().build();
	}

	/**
	 * Branch change step.
	 * 
	 * @author mlamloum
	 * @return the step
	 */
	@Bean
	public Step branchChangeStep() {

		return stepBuilderFactory.get("branchChangeStep")
				.<BranchChangeDTO, BranchChangeDTO>chunk(400).reader(branchChangeReader())
				.writer(branchChangeWriter()).build();
	}

	/**
	 * Branch change reader.
	 * 
	 * @author mlamloum
	 * @return the branch change reader
	 */
	@Bean
	public BranchChangeReader branchChangeReader() {

		return new BranchChangeReader();
	}

	/**
	 * Branch change writer.
	 * 
	 * @author mlamloum
	 * @return the branch change writer
	 */
	@Bean
	public BranchChangeWriter branchChangeWriter() {

		return new BranchChangeWriter();
	}

	/**
	 * Listener.
	 * 
	 * @author mlamloum
	 * @return the job execution listener
	 */
	@Bean
	public JobExecutionListener listener() {

		return new JobCompletionListener();
	}
}
