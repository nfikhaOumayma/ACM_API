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

import com.acm.service.batch.UserReader;
import com.acm.service.batch.UserWriter;
import com.acm.utils.models.User;

/**
 * {@link BatchUserConfiguration} class.
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
// @Configuration
// @Import({BatchScheduler.class})
public class BatchUserConfiguration {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(BatchUserConfiguration.class);

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
	//@Scheduled(cron = "${cron.expression.users.load}") // => load value from .properties file
	// @Scheduled(cron = "#{@getUsersCronValue}") // => load value from DB ACM_ENVIRONNEMENT
	public void perform() throws Exception {

		logger.info("--- --- USER Job Started at : {} --- ---", new Date());
		JobParameters param = new JobParametersBuilder()
				.addString("UserJobID", String.valueOf(System.currentTimeMillis()))
				.toJobParameters();
		JobExecution execution = simpleJobLauncher.run(importUserJob(), param);
		logger.info("--- --- USER Job finished with status : {} --- ---", execution.getStatus());
	}

	/**
	 * Step 1.
	 *
	 * @return the step
	 */
	@Bean
	public Step step1() {

		return stepBuilderFactory.get("stepUserJob1").<User, User>chunk(200).reader(readerUser())
				.writer(writerUser()).build();
	}

	/**
	 * Import user job.
	 *
	 * @return the job
	 */
	@Bean
	public Job importUserJob() {

		return jobBuilderFactory.get("importUserJob").incrementer(new RunIdIncrementer())
				.flow(step1()).end().build();
	}

	/**
	 * Reader user.
	 *
	 * @return the user reader
	 */
	@Bean
	public UserReader readerUser() {

		return new UserReader();
	}

	/**
	 * Writer.
	 *
	 * @return the user writer
	 */
	@Bean
	public UserWriter writerUser() {

		return new UserWriter();
	}
}
