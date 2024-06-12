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

import com.acm.service.batch.CloseCollectionTask;
import com.acm.service.batch.CollectionProcessor;
import com.acm.service.batch.CollectionReader;
import com.acm.service.batch.CollectionWriter;
import com.acm.service.batch.JobCompletionListener;
import com.acm.service.batch.LegalCollectionProcessor;
import com.acm.service.batch.LegalCollectionReader;
import com.acm.service.batch.LegalCollectionWriter;
import com.acm.utils.dtos.AcmCollectionDTO;
import com.acm.utils.models.AcmCollection;

/**
 * The Class BatchCollectionConfiguration.
 */
@Configuration
public class BatchCollectionConfiguration {

	/** The job builder factory. */
	@Autowired
	public JobBuilderFactory jobBuilderFactory;

	/** The step builder factory. */
	@Autowired
	public StepBuilderFactory stepBuilderFactory;

	/**
	 * Process job.
	 *
	 * @return the job
	 */
	@Bean
	public Job collectionProccess() {

		return jobBuilderFactory.get("collectionProccess").incrementer(new RunIdIncrementer())
				.listener(listener()).flow(collectionStep()).next(closeCollectionStep())
				.next(legalCollectionStep()).end().build();
	}

	/**
	 * Collection step.
	 *
	 * @return the step
	 */
	@Bean
	public Step collectionStep() {

		return stepBuilderFactory.get("collectionStep").<AcmCollectionDTO, AcmCollection>chunk(400)
				.reader(collectionReader()).processor(collectionProcessor())
				.writer(collectionWriter()).build();
	}

	/**
	 * Collection reader.
	 *
	 * @return the collection reader
	 */
	@Bean
	public CollectionReader collectionReader() {

		CollectionReader reader = new CollectionReader();
		reader.reset();
		return reader;
	}

	/**
	 * Collection writer.
	 *
	 * @return the Collection writer
	 */
	@Bean
	public CollectionWriter collectionWriter() {

		return new CollectionWriter();
	}

	/**
	 * Collection processor.
	 *
	 * @return the Collection processor
	 */
	@Bean
	public CollectionProcessor collectionProcessor() {

		return new CollectionProcessor();
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
	 * Close collection step.
	 *
	 * @return the step
	 */
	@Bean
	public Step closeCollectionStep() {

		return stepBuilderFactory.get("closeCollectionStep").tasklet(closeCollectionTask()).build();
	}

	/**
	 * Close collection task.
	 *
	 * @return the close collection task
	 */
	@Bean
	public CloseCollectionTask closeCollectionTask() {

		return new CloseCollectionTask();
	}

	/**
	 * Legal Collection step.
	 *
	 * @return the step
	 */
	@Bean
	public Step legalCollectionStep() {

		return stepBuilderFactory.get("legalCollectionStep")
				.<AcmCollectionDTO, AcmCollection>chunk(400).reader(legalCollectionReader())
				.processor(legalCollectionProcessor()).writer(legalCollectionWriter()).build();
	}

	/**
	 * Legal Collection reader.
	 *
	 * @return the Legal collection reader
	 */
	@Bean
	public LegalCollectionReader legalCollectionReader() {

		return new LegalCollectionReader();
	}

	/**
	 * Legal Collection writer.
	 *
	 * @return the Legal Collection writer
	 */
	@Bean
	public LegalCollectionWriter legalCollectionWriter() {

		return new LegalCollectionWriter();
	}

	/**
	 * Legal Collection processor.
	 *
	 * @return the Legal Collection processor
	 */
	@Bean
	public LegalCollectionProcessor legalCollectionProcessor() {

		return new LegalCollectionProcessor();
	}
}
