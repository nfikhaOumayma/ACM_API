/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.listener.JobExecutionListenerSupport;

/**
 * {@link JobCompletionListener} class : The listener interface for receiving jobCompletion events.
 * The class that is interested in processing a jobCompletion event implements this interface, and
 * the object created with that class is registered with a component using the component's
 * "addJobCompletionListener" method. When the jobCompletion event occurs, that object's appropriate
 * method is invoked => see JobCompletionEvent.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
public class JobCompletionListener extends JobExecutionListenerSupport {

	/** logger. */
	private static final Logger logger = LoggerFactory.getLogger(JobCompletionListener.class);

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.core.listener.JobExecutionListenerSupport#beforeJob(org.
	 * springframework.batch.core.JobExecution)
	 */
	@Override
	public void beforeJob(JobExecution jobExecution) {

		if (jobExecution.getStatus() == BatchStatus.STARTED) {
			logger.info(" ### BATCH JOB STARTED ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.STARTING) {
			logger.info(" ### BATCH JOB STARTING ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.UNKNOWN) {
			logger.warn(" ### BATCH JOB with UNKNOWN status ### {}  ###", jobExecution);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.core.listener.JobExecutionListenerSupport#afterJob(org.
	 * springframework.batch.core.JobExecution)
	 */
	@Override
	public void afterJob(JobExecution jobExecution) {

		if (jobExecution.getStatus() == BatchStatus.COMPLETED) {
			logger.info(" ### BATCH JOB COMPLETED SUCCESSFULLY ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.ABANDONED) {
			logger.warn(" ### BATCH JOB ABANDONED ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.FAILED) {
			logger.error(" ### BATCH JOB ABANDONED ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.STOPPED) {
			logger.warn(" ### BATCH JOB STOPPED ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.STOPPING) {
			logger.warn(" ### BATCH JOB STOPPING ### {}  ###", jobExecution);
		}
		else if (jobExecution.getStatus() == BatchStatus.UNKNOWN) {
			logger.warn(" ### BATCH JOB with UNKNOWN status ### {}  ###", jobExecution);
		}
	}
}
