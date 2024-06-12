/*
 * 
 */
package com.acm.service.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.service.AcmCollectionService;

/**
 * The Class CloseCollectionTask.
 */
public class CloseCollectionTask implements Tasklet {

	
	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CloseCollectionTask.class);
	
	/** The acm collection service. */
	@Autowired
	private AcmCollectionService acmCollectionService;
	
	/* (non-Javadoc)
	 * @see org.springframework.batch.core.step.tasklet.Tasklet#execute(org.springframework.batch.core.StepContribution, org.springframework.batch.core.scope.context.ChunkContext)
	 */
	@Override
	public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext)
			throws Exception {

		int closedColl = acmCollectionService.closeCollections();
		acmCollectionService.closeCollectionTasks();
		logger.info("DONE : Close unfound Collections batch :: {} Collections", closedColl);
		return RepeatStatus.FINISHED;
	}

}
