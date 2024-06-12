/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.NonTransientResourceException;
import org.springframework.batch.item.ParseException;
import org.springframework.batch.item.UnexpectedInputException;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.TransversClient;
import com.acm.constants.common.CommonConstants;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.BranchChangeDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link BranchChangeReader} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class BranchChangeReader implements ItemReader<BranchChangeDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(BranchChangeReader.class);

	/** The count. */
	private int count = 0;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The branchchange DT os. */
	private List<BranchChangeDTO> branchchangeDTOs;

	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemReader#read()
	 */
	@Override
	public BranchChangeDTO read() throws Exception, UnexpectedInputException, ParseException,
			NonTransientResourceException {

		if (branchchangeDTOs == null) {
			AcmEnvironnementDTO environnementDTO = acmEnvironnementService
					.find(CommonConstants.LAST_BRANCH_CHANGED_ID_SYNCHRONIZED);
			if (ACMValidationUtils.isNullOrEmpty(environnementDTO)) {
				logger.error(" ################# acm_environment object not found with key  = {} "
						+ CommonConstants.LAST_BRANCH_CHANGED_ID_SYNCHRONIZED
						+ "#################");
				return null;
			}
			else {
				branchchangeDTOs =
						transversClient.findBranchChangeForBatch(environnementDTO.getValue());
			}

		}

		if (count < branchchangeDTOs.size()) {
			BranchChangeDTO branchChangeDTO = branchchangeDTOs.get(count);
			count++;
			return branchChangeDTO;
		}
		else {
			count = 0;
			branchchangeDTOs = null;
		}
		return null;
	}
}
