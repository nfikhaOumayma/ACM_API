/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.batch;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;

import com.acm.client.CreditClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.CreditException;
import com.acm.service.AcmEnvironnementService;
import com.acm.utils.dtos.BranchChangeDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link BranchChangeWriter} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
public class BranchChangeWriter implements ItemWriter<BranchChangeDTO> {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(BranchChangeWriter.class);

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.batch.item.ItemWriter#write(java.util.List)
	 */
	@Override
	public void write(List<? extends BranchChangeDTO> items) throws Exception {

		// check if items are not null
		if (!ACMValidationUtils.isNullOrEmpty(items)) {
			// init customers to be updated in ACM
			List<CustomerDTO> customerDTOs = new ArrayList<>();
			String lastBranchChangedId = "0";
			// loop on items
			for (BranchChangeDTO item : items) {
				// check if branchId and customerIdExtern of item are not null
				if (!ACMValidationUtils.isNullOrEmpty(item.getBranchId())
						&& !ACMValidationUtils.isNullOrEmpty(item.getCustomerId())) {
					// init customerDTO
					CustomerDTO customerDTO = new CustomerDTO(item.getCustomerId());
					// set customer new brancheId
					customerDTO.setBranchId(item.getBranchId());
					// set customer new branch name
					customerDTO.setBranchesName(item.getBranchName());
					// set customer new branch description
					customerDTO.setBranchesDescription(item.getBranchDescription());
					// add customerDTO to the list to be updated in ACM
					customerDTOs.add(customerDTO);
					lastBranchChangedId = item.getId().toString();
				}
			}
			if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {

				try {
					// update customerDTOs in ACM
					creditClient.updateCustomersBranches(customerDTOs);
					// update loanDTOs in ACM
					creditClient.updateLoanBranches(customerDTOs);
				}
				catch (Exception e) {
					throw new CreditException(
							new ExceptionResponseMessage(
									CommonErrorCode.ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES,
									CommonExceptionsMessage.ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES,
									new TechnicalException()),
							CommonExceptionsMessage.ERROR_WHILE_UPDATE_CUSTOMERS_BRANCHES);
				}
			}
			if (!ACMValidationUtils.isNullOrEmpty(lastBranchChangedId)) {
				acmEnvironnementService.updateLimite(
						CommonConstants.LAST_BRANCH_CHANGED_ID_SYNCHRONIZED, lastBranchChangedId);
			}
		}
	}
}
