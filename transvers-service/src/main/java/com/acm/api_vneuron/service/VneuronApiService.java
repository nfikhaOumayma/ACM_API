/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_vneuron.service;

import com.acm.utils.dtos.LoanDTO;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.vneuron.utils.dtos.CustomerStatusResponse;
import com.vneuron.utils.dtos.CustomerVneuron;
import com.vneuron.utils.dtos.RiskResponse;
import com.vneuron.utils.dtos.SearchPersonCustomerResponse;
import com.vneuron.utils.dtos.SearchWebhookResponse;
import com.vneuron.utils.dtos.UserVneuron;

/**
 * The Interface VneuronApiService.
 */
public interface VneuronApiService {

	/**
	 * Gets the user token.
	 *
	 * @return the user token
	 */
	UserVneuron getUserToken();

	/**
	 * Client staus.
	 *
	 * @author kouali
	 * @param customerStatusResponse the customer status response
	 * @throws JsonProcessingException the json processing exception
	 */
	void customerStatus(CustomerStatusResponse customerStatusResponse)
			throws JsonProcessingException;

	/**
	 * Search web hook.
	 *
	 * @author kouali
	 * @param searchWebhookResponse the search webhook response
	 * @return the risk response
	 * @throws JsonProcessingException the json processing exception
	 */
	LoanDTO searchWebHook(SearchWebhookResponse searchWebhookResponse)
			throws JsonProcessingException;

	/**
	 * Gets the score custumer kyc.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @return the score custumer kyc
	 * @throws JsonProcessingException the json processing exception
	 */
	SearchPersonCustomerResponse postAML(CustomerVneuron customerVneuron, Long loanId,
			Long customerId) throws JsonProcessingException;

	/**
	 * Gets the score.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @return the score
	 * @throws JsonProcessingException the json processing exception
	 */
	RiskResponse getScore(CustomerVneuron customerVneuron, Long idLoan, Long customerId)
			throws JsonProcessingException;

	/**
	 * Gets the status.
	 *
	 * @param customerId the customer id
	 * @param idLoan the id loan
	 * @return the status
	 */
	String getStatus(Long customerId, Long idLoan);

}
