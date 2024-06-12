/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_vneuron.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_vneuron.service.VneuronApiService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.vneuron.utils.dtos.CustomerStatusResponse;
import com.vneuron.utils.dtos.CustomerVneuron;
import com.vneuron.utils.dtos.RiskResponse;
import com.vneuron.utils.dtos.SearchPersonCustomerResponse;
import com.vneuron.utils.dtos.SearchWebhookResponse;
import com.vneuron.utils.dtos.UserVneuron;

/**
 * The Class VneuronKycController.
 */
@RestController
@RequestMapping("/vneuron")
public class VneuronKycController {

	/** The vneuron api service. */
	@Autowired
	VneuronApiService vneuronApiService;

	/**
	 * Gets the user token.
	 *
	 * @return the user token
	 */
	@GetMapping("/authenticate")
	public UserVneuron getUserToken() {

		return vneuronApiService.getUserToken();
	}

	/**
	 * Web hook match resolution.
	 *
	 * @param searchWebhookResponse the search webhook response
	 * @return the http status
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostMapping("/searchWebhook")
	public HttpStatus webHookMatchResolution(
			@RequestBody SearchWebhookResponse searchWebhookResponse)
			throws JsonProcessingException {

		vneuronApiService.searchWebHook(searchWebhookResponse);
		return HttpStatus.OK;
	}

	/**
	 * Web hook customer status.
	 *
	 * @param customerStatusResponse the customer status response
	 * @return the http status
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostMapping("/customerStatus")
	public CustomerStatusResponse webHookCustomerStatus(
			@RequestBody CustomerStatusResponse customerStatusResponse)
			throws JsonProcessingException {

		vneuronApiService.customerStatus(customerStatusResponse);

		return customerStatusResponse;
	}

	/**
	 * Gets the score custumer kyc.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param loanId the loan id
	 * @param customerId the customer id
	 * @return the score custumer kyc
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostMapping("/postAML/{loanId}/{customerId}")
	SearchPersonCustomerResponse postAML(@RequestBody CustomerVneuron customerVneuron,
			@PathVariable("loanId") Long loanId, @PathVariable("customerId") Long customerId)
			throws JsonProcessingException {

		return vneuronApiService.postAML(customerVneuron, loanId, customerId);

	}

	/**
	 * Calcul risk.
	 *
	 * @param customerVneuron the customer vneuron
	 * @param idLoan the id loan
	 * @param customerId the customer id
	 * @return the risk response
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostMapping("/calculRiskVneuron/{idLoan}/{customerId}")
	public RiskResponse calculRisk(@RequestBody CustomerVneuron customerVneuron,
			@PathVariable("idLoan") Long idLoan, @PathVariable("customerId") Long customerId)
			throws JsonProcessingException {

		return vneuronApiService.getScore(customerVneuron, idLoan, customerId);

	}

}
