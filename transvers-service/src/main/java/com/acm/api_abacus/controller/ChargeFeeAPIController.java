package com.acm.api_abacus.controller;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.api_abacus.service.ChargeFeeAPIService;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.ResponseChargeFeeDTO;

/**
 * The Class ChargeFeeAPIController.
 */
@RestController
@RequestMapping("/charge-fee")
public class ChargeFeeAPIController {

	/** The charge fee API service. */
	@Autowired
	private ChargeFeeAPIService chargeFeeAPIService;

	/**
	 * Find loan info.
	 *
	 * @param accountId the account id
	 * @return the response charge fee DTO
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws URISyntaxException the URI syntax exception
	 */
	@GetMapping("/charge-fee-info/{accountId}")
	public ResponseChargeFeeDTO initializeChargeFee(@PathVariable("accountId") Long accountId)
			throws ApiAbacusException, IOException, URISyntaxException {

		return chargeFeeAPIService.initializeChargeFee(accountId);

	}

	/**
	 * Charge fee.
	 *
	 * @param chargeFeeDTO the charge fee DTO
	 * @return the response entity
	 * @throws URISyntaxException the URI syntax exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@PostMapping("/post-charge-fee")
	public ResponseEntity<String> postChargeFees(@RequestBody ResponseChargeFeeDTO chargeFeeDTO)
			throws URISyntaxException, ApiAbacusException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException, IOException {

		return chargeFeeAPIService.postChargeFees(chargeFeeDTO);
	}
}
