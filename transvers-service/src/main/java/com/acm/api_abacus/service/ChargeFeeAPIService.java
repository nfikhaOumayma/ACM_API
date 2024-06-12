package com.acm.api_abacus.service;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

import org.springframework.http.ResponseEntity;

import com.acm.exceptions.type.ApiAbacusException;
import com.acm.utils.dtos.ResponseChargeFeeDTO;

/**
 * The Interface ChargeFeeAPIService.
 */
public interface ChargeFeeAPIService {
	/**
	 * Gets the loan info.
	 *
	 * @param accountId the account id
	 * @return the loan info
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 */
	ResponseChargeFeeDTO initializeChargeFee(Long accountId)
			throws IOException, ApiAbacusException, URISyntaxException;

	/**
	 * Charge fee.
	 *
	 * @param chargeFeeDTO the charge fee DTO
	 * @return the response entity
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws URISyntaxException the URI syntax exception
	 * @throws KeyManagementException the key management exception
	 * @throws KeyStoreException the key store exception
	 * @throws NoSuchAlgorithmException the no such algorithm exception
	 */
	ResponseEntity<String> postChargeFees(ResponseChargeFeeDTO chargeFeeDTO)
			throws IOException, ApiAbacusException, URISyntaxException, KeyManagementException,
			KeyStoreException, NoSuchAlgorithmException;
}
